namespace Kaleidochron

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Media

type LinearClockControl() =
   inherit Control()

   let mutable animating = false

   static let clockProperty = AvaloniaProperty.Register<LinearClockControl, Clock>("Clock", Clock.Default)

   static let tuningProperty = AvaloniaProperty.Register<LinearClockControl, ClockTuning>("Tuning", ClockTuning.Default)

   static let hoursLoggedProperty = AvaloniaProperty.Register<LinearClockControl, TimeSpan>("HoursLogged", TimeSpan.Zero)

   static member ClockProperty = clockProperty

   static member TuningProperty = tuningProperty

   static member HoursLoggedProperty = hoursLoggedProperty

   member this.Clock
      with get () = this.GetValue clockProperty
      and set (v : Clock) = this.SetValue(clockProperty, v) |> ignore

   member this.Tuning
      with get () = this.GetValue tuningProperty
      and set (v : ClockTuning) = this.SetValue(tuningProperty, v) |> ignore

   member this.HoursLogged
      with get () = this.GetValue hoursLoggedProperty
      and set (v : TimeSpan) = this.SetValue(hoursLoggedProperty, v) |> ignore

   override this.OnAttachedToVisualTree e =
      base.OnAttachedToVisualTree e

      if not animating then
         animating <- true

         match TopLevel.GetTopLevel(this) with
         | null -> ()
         | topLevel ->
            let rec loop (_ : TimeSpan) =
               if animating then
                  this.InvalidateVisual()
                  topLevel.RequestAnimationFrame(Action<TimeSpan> loop)

            topLevel.RequestAnimationFrame(Action<TimeSpan> loop)

   override this.OnDetachedFromVisualTree e =
      animating <- false
      base.OnDetachedFromVisualTree e

   override this.Render(ctx : DrawingContext) =
      base.Render ctx

      let tuning = this.Tuning
      let bounds = this.Bounds

      if bounds.Width > 0.0 && tuning.HourCount >= 2 then
         let timeOfDay = this.Clock.GetTimeOfDay() + this.Tuning.RealTimeOffset

         let windowStart = tuning.WindowStart timeOfDay

         let minutesSinceWindowStart =
            let day = 24.0 * 60.0
            (timeOfDay - windowStart).TotalMinutes %% day
            |> min (tuning.Span.TotalMinutes - 1e-4)

         let hoursSinceWindowStart = int (minutesSinceWindowStart / 60.0)
         let phase = ClockMath.phase tuning hoursSinceWindowStart (TimeSpan.FromMinutes(minutesSinceWindowStart % 60.0))

         let width = bounds.Width
         let barHeight = tuning.BarHeight
         let yTop = 0.0
         let yMid = yTop + (barHeight - yTop) / 2.0

         let layoutCurrentHour = ClockMath.layoutFor tuning width hoursSinceWindowStart

         let layoutPrevHour =
            if hoursSinceWindowStart > 0 then
               Some(ClockMath.layoutFor tuning width (hoursSinceWindowStart - 1))
            else
               None

         let map, markerX, oldHot =
            match phase, layoutPrevHour with
            | FirstHour, _
            | _, None -> layoutCurrentHour.Map, layoutCurrentHour.Edge, 0.0
            | Flash p, Some a -> a.Map, a.Edge, p
            | Slide e, Some a -> (fun m -> ClockMath.lerp (a.Map m) (layoutCurrentHour.Map m) e), ClockMath.lerp a.Edge layoutCurrentHour.Edge e, 0.25 * (1.0 - e)
            | Filling glow, Some _ -> layoutCurrentHour.Map, layoutCurrentHour.Edge, glow

         // ---- draw helpers ----
         let fillRect (brush : IBrush) x y wd ht =
            if wd > 0.0 && ht > 0.0 then
               ctx.FillRectangle(brush, Rect(x, y, wd, ht))

         let label (text : string) x y (color : Color) alpha =
            let ft =
               FormattedText(text, CultureInfo.InvariantCulture, FlowDirection.LeftToRight, Palette.typeface, 9.0, Palette.solidA color alpha)

            ctx.DrawText(ft, Point(x, y))

         // ---- hour cells ----
         for j in 0 .. tuning.HourCount - 1 do
            let m0 = float j * 60.0
            let m1 = m0 + 60.0
            let x0 = map m0 + 1.0
            let x1 = map m1 - 1.0

            let baseColor =
               if j < hoursSinceWindowStart then
                  Palette.cell
               elif j = hoursSinceWindowStart then
                  Palette.lerpColor Palette.cellDim Palette.cell 0.07 // staged
               else
                  Palette.cellDim

            let cellColor =
               if j = hoursSinceWindowStart - 1 && oldHot > 0.0 then
                  Palette.lerpColor baseColor Palette.cellHot oldHot
               else
                  baseColor

            fillRect (Palette.solid cellColor) x0 yTop (x1 - x0) barHeight

            let pulse, pulseAlpha =
               let interval = 5.0

               let fillDuration = 1.5
               let flashDuration = 0.25
               let fadeDuration = 2.25

               let fillStart, fillEnd = 0.0, fillDuration
               let flashStart, flashEnd = fillEnd, fillEnd + flashDuration
               let fadeStart, fadeEnd = flashEnd, flashEnd + fadeDuration

               if fadeEnd > interval then
                  failwith "Fade duration exceeds pulse interval"

               let t = timeOfDay.TotalSeconds % interval

               Tween.Exponential.easeIn (t / fillDuration),
               match t with
               | t when t >= fillStart && t < fillEnd -> 0.5
               | t when t >= flashStart && t < flashEnd -> 1.0
               | t when t >= fadeStart && t < fadeEnd -> let s = (t - fadeStart) / fadeDuration in 1.0 - Tween.Exponential.easeOut s
               | _ -> 0.0

            // current-hour fill + leading edge
            if j = hoursSinceWindowStart then
               let xf = map (min minutesSinceWindowStart m1)

               if xf > x0 then

                  fillRect (Palette.solid Palette.currentCell) x0 yTop (xf - x0) barHeight
                  fillRect (Palette.solid Palette.currentCellDim) xf yTop (x1 - xf) barHeight
                  let widthStart = 0.0
                  let widthEnd = xf - x0
                  let pulseWidth = widthStart * (1.0 - pulse) + widthEnd * pulse
                  fillRect (Palette.solidA Palette.currentCellHot pulseAlpha) x0 yTop pulseWidth barHeight

                  if pulse < 1.0 then
                     let w = min pulseWidth 1.0
                     fillRect (Palette.solid Palette.marker) (x0 + pulseWidth - w) yTop w barHeight

                  // fillRect (Palette.gradientA [| (0.0, Palette.currentCellHot); (0.0, Palette.currentCellHot); (1.0, Palette.currentCell) |] pulse) (xf - pulseWidth) yTop pulseWidth barHeight

               if oldHot > 0.0 then
                  fillRect (Palette.solid cellColor) x0 yTop (x1 - x0) barHeight

            // structural quarter kerfs + labels: any hour wide enough,
            // so the completed hour keeps its quarters through flash & collapse
            if x1 - x0 > 120.0 then
               for q in [ 15.0; 30.0; 45.0 ] do
                  let xq = map (m0 + q)
                  fillRect (Palette.solid Palette.kerf) (xq - 1.0) yTop 2.0 barHeight
                  // label $":%02d{int q}" (xq + 3.0) (yTop + barH + 4.0) Palette.labelC 0.75
                  label $":%02d{int q}" (xq + 3.0) (yTop + barHeight) Palette.labelC 1.0

            // commit edge highlight on the landed hour
            match phase with
            | Filling glow when j = hoursSinceWindowStart - 1 && glow > 0.0 ->
               let br = Palette.solidA Palette.currentCellHot glow
               fillRect br x0 (yTop - 1.0) (x1 - x0) 1.0
               fillRect br x0 (yTop + barHeight) (x1 - x0) 1.0
            | _ -> ()

         // ---- gated fine ticks ----
         for tickLevel in ClockMath.tickLevels do
            // sub-5-minute levels can only clear their gate near the expanded
            // hour(s); bound the scan to hours h−1..h during transitions
            let lo, hi =
               if tickLevel.Step < 5.0 then
                  max 0.0 (float (hoursSinceWindowStart - 1) * 60.0 - tickLevel.Step),
                  min tuning.Span.TotalMinutes (float hoursSinceWindowStart * 60.0 + 60.0 + tickLevel.Step)
               else
                  0.0,
                  tuning.Span.TotalMinutes

            let mutable m = ceil (lo / tickLevel.Step) * tickLevel.Step

            while m < hi do
               if m % tickLevel.Coarser <> 0.0 &&
                  m > 0.0 &&
                  m < tuning.Span.TotalMinutes then
                  let spacing = (map (m + tickLevel.Step) - map (m - tickLevel.Step)) / 2.0
                  let a = Math.Clamp((spacing - tickLevel.Gate) / tickLevel.Gate, 0.0, 1.0)

                  if a > 0.02 then
                     let ht = barHeight * tickLevel.HeightFrac
                     fillRect (Palette.solidA Palette.kerf (0.6 * a)) (map m - tickLevel.Width / 2.0) (yMid - ht / 2.0) tickLevel.Width ht

               m <- m + tickLevel.Step

         // ---- hour labels ----
         for hourIndex in 0 .. tuning.HourCount do
            let x = map (float hourIndex * 60.0)
            // label (string (tuning.DayStart.Hours + j)) (x + 3.0) (yTop + barH + 0.0) Palette.labelC 0.9
            label (string ((((windowStart.Hours + hourIndex) - 1) % 12) + 1)) (x + 3.0) (yTop + barHeight) Palette.labelC 0.75

         // ---- now line ----
         fillRect (Palette.solid Palette.marker) (markerX + 1.0) yTop 1.0 barHeight

         // ---- hours-logged gauge ----
         // Shares `map` with the bar above, so its left edge rides the Origin
         // hour boundary through commit animations, and its fill edge can be
         // compared against the now line: logged = elapsed-since-Origin lands
         // the fill edge exactly on the marker.
         let g = tuning.Gauge

         if g.Enabled && g.Height > 0.0 then
            let originMin = (g.Origin - windowStart).TotalMinutes

            // Origin outside the active window (e.g. overnight): gauge hidden
            if originMin >= 0.0 && originMin < tuning.Span.TotalMinutes then
               let filledMin = Math.Clamp(originMin + this.HoursLogged.TotalMinutes, originMin, tuning.Span.TotalMinutes)
               let yg = yTop + barHeight + g.Offset
               let x0 = map originMin
               let xf = map filledMin

               fillRect (Palette.solid Palette.gaugeTrack) x0 yg (map tuning.Span.TotalMinutes - x0) g.Height
               fillRect (Palette.solid Palette.gaugeFill) x0 yg (xf - x0) g.Height
