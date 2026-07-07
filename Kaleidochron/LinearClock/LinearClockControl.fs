namespace Kaleidochron

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Media.Immutable

type Clock =
   {
      GetTimeOfDay : unit -> TimeSpan
   }

   static member Default = { GetTimeOfDay = fun () -> DateTime.Now.TimeOfDay }

type ClockTuning =
   {
      DayStart : TimeSpan
      DayEnd : TimeSpan
      HourWidthMultiple : float
      FlashDuration : TimeSpan
      SlideDuration : TimeSpan
      CoolDuration : TimeSpan
      BarHeight : float
      RealTime : bool
   }

   member this.SpanMinutes = (this.DayEnd - this.DayStart).TotalMinutes

   member this.HourCount = int (this.SpanMinutes / 60.0)
   member this.HourCount2 = (this.DayEnd - this.DayStart).Hours

   static member Default =
      {
         DayStart = TimeSpan.FromHours 9.0
         DayEnd = TimeSpan.FromHours 20.0
         HourWidthMultiple = 8.0
         FlashDuration = TimeSpan.FromMilliseconds 400.0
         SlideDuration = TimeSpan.FromMilliseconds 1200.0
         CoolDuration = TimeSpan.FromMilliseconds 2500.0
         BarHeight = 16.0
         RealTime = false
      }

/// Stage of the hour-commit choreography. Exhaustive by construction;
/// derived purely from time-since-hour — replay is free.
type Phase =
   | FirstHour // no predecessor to animate
   | Flash of pulse : float // completed hour, still expanded, pulsing
   | Slide of ease : float // ease-in collapse (t³)
   | Filling of glow : float

/// Piecewise-linear layout with hour k expanded.
type Layout =
   {
      Map : float -> float // minutes-since-DayStart → x (device-independent px)
      Edge : float // left edge of the expanded hour = now line's home
      S : float // px/min outside the expanded hour (constant all day)
      WL : float // px width of the expanded hour
   }

module ClockMath =

   /// Top-hat lens on hour k with global amortization:
   /// s = (W − W_L)/(span − 60) is independent of k, so every hour outside
   /// the expanded one sits at a fixed position for the entire day. Between
   /// commits the layout is fully static; only the fill edge moves.
   let layoutFor (t : ClockTuning) (width : float) (k : int) : Layout =
      let span = t.SpanMinutes
      let wl = t.HourWidthMultiple * width * 60.0 / span
      let s = (width - wl) / (span - 60.0)
      let ks = float k * 60.0
      let edge = s * ks

      {
         Map =
            fun m ->
               if m <= ks then s * m
               elif m >= ks + 60.0 then edge + wl + s * (m - ks - 60.0)
               else edge + (m - ks) * wl / 60.0
         Edge = edge
         S = s
         WL = wl
      }

   let lerp a b t = a + (b - a) * t

   let phase (t : ClockTuning) (hourIndex : int) (sinceTick : TimeSpan) : Phase =
      if hourIndex = 0 then
         FirstHour
      else
         let sec = sinceTick.TotalSeconds
         let f = max 0.001 t.FlashDuration.TotalSeconds
         let sl = max 0.02 t.SlideDuration.TotalSeconds

         if sec < f then
            let p = sin (Math.PI * 2.0 * sec / f)
            Flash(0.6 * p * p) // two pulses across the window
         elif sec < f + sl then
            let tn = (sec - f) / sl
            Slide(tn * tn * tn)
         else
            let ga = sec - f - sl
            let coolTau = max 0.02 (t.CoolDuration.TotalSeconds / 3.0)
            let glow = 0.8 * exp (-ga / 0.13) + 0.3 * exp (-ga / coolTau)
            Filling(if glow < 0.01 then 0.0 else glow)

   type TickLevel =
      {
         Step : float
         Coarser : float
         HeightFrac : float
         Width : float
         Gate : float
      }

   /// Gated fine levels. Quarters are structural (drawn per expanded hour),
   /// so the chain here is 5 min → 1 min → 15 s, each fading in only where
   /// its local pixel spacing clears its gate.
   let tickLevels =
      [
         {
            Step = 5.0
            Coarser = 15.0
            HeightFrac = 0.55
            Width = 1.0
            Gate = 4.0
         }
         // {
         //    Step = 1.0
         //    Coarser = 5.0
         //    HeightFrac = 0.40
         //    Width = 1.0
         //    Gate = 4.0
         // }
         // {
         //    Step = 0.25
         //    Coarser = 1.0
         //    HeightFrac = 0.26
         //    Width = 1.0
         //    Gate = 3.2
         // }
      ]

module private Palette =
   let amber = Color.FromRgb(0xF2uy, 0xA3uy, 0x3Cuy)
   let hot = Color.FromRgb(0xFFuy, 0xECuy, 0xC8uy)
   let dim = Color.FromRgb(0x2Auy, 0x24uy, 0x18uy)
   let kerf = Color.FromRgb(0x0Buy, 0x0Duy, 0x11uy)
   let marker = Color.FromRgb(0xCDuy, 0xD4uy, 0xDCuy)
   let labelC = Color.FromRgb(0x5Cuy, 0x64uy, 0x6Euy)

   let lerpColor (a : Color) (b : Color) (t : float) =
      let t = Math.Clamp(t, 0.0, 1.0)

      let ch (x : byte) (y : byte) =
         byte (Math.Round(float x + (float y - float x) * t))

      Color.FromRgb(ch a.R b.R, ch a.G b.G, ch a.B b.B)

   let solid (c : Color) : IBrush = ImmutableSolidColorBrush c

   let solidA (c : Color) (alpha : float) : IBrush =
      ImmutableSolidColorBrush(c, Math.Clamp(alpha, 0.0, 1.0))

   let typeface = Typeface(FontFamily "Fira Code, Consolas, monospace")

type LinearClockControl () =
   inherit Control ()

   let mutable animating = false

   static let clockProperty = AvaloniaProperty.Register<LinearClockControl, Clock>("Clock", Clock.Default)

   static let tuningProperty =
      AvaloniaProperty.Register<LinearClockControl, ClockTuning>("Tuning", ClockTuning.Default)

   static member ClockProperty = clockProperty
   static member TuningProperty = tuningProperty

   member this.Clock
      with get () = this.GetValue clockProperty
      and set (v : Clock) = this.SetValue(clockProperty, v) |> ignore

   member this.Tuning
      with get () = this.GetValue tuningProperty
      and set (v : ClockTuning) = this.SetValue(tuningProperty, v) |> ignore

   override this.OnAttachedToVisualTree e =
      base.OnAttachedToVisualTree e

      if not animating then
         animating <- true

         match TopLevel.GetTopLevel this with
         | null -> ()
         | top ->
            let rec loop (_ : TimeSpan) =
               if animating then
                  this.InvalidateVisual()
                  top.RequestAnimationFrame(Action<TimeSpan> loop)

            top.RequestAnimationFrame(Action<TimeSpan> loop)


   override this.OnDetachedFromVisualTree e =
      animating <- false
      base.OnDetachedFromVisualTree e

   override this.Render(ctx : DrawingContext) =
      base.Render ctx
      let tuning = this.Tuning
      let bounds = this.Bounds

      if bounds.Width <= 0.0 || tuning.HourCount < 2 then
         ()
      else

         // ---- snapshot; everything below is a function of these values ----
         let tod = this.Clock.GetTimeOfDay()

         let mNow =
            (tod - tuning.DayStart).TotalMinutes
            |> max 0.0
            |> min (tuning.SpanMinutes - 1e-4) // outside window: pinned empty/full

         let h = int (mNow / 60.0)
         let sinceTick = TimeSpan.FromMinutes(mNow - float h * 60.0)
         let phase = ClockMath.phase tuning h sinceTick

         let w = bounds.Width
         let mid = bounds.Height / 2.0
         let barH = tuning.BarHeight
         let yTop = mid - barH / 2.0

         let lb = ClockMath.layoutFor tuning w h

         let la =
            if h > 0 then
               Some(ClockMath.layoutFor tuning w (h - 1))
            else
               None

         let map, markerX, oldHot =
            match phase, la with
            | FirstHour, _
            | _, None -> lb.Map, lb.Edge, 0.0
            | Flash p, Some a -> a.Map, a.Edge, p
            | Slide e, Some a -> (fun m -> ClockMath.lerp (a.Map m) (lb.Map m) e), ClockMath.lerp a.Edge lb.Edge e, 0.25 * (1.0 - e)
            | Filling glow, Some _ -> lb.Map, lb.Edge, glow

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
               if j < h then
                  Palette.amber
               elif j = h then
                  Palette.lerpColor Palette.dim Palette.amber 0.07 // staged
               else
                  Palette.dim

            let cellColor =
               if j = h - 1 && oldHot > 0.0 then
                  Palette.lerpColor baseColor Palette.hot oldHot
               else
                  baseColor

            fillRect (Palette.solid cellColor) x0 yTop (x1 - x0) barH

            // current-hour fill + leading edge
            if j = h then
               let xf = map (min mNow m1)

               if xf > x0 then
                  fillRect (Palette.solid (Palette.lerpColor Palette.amber Palette.hot 0.12)) x0 yTop (xf - x0) barH
                  fillRect (Palette.solidA Palette.hot 0.9) (xf - 1.0) yTop 1.0 barH

            // structural quarter kerfs + labels: any hour wide enough,
            // so the completed hour keeps its quarters through flash & collapse
            if x1 - x0 > 120.0 then
               for q in [ 15.0; 30.0; 45.0 ] do
                  let xq = map (m0 + q)
                  fillRect (Palette.solid Palette.kerf) (xq - 1.0) yTop 2.0 barH
                  label (sprintf ":%02d" (int q)) (xq + 3.0) (yTop + barH + 4.0) Palette.labelC 0.75

            // commit edge highlight on the landed hour
            match phase with
            | Filling glow when j = h - 1 && glow > 0.0 ->
               let br = Palette.solidA Palette.hot glow
               fillRect br x0 (yTop - 1.0) (x1 - x0) 1.0
               fillRect br x0 (yTop + barH) (x1 - x0) 1.0
            | _ -> ()

         // ---- gated fine ticks ----
         for lvl in ClockMath.tickLevels do
            // sub-5-minute levels can only clear their gate near the expanded
            // hour(s); bound the scan to hours h−1..h during transitions
            let lo, hi =
               if lvl.Step < 5.0 then
                  max 0.0 (float (h - 1) * 60.0 - lvl.Step), min tuning.SpanMinutes (float h * 60.0 + 60.0 + lvl.Step)
               else
                  0.0, tuning.SpanMinutes

            let mutable m = ceil (lo / lvl.Step) * lvl.Step

            while m < hi do
               if m % lvl.Coarser <> 0.0 && m > 0.0 && m < tuning.SpanMinutes then
                  let spacing = (map (m + lvl.Step) - map (m - lvl.Step)) / 2.0
                  let a = Math.Clamp((spacing - lvl.Gate) / lvl.Gate, 0.0, 1.0)

                  if a > 0.02 then
                     let ht = barH * lvl.HeightFrac
                     fillRect (Palette.solidA Palette.kerf (0.92 * a)) (map m - lvl.Width / 2.0) (mid - ht / 2.0) lvl.Width ht

               m <- m + lvl.Step

         // // ---- commit ripple: wavefronts outward from the landing point ----
         // match rippleAge with
         // | ValueSome ga ->
         //    let alpha = 0.28 * exp (-ga / 0.35)
         //
         //    if alpha > 0.01 then
         //       let xc = lb.Edge
         //       let d = 900.0 * ga
         //       let wv = 26.0
         //
         //       for dir in [ -1.0; 1.0 ] do
         //          let xf = xc + dir * d
         //          let transparent = Color.FromArgb(0uy, Palette.hot.R, Palette.hot.G, Palette.hot.B)
         //          let stops = GradientStops()
         //          stops.Add(GradientStop(transparent, 0.0))
         //          stops.Add(GradientStop(Palette.hot, 0.5))
         //          stops.Add(GradientStop(transparent, 1.0))
         //
         //          let brush =
         //             LinearGradientBrush(
         //                StartPoint = RelativePoint(0.0, 0.5, RelativeUnit.Relative),
         //                EndPoint = RelativePoint(1.0, 0.5, RelativeUnit.Relative),
         //                GradientStops = stops,
         //                Opacity = alpha
         //             )
         //
         //          ctx.FillRectangle(brush, Rect(xf - 3.0 * wv, yTop, 6.0 * wv, barH))
         // | ValueNone -> ()

         // ---- hour labels ----
         for j in 0 .. tuning.HourCount do
            let x = map (float j * 60.0)
            label (string (tuning.DayStart.Hours + j)) (x + 3.0) (yTop + barH + 0.0) Palette.labelC 0.9

         // ---- now line ----
         fillRect (Palette.solid Palette.marker) (markerX - 1.0) (yTop - 7.0) 2.0 (barH + 14.0)
