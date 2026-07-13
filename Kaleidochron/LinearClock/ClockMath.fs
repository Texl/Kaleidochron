namespace Kaleidochron

[<AutoOpen>]
module Math =
   let inline (%%) a b =
      ((a % b) + b) % b

[<AutoOpen>]
module ClockMathTypes =
   open System
   type Clock =
      {
         GetTimeOfDay : unit -> TimeSpan
      }

      static member Default = { GetTimeOfDay = fun () -> DateTime.Now.TimeOfDay }

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

   /// Hours-logged gauge drawn under the clock bar, sharing its time→x map.
   type GaugeTuning =
      {
         Enabled : bool
         Origin : TimeSpan // clock time where the gauge's left edge sits
         Offset : float // px gap below the clock bar (clears the label band)
         Height : float
      }

      static member Default =
         {
            Enabled = true
            Origin = TimeSpan.FromHours 10.0
            Offset = 10.0
            Height = 4.0
         }

   type ClockTuning =
      {
         DayStart : TimeSpan
         DayEnd : TimeSpan
         HourWidthMultiple : float
         FlashDuration : TimeSpan
         SlideDuration : TimeSpan
         CoolDuration : TimeSpan
         BarHeight : float
         RealTimeOffset : TimeSpan
         Gauge : GaugeTuning
      }

      member this.Span = this.DayEnd - this.DayStart

      /// Left edge of the span-sized window containing timeOfDay. Windows
      /// tile the 24h day anchored at DayStart, so 9→21 rolls over to 21→9
      /// (and back again at 9), including the stretch past midnight.
      member this.WindowStart (timeOfDay : TimeSpan) : TimeSpan =
         let day = TimeSpan.FromDays(1).TotalMinutes
         let since = (timeOfDay - this.DayStart).TotalMinutes %% day
         let k = floor (since / this.Span.TotalMinutes)
         TimeSpan.FromMinutes((this.DayStart.TotalMinutes + k * this.Span.TotalMinutes) %% day)

      member this.HourCount = (this.DayEnd - this.DayStart).Hours

      static member Default =
         {
            DayStart = TimeSpan.FromHours 9.0
            DayEnd = TimeSpan.FromHours 21.0
            HourWidthMultiple = 8.0
            FlashDuration = TimeSpan.FromMilliseconds 600.0
            SlideDuration = TimeSpan.FromMilliseconds 1200.0
            CoolDuration = TimeSpan.FromMilliseconds 4800.0
            BarHeight = 12.0
            RealTimeOffset = TimeSpan.Zero
            Gauge = GaugeTuning.Default
         }

module ClockMath =
   open System

   /// Top-hat lens on hour k with global amortization:
   /// s = (W − W_L)/(span − 60) is independent of k, so every hour outside
   /// the expanded one sits at a fixed position for the entire day. Between
   /// commits the layout is fully static; only the fill edge moves.
   let layoutFor (t : ClockTuning) (width : float) (k : int) : Layout =
      let span = t.Span.TotalMinutes
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

   let inline lerp a b t = a + (b - a) * t

   let phase (t : ClockTuning) (hourIndex : int) (sinceTick : TimeSpan) : Phase =
      if hourIndex = 0 then
         FirstHour
      else
         let sec = sinceTick.TotalSeconds
         let f = max 0.001 t.FlashDuration.TotalSeconds
         let sl = max 0.02 t.SlideDuration.TotalSeconds

         if sec < f then
            let p = sin (Math.PI * 3.0 * sec / f)
            Flash(p * p) // two pulses across the window
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
            Step = 15.0
            Coarser = 60.0
            HeightFrac = 0.8
            Width = 1.0
            Gate = 4.0
         }
         {
            Step = 5.0
            Coarser = 15.0
            HeightFrac = 0.4
            Width = 1.0
            Gate = 20.0
         }
         {
            Step = 1.0
            Coarser = 5.0
            HeightFrac = 0.2
            Width = 1.0
            Gate = 20.0
         }
         {
            Step = 0.25
            Coarser = 1.0
            HeightFrac = 0.1
            Width = 1.0
            Gate = 20.0
         }
      ]
