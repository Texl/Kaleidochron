namespace Kaleidochron

module LinearClock =
   open Avalonia.FuncUI.Builder
   open Avalonia.FuncUI.DSL
   open Avalonia.FuncUI.Types

   let create (attrs : IAttr<LinearClockControl> list) : IView<LinearClockControl> =
      ViewBuilder.Create<LinearClockControl>(attrs)

   let clock (value : Clock) : IAttr<LinearClockControl> =
      AttrBuilder<LinearClockControl>.CreateProperty<Clock>(LinearClockControl.ClockProperty, value, ValueNone)

   let tuning (value : ClockTuning) : IAttr<LinearClockControl> =
      AttrBuilder<LinearClockControl>.CreateProperty<ClockTuning>(LinearClockControl.TuningProperty, value, ValueNone)

   let hoursLogged (value : System.TimeSpan) : IAttr<LinearClockControl> =
      AttrBuilder<LinearClockControl>.CreateProperty<System.TimeSpan>(LinearClockControl.HoursLoggedProperty, value, ValueNone)
