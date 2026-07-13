module Kaleidochron.Shell

open System
open Avalonia.Media
open Avalonia.Remote.Protocol.Input
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Input
open Vanara.PInvoke

type Model =
   {
      ClockTuning : ClockTuning
      Playing : bool
      HoursLogged : TimeSpan
   }

let init () =
   {
      ClockTuning = ClockTuning.Default
      Playing = true
      HoursLogged = TimeSpan.FromHours 3.25 // placeholder until wired to a real data source
   }

type Msg =
   | SetClockTuning of ClockTuning
   | SetHoursLogged of TimeSpan
   | TogglePlay
   | ResetRealTimeOffset
   | AdjustRealTimeOffset of TimeSpan
   | AdjustHoursLogged of TimeSpan

let update msg m =
   match msg with
   | SetClockTuning clockTuning -> { m with ClockTuning = clockTuning }
   | SetHoursLogged hours -> { m with HoursLogged = hours }
   | TogglePlay -> { m with Playing = not m.Playing }
   | ResetRealTimeOffset -> { m with Model.ClockTuning.RealTimeOffset = TimeSpan.Zero }
   | AdjustRealTimeOffset delta -> { m with Model.ClockTuning.RealTimeOffset = m.ClockTuning.RealTimeOffset + delta }
   | AdjustHoursLogged delta -> { m with HoursLogged = max TimeSpan.Zero (m.HoursLogged + delta) }

let shutdown exitCode =
   match Application.Current.ApplicationLifetime with
   | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.Shutdown(exitCode)
   | _ -> ()

let clock =
// #if DEBUG
//    let initial = DateTime.Now
//    let debugInitial = initial.Date + TimeSpan.FromHours(initial.TimeOfDay.Hours, 59, 55)
//    let offset = debugInitial - initial
//    { GetTimeOfDay = fun () -> (DateTime.Now + offset).TimeOfDay }
// #else
   Clock.Default
// #endif

let view (m : Model) (_dispatch : Msg -> unit) =
   StackPanel.create [
      StackPanel.background Brushes.Black
      StackPanel.children [
         LinearClock.create [
            Control.height 26.0
            LinearClock.clock clock
            LinearClock.tuning m.ClockTuning
            LinearClock.hoursLogged m.HoursLogged
         ]
      ]
   ]

type MainWindow () as this =
   inherit HostWindow ()

   let appBarEdge = Shell32.ABE.ABE_TOP
   let appBarThickness = 26
   let messageId = 0x0401u

   let mutable registeredAppBar : AppBarInterop.ScreenEdgeAppBar option = None

   let configuration : AppBarInterop.Configuration =
      {
         AppBarEdge = appBarEdge
         AppBarThickness = appBarThickness
         MessageId = messageId
      }

   let windowStylesCallback style exStyle =
      let WS_POPUP = 0x80000000u // remove window margin
      let WS_EX_TOOLWINDOW = 0x00000080u // hide from Alt+Tab
      struct (style ||| WS_POPUP, exStyle ||| WS_EX_TOOLWINDOW)

#if DEBUG
   // Debug keyboard hooks, window-local (click the bar to focus it first):
   //   Space     toggle play
   //   R         toggle real time
   //   D         reset day
   //   Up/Down   hours logged +/- 15 min
   //   Escape    quit (the app bar has no close button)
   let onDebugKey (dispatch : Msg -> unit) (e : KeyEventArgs) =
      let send msg =
         printfn $"[debug-keys] %A{e.Key} -> %A{msg}"
         dispatch msg
         e.Handled <- true

      let delta =
         if e.KeyModifiers.HasFlag(KeyModifiers.Shift) then
            TimeSpan.FromSeconds(30.0)
         else
            TimeSpan.FromMinutes(5.0)

      match e.Key with
      | Key.Space -> send TogglePlay
      | Key.R -> send ResetRealTimeOffset
      | Key.OemPlus -> send (AdjustRealTimeOffset delta)
      | Key.OemMinus -> send (AdjustRealTimeOffset -delta)
      | Key.Up -> send (AdjustHoursLogged (TimeSpan.FromMinutes(15.0)))
      | Key.Down -> send (AdjustHoursLogged (TimeSpan.FromMinutes(-15.0)))
      | Key.Escape ->
         printfn $"[debug-keys] %A{e.Key} -> shutdown"
         e.Handled <- true
         shutdown 0
      | _ -> ()

   let debugKeySub _model : Sub<Msg> =
      [
         [ "debug-keys" ], fun dispatch -> this.KeyDown |> Observable.subscribe (onDebugKey dispatch)
      ]
#endif

   let wndProcHookCallback =
      Win32Properties.CustomWndProcHookCallback(fun _ msg wParam lParam handled ->
         match registeredAppBar with
         | Some appBar when appBar.Configuration.MessageId = msg ->
            let screen = this.Screens.ScreenFromWindow(this)
            appBar.HandleNotification(wParam, screen, appBarThickness) |> ignore
            IntPtr.Zero
         | _ -> IntPtr.Zero)

   do
      base.Title <- "LinearClock"

      this.SizeToContent <- SizeToContent.Height
      this.WindowDecorations <- WindowDecorations.None
      this.ShowInTaskbar <- false

      // Set WS_POPUP style to remove window padding
      Win32Properties.AddWindowStylesCallback(this, windowStylesCallback)

      Elmish.Program.mkSimple init update view // make program
      |> Program.withHost this
#if DEBUG
      |> Program.withSubscription debugKeySub
#endif
      |> Program.run

   override _.OnOpened(e) =
      base.OnOpened(e)

      let appBar =
         let hwnd = this.TryGetPlatformHandle().Handle
         AppBarInterop.ScreenEdgeAppBar(hwnd, configuration)

      let screen = this.Screens.ScreenFromWindow(this)

      appBar.Register()
      appBar.UpdatePosition(screen, configuration.AppBarThickness) |> ignore

      registeredAppBar <- Some appBar

      Win32Properties.AddWndProcHookCallback(this, wndProcHookCallback)

   override _.OnClosed(e) =
      base.OnClosed(e)
      registeredAppBar |> Option.iter _.Unregister()
      registeredAppBar <- None
