module Kaleidochron.Program

open Avalonia
open App

[<EntryPoint>]
let main (args : string[]) =
   AppBuilder //
      .Configure<App>()
      .UsePlatformDetect()
      .UseSkia()
      .StartWithClassicDesktopLifetime(args)
