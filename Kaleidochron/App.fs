module Kaleidochron.App

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent

type App () =
   inherit Application ()

   override this.Initialize() =
      this.Styles.Add(FluentTheme())
      this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

   override this.OnFrameworkInitializationCompleted() =
      match this.ApplicationLifetime with
      | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
         let mainWindow = Shell.MainWindow()
         desktopLifetime.MainWindow <- mainWindow
      | _ -> ()
