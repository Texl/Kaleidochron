module Kaleidochron.AppBarInterop

open Avalonia.Platform
open Vanara.PInvoke

type AppBarEdge =
   | Top
   | Bottom
   | Left
   | Right

type Configuration =
   {
      AppBarEdge : Shell32.ABE
      AppBarThickness : int
      MessageId : uint
   }

type ScreenEdgeAppBar (hWnd : HWND, configuration : Configuration) =

   let mutable registration : nativeint = 0
   let mutable appBarData = Shell32.APPBARDATA(hWnd, configuration.MessageId, configuration.AppBarEdge)

   let (|AppBarNotification|) (wParam : nativeint) = enum<Shell32.ABN> (int wParam)

   let calculateBounds (screenRect : RECT) (appBarThickness : int) : RECT =
      let left = screenRect.Left
      let top = screenRect.Top
      let right = screenRect.Right
      let bottom = screenRect.Bottom

      match configuration.AppBarEdge with
      | Shell32.ABE.ABE_TOP -> RECT(left, top, right, top + appBarThickness)
      | Shell32.ABE.ABE_BOTTOM -> RECT(left, bottom - appBarThickness, right, bottom)
      | Shell32.ABE.ABE_LEFT -> RECT(left, top, left + appBarThickness, bottom)
      | Shell32.ABE.ABE_RIGHT -> RECT(right - appBarThickness, top, right, bottom)
      | _ -> RECT(left, top, right, bottom)

   member _.Configuration = configuration

   member _.Register() =
      registration <- Shell32.SHAppBarMessage(Shell32.ABM.ABM_NEW, &appBarData)

   member _.Unregister() =
      if registration <> 0 then
         Shell32.SHAppBarMessage(Shell32.ABM.ABM_REMOVE, &appBarData) |> ignore
         registration <- 0

   member _.UpdatePosition(screen : Screen, appBarThickness : int) : bool =
      let screenRect =
         let r = screen.Bounds
         RECT(r.X, r.Y, r.Right, r.Bottom)

      appBarData.rc <- calculateBounds screenRect appBarThickness

      Shell32.SHAppBarMessage(Shell32.ABM.ABM_QUERYPOS, &appBarData) |> ignore

      appBarData.rc <- calculateBounds appBarData.rc appBarThickness
      Shell32.SHAppBarMessage(Shell32.ABM.ABM_SETPOS, &appBarData) |> ignore

      let rect = appBarData.rc
      let width = rect.Right - rect.Left
      let height = rect.Bottom - rect.Top
      User32.MoveWindow(hWnd, rect.Left, rect.Top, width, height, bRepaint = true)

   member this.HandleNotification(AppBarNotification notification, screen : Screen, appBarThickness : int) : bool =
      match notification with
      | Shell32.ABN.ABN_POSCHANGED -> this.UpdatePosition(screen, appBarThickness)
      | _ -> false
