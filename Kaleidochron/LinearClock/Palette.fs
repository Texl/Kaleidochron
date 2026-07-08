namespace Kaleidochron

module private Palette =
   open System
   open Avalonia.Media
   open Avalonia.Media.Immutable

   let rgb (v : int) = Color.FromUInt32(0xFF000000u ||| uint v)

   // MP4 scheme
   module MP4 =
      let blue5 = rgb 0xBFEBFF // hsv(199  25% 100%)
      let blue4 = rgb 0x00AEFF // hsv(199 100% 100%)
      let blue3 = rgb 0x006999 // hsv(199 100%  60%)
      let blue2 = rgb 0x004666 // hsv(199 100%  40%)
      let blue1 = rgb 0x002333 // hsv(199 100%  20%)

      let gray5 = rgb 0xFFFFFF // hsv(0 0% 100%)
      let gray3 = rgb 0xD3D3D3 // hsv(0 0%  83%)
      let gray2 = rgb 0x545454 // hsv(0 0%  33%)
      let gray1 = rgb 0x2A2A2A // hsv(0 0%  17%)

      let orange5 = rgb 0xFFEECC
      let orange4 = rgb 0xFFA800
      let orange3 = rgb 0x996500
      // let orange2 = rgb // todo
      let orange1 = rgb 0x33220

      let cyan = rgb 0x49F7FF
      let green = rgb 0x7FED46
      let red = rgb 0xD8193B

   // old scheme
   module Old =
      let amber = rgb 0xF2A33C
      let amberHot = rgb 0xFFECC8
      let amberDim = rgb 0x2A2418

      let gray1 = rgb 0x0B0D11
      let gray2 = rgb 0x5C646E
      let gray3 = rgb 0xCDD4DC

   let cellHot = MP4.blue3
   let cell = MP4.blue2
   let cellDim = MP4.blue1

   let currentCellHot = MP4.blue5
   let currentCell = MP4.blue4
   let currentCellDim = MP4.blue3

   let kerf = MP4.gray1
   let labelC = MP4.gray3
   let marker = MP4.gray5

   let lerpColor (a : Color) (b : Color) (t : float) =
      let t = Math.Clamp(t, 0.0, 1.0)

      let ch (x : byte) (y : byte) =
         byte (Math.Round(float x + (float y - float x) * t))

      Color.FromRgb(ch a.R b.R, ch a.G b.G, ch a.B b.B)

   let solid (c : Color) : IBrush = ImmutableSolidColorBrush c

   let solidA (c : Color) (alpha : float) : IBrush =
      ImmutableSolidColorBrush(c, Math.Clamp(alpha, 0.0, 1.0))

   let gradient (t1 : float, c1 : Color) (t2 : float, c2 : Color) : IBrush =
      let stop1 = ImmutableGradientStop(Math.Clamp(t1, 0.0, 1.0), c1)
      let stop2 = ImmutableGradientStop(Math.Clamp(t2, 0.0, 1.0), c2)
      ImmutableLinearGradientBrush([| stop1; stop2 |])

   let gradientA (t1 : float, c1 : Color) (t2 : float, c2 : Color) (alpha : float) : IBrush =
      let stop1 = ImmutableGradientStop(Math.Clamp(t1, 0.0, 1.0), c1)
      let stop2 = ImmutableGradientStop(Math.Clamp(t2, 0.0, 1.0), c2)
      ImmutableLinearGradientBrush([| stop1; stop2 |], Math.Clamp(alpha, 0.0, 1.0))

   let typeface = Typeface(FontFamily "Fira Code, Consolas, monospace")
