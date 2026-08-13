namespace Kaleidochron

module private Tween =
   open System

   let private clamp t = Math.Clamp(t, 0.0, 1.0)

   module Linear =
      let easeIn t = clamp t

      let easeOut t = clamp t

      let easeInOut t = clamp t

   module Quadratic =
      let easeIn t = let s = clamp t in s * s

      let easeOut t = let s = 1.0 - clamp t in 1.0 - s * s

      let easeInOut t =
         if t < 0.5 then
            let s = clamp t in 2.0 * s * s
         else
            let s = 1.0 - clamp t in 1.0 - 2.0 * s * s

   module Cubic =
      let easeIn t = let s = clamp t in s * s * s

      let easeOut t =
         let s = 1.0 - clamp t in 1.0 - s * s * s

      let easeInOut t =
         if t < 0.5 then
            let s = clamp t in 4.0 * s * s * s
         else
            let s = 1.0 - clamp t in 1.0 - 4.0 * s * s * s

   module Quartic =
      let easeIn t = let s = clamp t in s * s * s * s

      let easeOut t =
         let s = 1.0 - clamp t in 1.0 - s * s * s * s

      let easeInOut t =
         if t < 0.5 then
            let s = clamp t in 8.0 * s * s * s * s
         else
            let s = 1.0 - clamp t in 1.0 - 8.0 * s * s * s * s

   module Quintic =
      let easeIn t = let s = clamp t in s * s * s * s * s

      let easeOut t =
         let s = 1.0 - clamp t in 1.0 - s * s * s * s * s

      let easeInOut t =
         if t < 0.5 then
            let s = clamp t in 16.0 * s * s * s * s * s
         else
            let s = 1.0 - clamp t in 1.0 - 16.0 * s * s * s * s * s

   module Sinusoidal =
      let private piOverTwo = Math.PI / 2.0

      let easeIn t =
         let s = clamp t in 1.0 - Math.Cos(s * piOverTwo)

      let easeOut t =
         let s = clamp t in Math.Sin(s * piOverTwo)

      let easeInOut t =
         let s = clamp t in 0.5 - 0.5 * Math.Cos(s * Math.PI)

   module Exponential =
      let easeIn t =
         match clamp t with
         | 0.0 -> 0.0
         | s -> Math.Pow(2.0, 10.0 * (s - 1.0))

      let easeOut t =
         1.0 - Math.Pow(2.0, -10.0 * (clamp t))

      let easeInOut t =
         match clamp t with
         | 0.0 -> 0.0
         | s when s < 0.5 -> Math.Pow(2.0, 20.0 * s - 11.0)
         | s -> 1.0 - Math.Pow(2.0, -20.0 * s + 9.0)
