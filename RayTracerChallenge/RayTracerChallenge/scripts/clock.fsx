#load "load.fsx"

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Canvas
open RayTracer.PPM
open RayTracer.Transformation

let twelveOClock = point(0.0, 1.0, 0.0)
let angle = pi/6.0
let canvasSize = 200
let center = point((float canvasSize)/2.0, (float canvasSize)/2.0, 0.0)

let writeHour h =
    let scaleFactor = (float canvasSize) * 3.0 / 8.0
    twelveOClock
    |> rotate (Z, float(h) * angle)             // move the 12 o'clock position to the hour position
    |> scale (scaleFactor, scaleFactor, 0.0)    // scale the clock radius to 3/8 of canvas size
    |> translate (center.X, center.Y, center.Z) // translate the clock to the middle of the canvas
    |> (fun p -> writePixel (roundToInt p.X1, roundToInt p.X2) (color(1.0, 1.0, 1.0)))
    //|> (fun p -> writePixel (int (p :?> Point).X, int (p :?> Point).Y) (color(1.0, 1.0, 0.0)))

let c = List.fold (fun canvas hour -> canvas |> writeHour hour)
                  (canvas (canvasSize, canvasSize))
                  [1..12]

writeToPPM c (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../Images/clock.ppm"))