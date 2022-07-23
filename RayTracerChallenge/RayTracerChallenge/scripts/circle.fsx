// Implementation of the putting it together at the end of Chapter 5

#load "load.fsx"

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Canvas
open RayTracer.PPM
open RayTracer.Ray

let rayOrigin = pointu<world>(0.0, 0.0, -5.0)
let wallZ = 10.0<world>
let wallSize = 7.0<world>
let canvasSize = 200.0<pixels>
let pixelSize = wallSize / canvasSize
let halfSize = wallSize / 2.0
let canvas = Canvas(canvasSize)

let compute x y =
    let worldX = -halfSize + pixelSize * x
    let worldY =  halfSize - pixelSize * y
    let position = point(worldX, worldY, wallZ)
    let r = ray rayOrigin (normalize (position - rayOrigin))
    let xs = intersect r sphere
    match hit xs with
    | Some _ -> color(0.0, 0.5, 1.0)
    | None   -> color(0.0, 0.0, 0.0)

#time
canvas.UpdatePixels(fun x y _ -> compute (floatUnits<pixels> x) (floatUnits<pixels> y))
#time

writeToPPM canvas (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/circle.ppm"))