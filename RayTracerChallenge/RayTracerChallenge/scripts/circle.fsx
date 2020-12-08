#load "load.fsx"

open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Canvas
open RayTracer.PPM
open RayTracer.Ray

let rayOrigin = point(0.0, 0.0, -5.0)
let wallZ = 10.0
let wallSize = 7.0
let canvasSize = 200.0
let pixelSize = wallSize / canvasSize
let halfSize = wallSize / 2.0
let canvas = Canvas(canvasSize, canvasSize)

let compute x y =
    let worldX = -halfSize + pixelSize * x
    let worldY =  halfSize - pixelSize * y
    let position = point(worldX, worldY, wallZ)
    let r = ray rayOrigin (normalize (position - rayOrigin))
    let xs = intersect r sphere
    match hit xs with
    | Some _ -> color(0.0, 0.5, 1.0)
    | None   -> color(0.0, 0.0, 0.0)

canvas.UpdatePixels(fun x y _ -> compute (float x) (float y))

writeToPPM canvas (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/circle.ppm"))