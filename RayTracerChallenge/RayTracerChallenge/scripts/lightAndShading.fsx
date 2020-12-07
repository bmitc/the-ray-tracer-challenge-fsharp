#load "load.fsx"

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Canvas
open RayTracer.PPM
open RayTracer.Ray
open RayTracer.LightAndShading

let rayOrigin = point(0.0, 0.0, -5.0)
let wallZ = 10.0
let wallSize = 7.0
let canvasSize = 1000.0
let pixelSize = wallSize / canvasSize
let halfSize = wallSize / 2.0
let c = canvas (roundToInt canvasSize, roundToInt canvasSize)

let m = {material() with Color = color(0.0, 0.5, 1.0)}
let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}

let compute x y =
    let worldX = -halfSize + pixelSize * x
    let worldY =  halfSize - pixelSize * y
    let pos = point(worldX, worldY, wallZ)
    let r = ray rayOrigin (normalize (pos - rayOrigin))
    let xs = intersect r sphere
    match hit xs with
    | Some i -> let point = position r i.Time
                let normal = normalAt i.Object None point
                let eye = -r.Direction
                lighting m light point eye normal
    | None   -> color(0.0, 0.0, 0.0)

let newC = mapxy (fun x y _ -> compute (float x) (float y)) c

writeToPPM newC (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/lightAndShading.ppm"))