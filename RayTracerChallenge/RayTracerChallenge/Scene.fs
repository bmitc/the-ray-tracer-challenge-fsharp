module RayTracer.Scene

open Utilities
open Tuples
open Color
open Canvas
open Matrix
open Transformation
open Ray
open LightAndShading

type World =
    { Objects     : Object list;
      LightSource : Light}

let intersectWorld world ray =
    world.Objects
    |> List.map (intersect ray)
    |> List.concat
    |> sort

type Computation =
    { Time   : float;
      Object : Object;
      Point  : Point;
      Eye    : Vector;
      Normal : Vector;
      Inside : bool}

let prepareComputation (intersection : Intersection) ray =
    let time = intersection.Time
    let object = intersection.Object
    let point = position ray time
    let eye = -ray.Direction
    let normal = normalAt object point
    let d = dot normal eye
    { Time   = time;
      Object = object;
      Point  = point;
      Eye    = eye;
      Normal = if d < 0.0 then -normal else normal;
      Inside = d < 0.0}

let shadeHit world computation =
    let material =
        match computation.Object.Material with
        | Some m -> m
        | None   -> material()
    lighting material
             world.LightSource
             computation.Point
             computation.Eye
             computation.Normal

let colorAt world ray =
    match hit(intersectWorld world ray) with
    | Some i -> prepareComputation i ray |> shadeHit world
    | None   -> black

let viewTransform from toward up =
    let forward = normalize (toward - from)
    let left = crossProduct forward (normalize up)
    let trueUp = crossProduct left forward
    let orientation = Matrix(4, 4, array2D [[    left.I;     left.J;     left.K; 0.0];
                                            [  trueUp.I;   trueUp.J;   trueUp.K; 0.0];
                                            [-forward.I; -forward.J; -forward.K; 0.0];
                                            [       0.0;        0.0;        0.0; 1.0]])
    let translationMatrix = getTransformMatrix (Translation(-from.X, -from.Y, -from.Z))
    orientation * translationMatrix

type Camera =
    { HorizontalSize : float<pixels>
      VerticalSize   : float<pixels>
      FieldOfView    : float<radians>
      Transform      : Matrix }

    with

        /// A default camera
        static member Default =
            { HorizontalSize = 0.0<pixels>
              VerticalSize   = 0.0<pixels>
              FieldOfView    = 0.0<radians>
              Transform      = Matrix(4, 4, Identity) }

        /// A value used to compute the HalfWidth and HalfHeight of the camera's canvas
        member private this.HalfView = tan(removeRadians(this.FieldOfView) / 2.0)

        /// The aspect ratio, the horizontal size over the vertical size, of the camera
        member private this.AspectRatio = this.HorizontalSize / this.VerticalSize

        /// Half of the width of the camera's canvas
        member this.HalfWidth =
            if this.AspectRatio >= 1.0
            then this.HalfView * 1.0<world>
            else this.HalfView * this.AspectRatio * 1.0<world>

        /// Half of the height of the camera's canvas
        member this.HalfHeight =
            if this.AspectRatio >= 1.0
            then this.HalfView / this.AspectRatio * 1.0<world>
            else this.HalfView * 1.0<world>

        /// The size of pixels in world space
        member this.PixelSize = (this.HalfWidth * 2.0) / this.HorizontalSize

let camera (horizontalPixels, verticalPixels, fieldOfView) =
    { Camera.Default with HorizontalSize = horizontalPixels;
                          VerticalSize   = verticalPixels;
                          FieldOfView    = fieldOfView }

let rayForPixel (camera: Camera) (x: float<pixels>) (y: float<pixels>) =
    // The offset from the edge of the canvas to the pixel's center
    let xOffset = (x + 0.5<pixels>) * camera.PixelSize
    let yOffset = (y + 0.5<pixels>) * camera.PixelSize

    // The untransformed coordinates of the pixel in world space.
    // (Remember that the camera looks toward -z, so +x is to the *left*.)
    let worldX = camera.HalfWidth - xOffset
    let worldY = camera.HalfHeight - yOffset

    // Using the camera matrix, transform the canvas point and the origin,
    // and then compute the ray's direction vector.
    // (Remember that the canvas is at z=-1.)
    let transformMatrix = camera.Transform.Invert()
    let pixel = applyTransformMatrix transformMatrix (point(worldX/1.0<world>, worldY/1.0<world>, -1.0))
    let origin = applyTransformMatrix transformMatrix (point(0.0, 0.0, 0.0))
    let direction = normalize(pixel - origin)

    ray origin direction

let render camera world =
    let image = Canvas(roundToInt (camera.HorizontalSize/1.0<pixels>), roundToInt (camera.VerticalSize/1.0<pixels>))
    image.UpdatePixels(fun x y _ -> colorAt world (rayForPixel camera (float x*1.0<pixels>) (float y*1.0<pixels>)))