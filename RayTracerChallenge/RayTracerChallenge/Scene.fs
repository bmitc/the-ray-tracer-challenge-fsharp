module RayTracer.Scene

open Utilities
open Tuples
open Color
open Canvas
open Matrix
open Transformation
open Object
open Ray
open LightAndShading

/// Represents a world, consisting of objects and a light source
[<Struct>]
type World<[<Measure>] 'PointUnit> =
    { /// Collection of objects contained in the world
      Objects     : Object list
      /// A single point light source for the world
      LightSource : Light<'PointUnit> }

/// Intersects the ray with all the objects in the world, returning an intersection list 
let intersectWorld world ray =
    world.Objects
    |> List.map (intersect ray)
    |> List.concat
    |> sort

/// Determines whether the point is in the shadow of one of the world's objects
let isShadowed world (point: Point<world>) =
    // Calculate the distance from the point to the world's light source and
    // the direction from the point toward the light source
    let vector = world.LightSource.Position - point
    let distance = magnitude vector
    let direction = normalize vector

    // A ray pointing from the point toward the world's light source
    let ray = ray point direction

    // Calculate any intersections with the ray, which starts at the point and points
    // in the direction of the world's light source, and all objects in the world
    let intersections = intersectWorld world ray

    // Check if there was a hit. If so, if the time is less than the distance between
    // the point and the world's light source, then the hit lies between the point and
    // the light source.
    let hit = hit intersections
    match hit with
    | Some intersection when intersection.Time < distance -> true
    | _ -> false

/// Represents precomputated values of an intersection, capturing the time and
/// object of the intersection
[<Struct>]
type Computation<[<Measure>] 'PointUnit> =
    { /// Time of the intersection
      Time      : float
      /// The intersection object
      Object    : Object
      /// The point, in world space, where the intersection occurred
      Point     : Point<'PointUnit>
      /// The point with a z component slightly less than z=0
      OverPoint : Point<'PointUnit>
      /// Vector pointing back towards the camera, or eye
      Eye       : Vector
      /// Normal at the intersection point and object's surface
      Normal    : Vector
      /// Describes if a hit occurs on the inside of the object's shape
      Inside    : bool }

/// Prepares a Computation relating to the intersection
let prepareComputation (intersection: Intersection) ray =
    let time    = intersection.Time
    let object  = intersection.Object
    let point   = position ray time
    let eye     = -ray.Direction
    let normal  = normalAt object point
    let d       = dot normal eye
    let computationNormal = if d < 0.0 then -normal else normal
    { Time      = time
      Object    = object
      Point     = point
      OverPoint = point + epsilon * computationNormal
      Eye       = eye
      Normal    = computationNormal
      Inside    = d < 0.0 }

/// Shades the world at a given intersection
let shadeHit world (computation: Computation<world>) =
    let shadowed = isShadowed world computation.OverPoint
    let material =
        match computation.Object.Material with
        | Some m -> m
        | None   -> Material.Default
    lighting material
             computation.Object
             world.LightSource
             computation.OverPoint
             computation.Eye
             computation.Normal
             shadowed

/// Computes the color at the first intersection, if any, of the ray
/// with an object in the world. In the event of no intersection,
/// black is returned.
let colorAt world ray =
    match hit (intersectWorld world ray) with
    | Some i -> prepareComputation i ray
                |> shadeHit world
    | None   -> black

/// <summary>
/// Returns a transformation matrix that orients the eye to the world.
/// </summary>
/// <param name="from"> The location of the eye in the world</param>
/// <param name="toward"> The location of the point in the world that the eye is looking at</param>
/// <param name="up"> The vector indicating which direction is up</param>
/// <returns>A transformation matrix used to orient the eye in the world.</returns>
let viewTransform from toward up =
    let forward = normalize (toward - from)
    let left    = crossProduct forward (normalize up)
    let trueUp  = crossProduct left forward
    let orientation = Matrix(4, 4, array2D [[    left.I;     left.J;     left.K; 0.0];
                                            [  trueUp.I;   trueUp.J;   trueUp.K; 0.0];
                                            [-forward.I; -forward.J; -forward.K; 0.0];
                                            [       0.0;        0.0;        0.0; 1.0]])
    let translationMatrix = getTransformMatrix (Translation(-from.X, -from.Y, -from.Z))
    orientation * translationMatrix

/// Represents a camera as a canvas one pixel in front of the camera
[<Struct>]
type Camera =
    { /// Horizontal size in pixels of the canvas that the picture will be rendered to
      HorizontalSize : int<pixels>
      /// Vertical size in pixels of the canvas that the picture will be rendered to
      VerticalSize   : int<pixels>
      /// Angle that describes how much the camera can see. When the field of view is small,
      /// the view will appear zoomed in, magnifying a smaller area of the scene.
      FieldOfView    : float<radians>
      /// Matrix describing how the world should be oriented relative to the camera
      Transform      : Matrix }

    with

    /// A default camera
    static member Default =
        { HorizontalSize = 0<pixels>
          VerticalSize   = 0<pixels>
          FieldOfView    = 0.0<radians>
          Transform      = Matrix(4, 4, Identity) }

    /// A value used to compute the HalfWidth and HalfHeight of the camera's canvas
    /// It is the width of half of the canvas one world unit away from the camera
    member private this.HalfView =
        castFloatUnit<world> (tan(removeRadians(this.FieldOfView) / 2.0))

    /// The aspect ratio, the horizontal size over the vertical size, of the camera
    member private this.AspectRatio =
        (floatPreserveUnits this.HorizontalSize) / (floatPreserveUnits this.VerticalSize)

    /// 
    member this.HalfWidth =
        if this.AspectRatio >= 1.0
        then this.HalfView
        else this.HalfView * this.AspectRatio

    /// 
    member this.HalfHeight =
        if this.AspectRatio >= 1.0
        then this.HalfView / this.AspectRatio
        else this.HalfView

    /// Conversion factor to go from pixels to world units
    member this.PixelSize = (this.HalfWidth * 2.0) / (floatPreserveUnits this.HorizontalSize)
    // Pixels are square, so only need to use horizontal or vertical values

/// Convenience function for creating a camera with a default view transform
let camera (horizontalPixels, verticalPixels, fieldOfView) =
    { Camera.Default with HorizontalSize = horizontalPixels;
                          VerticalSize   = verticalPixels;
                          FieldOfView    = fieldOfView }

/// Ray that emanates from the camera and passes through the (x,y) pixel on the camera's canvas
let rayForPixel origin cameraTransformMatrix (camera: Camera) (x: float<pixels>) (y: float<pixels>) =
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
    //let transformMatrix = camera.Transform.Invert()
    let pixel = applyTransformMatrix cameraTransformMatrix (point(worldX, worldY, -1.0<world>))
    //let origin = applyTransformMatrix transformMatrix (pointu<world>(0.0, 0.0, 0.0))
    let direction = normalize (pixel - origin)

    ray origin direction

/// Render a world onto a canvas using the camera
let render camera world =
    // Create a canvas that the camera will image upon
    let image = Canvas(camera.HorizontalSize, camera.VerticalSize)

    // Pre-compute the camera's transform matrix and world origin, which will not change during the render
    let cameraTransformMatrix = camera.Transform.Invert()
    let origin = applyTransformMatrix cameraTransformMatrix (pointu<world>(0, 0, 0))

    // Compute the color for every pixel, in parallel
    image.UpdatePixels(fun x y _color -> colorAt world (rayForPixel origin cameraTransformMatrix camera (floatUnits<pixels> x) (floatUnits<pixels> y)))
