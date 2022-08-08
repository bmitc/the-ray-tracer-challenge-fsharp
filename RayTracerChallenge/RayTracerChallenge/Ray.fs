/// Ray type and functions to process ray hits and intersections
module RayTracer.Ray

open Utilities
open Tuples
open Transformation
open Object

/// A ray starting at an origin and pointing in a direction
type Ray<[<Measure>] 'PointUnit> =
    { /// The point where the ray emanates from
      Origin    : Point<'PointUnit>
      /// The pointing direction of the ray
      Direction : Vector }

/// Convience function for creating a ray record
let ray origin direction = { Origin = origin; Direction = direction}

/// Calculates the position along a ray at the given time by parameterizing the ray
let position ray (time: float) = ray.Origin + time * ray.Direction

/// An intersection consists of what object was intersected and at what time along a ray
type Intersection =
    { /// The object that was intersected by a ray
      Object : Object
      /// The time at which the ray intersected the object
      Time   : float }

/// Transforms a ray by transforming the underlying origin point and direction vector
let transform transform ray =
    { Origin = applyTransform transform ray.Origin;
      Direction = applyTransform transform ray.Direction }

/// Transforms the given ray in world space into the object's local space, using the object's transform
let localRay (ray: Ray<world>) object : Ray<object> =
    let objectRay = { Origin = convertPointUnits<world, object> ray.Origin; Direction = ray.Direction }
    match object.Transform with
    | Some t -> transform (inverse t) objectRay
    | None   -> objectRay

/// Calculates all the intersections of the ray with the object. The intersections
/// are not sorted. It's possible there are no intersections, which is an empty list.
let intersect (ray: Ray<world>) (object: Object) =
    let r = localRay ray object
    match object.Shape with
    | Sphere  -> let sphereToRay = r.Origin - pointu<object>(0.0, 0.0, 0.0)
                 let a = dotProduct r.Direction r.Direction
                 let b = 2.0 * (dotProduct r.Direction sphereToRay)
                 let c = (dotProduct sphereToRay sphereToRay) - 1.0
                 let discriminant = b*b - 4.0*a*c
                 let times =
                     match discriminant with
                     | _ when compareFloat a 0.0 -> []
                     | d when d < 0.0            -> []
                     | d when compareFloat d 0.0 -> [-b / (2.0*a)]
                     | d                         -> [(-b - sqrt d) / (2.0*a); (-b + sqrt d) / (2.0*a)]
                 List.map (fun t -> {Object = object; Time = t}) times
    | PlaneXZ -> // The ray is parallel or coplanar with the XY plane, so no intersections
                 if abs(r.Direction.J) < epsilon
                 then []
                 // The ray's origin is either above or below the XY plane and the ray has
                 // a non-zero direction in J.
                 elif abs(r.Origin.Y |> removeUnits) > epsilon && abs(r.Direction.J) > epsilon
                 // Solve y = origin_Y + t * direction_J = 0 (ray parameterization) for t
                 then let t = (-r.Origin.Y / r.Direction.J) |> removeUnits
                      [{Object = object; Time = t}]
                 else []

/// Sorts an intersection list by time in order of lowest to greatest
let sort = List.sortBy (fun intersection -> intersection.Time)

/// Determines whether an intersection list, which is automatically sorted, contains
/// a hit or not, represented as an intersection option type.
let hit intersections =
    intersections
    |> sort
    |> List.tryFind (fun intersection -> intersection.Time >= 0.0)
