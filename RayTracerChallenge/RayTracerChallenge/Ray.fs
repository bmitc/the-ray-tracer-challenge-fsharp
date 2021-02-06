module RayTracer.Ray

open Utilities
open Tuples
open Color
open Transformation

/// A ray starting at an origin and pointing in a direction
type Ray = { Origin: Point; Direction: Vector}

/// Convience function for creating a ray record
let ray origin direction = { Origin = origin; Direction = direction}

/// Calculates the position at the given time for the ray by parameterizing the ray
let position ray (time: float) = ray.Origin + time * ray.Direction

/// Represents various types of shapes to be ray traced
type Shape = Sphere

/// A record to hold various material properties
type Material =
    { Color     : Color
      Ambient   : float
      Diffuse   : float
      Specular  : float
      Shininess : float }
    with
        static member Default = { Color     = color(1.0, 1.0, 1.0)
                                  Ambient   = 0.1
                                  Diffuse   = 0.9
                                  Specular  = 0.9
                                  Shininess = 200.0 }

/// Convenience function for creating a default material
let material () = Material.Default

/// An object is a shape with a possible transform and material assigned to it
type Object =
    { Shape     : Shape
      Transform : Transform option
      Material  : Material option }

/// Convenience function for creating a sphere object
let sphere = {Shape = Sphere; Transform = None; Material = None}

/// An intersection consists of what object was intersected and at what time along a ray
type Intersection =
    { Object : Object
      Time   : float }

/// Transforms a ray by transforming the underlying origin point and direction vector
let transform transform ray =
    { Origin = applyTransform transform ray.Origin;
      Direction = applyTransform transform ray.Direction }

/// Calculates all the intersections of the ray with the object. The intersections
/// are not sorted. It's possible there are no intersections, which is an empty list.
let intersect (ray: Ray) (object: Object) =
    let r =
        match object.Transform with
        | Some t -> transform (inverse t) ray
        | None   -> ray
    match object.Shape with
    | Sphere -> let sphereToRay = r.Origin - point(0.0, 0.0, 0.0)
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

/// Sorts an intersection list by time in order of lowest to greatest
let sort = List.sortBy (fun intersection -> intersection.Time)

/// Determines whether an intersection list, which is automatically sorted, contains
/// a hit or not, represented as an intersection option type.
let hit intersections = intersections |> sort |> List.tryFind (fun i -> i.Time >= 0.0)