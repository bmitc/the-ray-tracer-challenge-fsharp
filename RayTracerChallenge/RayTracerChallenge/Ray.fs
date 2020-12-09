module RayTracer.Ray

open Utilities
open Tuples
open Color
open Transformation

type Ray = { Origin: Point; Direction: Vector}

let ray origin direction = { Origin = origin; Direction = direction}

let position ray (time: float) = ray.Origin + time * ray.Direction

type Shape = Sphere

type Material = { Color     : Color;
                  Ambient   : float;
                  Diffuse   : float;
                  Specular  : float;
                  Shininess : float }
    with
        static member Default = { Color     = color(1.0, 1.0, 1.0);
                                  Ambient   = 0.1;
                                  Diffuse   = 0.9;
                                  Specular  = 0.9;
                                  Shininess = 200.0 }

let material () = Material.Default

type Object = {Shape: Shape; Transform: Transform option; Material: Material option}

let sphere = {Shape = Sphere; Transform = None; Material = None}

type Intersection = { Object: Object; Time: float}

let transform transform ray =
    { Origin = applyTransform transform ray.Origin;
      Direction = applyTransform transform ray.Direction }

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

let sort = List.sortBy (fun intersection -> intersection.Time)

let hit intersections = intersections |> sort |> List.tryFind (fun i -> i.Time >= 0.0)