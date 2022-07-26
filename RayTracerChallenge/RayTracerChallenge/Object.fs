module RayTracer.Object

open Utilities
open Tuples
open Color
open Transformation

/// Represents various types of shapes to be ray traced
type Shape =
    /// An abstract sphere. The size and location of the sphere is adjusted by applying transformations.
    | Sphere
    /// An abstract XZ plane. The orientation and origin of the plane is adjusted by applying transformations.
    | PlaneXZ

/// A record to hold various material properties
type Material =
    { /// The color of the material
      Color     : Color
      /// Background lighting, or light reflected from other objects in the environment.
      /// Typical values range between 0 and 1.
      Ambient   : float
      /// Light reflected from a matte surface. Typical values range between 0 and 1.
      Diffuse   : float
      /// Reflection of the light source itself and results in what is called a specular highlight,
      /// the bright spot on a curved surface. Typical values range between 0 and 1.
      Specular  : float
      /// Controls the effect of the specular reflection. The higher the shininess, the smaller and
      /// tighter the specular highlight. Typical values range between 10 (very large highlight) and
      /// 200 (very small highlight).
      Shininess : float }
    with
        static member Default = { Color     = color(1.0, 1.0, 1.0)
                                  Ambient   = 0.1
                                  Diffuse   = 0.9
                                  Specular  = 0.9
                                  Shininess = 200.0 }

/// An object is a shape with a possible transform and material assigned to it
type Object =
    { /// The shape that represents the object
      Shape     : Shape
      /// An object may have a transform that modifies its shape or not. Recall that a transform
      /// can be a combination of other transforms.
      Transform : Transform option
      /// An object may have a material that affects its appearance or not
      Material  : Material option }

/// Convenience function for a default shape object with no transform or material
let shape shape = {Shape = shape; Transform = None; Material = None}

/// Convenience value for a default sphere object with no transform or material
let sphere = shape Sphere

/// Convenience value for a default plane object with no transform or material
let plane = shape PlaneXZ

/// Computes the normal vector of the point in object space
let localNormal obj (point: Point<object>) =
    match obj.Shape with
    | Sphere  -> point - pointu<object>(0.0, 0.0, 0.0) // subtract the center of the sphere from the point on the sphere
    | PlaneXZ -> vector(0.0, 1.0, 0.0)                 // the normal of an XZ plane is constant and points in the Y direction
    |> normalize

let castWorldToObject (point: Point<world>) =
    Point<_>.mapElementwise (fun e -> (e / 1.0<world>) * 1.0<object>) point

let private _castObjectToWorld (point: Point<object>) =
    Point<_>.mapElementwise (fun e -> (e / 1.0<object>) * 1.0<world>) point

/// Calculates the surface normal of the object's shape, applying the object's transform
/// if it contains one, at the given world point
let normalAt object (worldPoint: Point<world>) =
    match object.Transform with
    | None   -> localNormal object (castWorldToObject worldPoint)                   // the object is untransformed
    | Some t -> let objectPoint = worldPoint                                        // convert from world space to object space
                                  |> applyTransform (inverse t)
                                  |> castWorldToObject
                let objectNormal = localNormal object objectPoint                   // compute the normal in object space
                let worldNormal = applyTransposedTransform (inverse t) objectNormal // convert back to world space
                normalize worldNormal                                               // return the normal normalized
