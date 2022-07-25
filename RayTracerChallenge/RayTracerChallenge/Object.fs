module RayTracer.Object

open Utilities
open Tuples
open Color
open Transformation

/// Represents various types of shapes to be ray traced
type Shape = Sphere

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

/// Convenience value for a default sphere object with no transform or material
let sphere = {Shape = Sphere; Transform = None; Material = None}

/// Calculates the surface normal of the object's shape, applying the object's transform
/// if it contains one, at the given world point
let normalAt object (worldPoint: Point<world>) =
    match object.Transform with
    | None   -> normalize (worldPoint - pointu<world>(0.0, 0.0, 0.0))               // the object is located at the origin
    | Some t -> let objectPoint = applyTransform (inverse t) worldPoint             // convert from world space to object space
                let objectNormal = objectPoint - pointu<world>(0.0, 0.0, 0.0)       // compute the normal in object space
                let worldNormal = applyTransposedTransform (inverse t) objectNormal // convert back to world space
                normalize worldNormal                                               // return the normal normalized
