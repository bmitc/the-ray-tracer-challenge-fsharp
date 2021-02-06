module RayTracer.LightAndShading

open Tuples
open Color
open Transformation
open Ray

/// Calculates the surface normal of the object's shape, applying the object's transform
/// if it contains one, at the given world point
let normalAt object worldPoint =
    match object.Transform with
    | None   -> normalize (worldPoint - point(0.0, 0.0, 0.0))                       // the object is located at the origin
    | Some t -> let objectPoint = applyTransform (inverse t) worldPoint             // convert from world space to object space
                let objectNormal = objectPoint - point(0.0, 0.0, 0.0)               // compute the normal in object space
                let worldNormal = applyTransposedTransform (inverse t) objectNormal // convert back to world space
                normalize worldNormal                                               // return the normal normalized

/// Calculates the reflection of the vector across the normal vector
let reflect vector normal = vector - 2.0 * (dot vector normal) * normal

/// Represents a light source at a point and with an intensity
type Light =
    { Position  : Point
      Intensity : Color }

/// Calculates a color according to the Phong reflection model
let lighting material light point eyev normalv =
    let effectiveColor = material.Color * light.Intensity
    let lightv = normalize(light.Position - point)
    let ambient = material.Ambient * effectiveColor
    let lightDotNormal = dot lightv normalv
    let (diffuse, specular) =
        if lightDotNormal < 0.0
        then (color(0.0, 0.0, 0.0), color(0.0, 0.0, 0.0))
        else let d = material.Diffuse * lightDotNormal * effectiveColor
             let reflectv = reflect -lightv normalv
             let reflectDotEye = dot reflectv eyev
             if reflectDotEye <= 0.0
             then (d, color(0.0, 0.0, 0.0))
             else let factor = reflectDotEye ** material.Shininess
                  (d, material.Specular * factor * light.Intensity)
    ambient + diffuse + specular