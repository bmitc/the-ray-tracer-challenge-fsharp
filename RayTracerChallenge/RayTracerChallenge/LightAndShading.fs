/// Functions and types for calculating light and shading of shapes
module RayTracer.LightAndShading

open Utilities
open Tuples
open Color
open Transformation
open Ray

/// Calculates the surface normal of the object's shape, applying the object's transform
/// if it contains one, at the given world point
let normalAt object (worldPoint: Point<world>) =
    match object.Transform with
    | None   -> normalize (worldPoint - pointu<world>(0.0, 0.0, 0.0))               // the object is located at the origin
    | Some t -> let objectPoint = applyTransform (inverse t) worldPoint             // convert from world space to object space
                let objectNormal = objectPoint - pointu<world>(0.0, 0.0, 0.0)       // compute the normal in object space
                let worldNormal = applyTransposedTransform (inverse t) objectNormal // convert back to world space
                normalize worldNormal                                               // return the normal normalized

/// Calculates the reflection of the vector across the normal vector
let reflect vector normal = vector - 2.0 * (dot vector normal) * normal

/// Represents a light source at a point and with an intensity
type Light<[<Measure>] 'PointUnit> =
    { Position  : Point<'PointUnit>
      Intensity : Color }

////////////////////////////////////
// Phong reflection model //////////
////////////////////////////////////
// Ambient reflection: background lighting, or light reflected from other objects in the environment
// Diffuse reflection: light reflected from a matte surface
// Specular reflection: reflectin of the light source itself and results in what is called a specular highlight,
//                      the bright spot on a curved surface

/// Calculates a color according to the Phong reflection model
let lighting material light (point: Point<_>) eyev normalv inShadow =
    // Combine the surface color with the light's color/intensity
    let effectiveColor = material.Color * light.Intensity

    // Find the direction to the light source
    let lightv = normalize(light.Position - point)

    // Compute the ambient contribution
    let ambient = material.Ambient * effectiveColor

    // Represents the cosine of the angle between the light vector and the normal vector.
    // A negative number means the light is on the other side of the surface.
    let lightDotNormal = dot lightv normalv

    let (diffuse, specular) =
        // If the lightDotNormal is negative or surface is inShadow, then both
        // diffuse and specular make no contributions to the surface's color
        if lightDotNormal < 0.0 || inShadow
        then (color(0.0, 0.0, 0.0), color(0.0, 0.0, 0.0))
        else // Compute the diffuse contribution
             let diffuse = material.Diffuse * lightDotNormal * effectiveColor

             // reflectDotEye represents the cosine of the angle between the reflection
             // vector and the eye vector. A negative number means the light reflects
             // away from the eye.
             let reflectv = reflect -lightv normalv
             let reflectDotEye = dot reflectv eyev
             if reflectDotEye <= 0.0
             then (diffuse, color(0.0, 0.0, 0.0))
             else // Compute the specular contribution
                  let factor = reflectDotEye ** material.Shininess
                  (diffuse, material.Specular * factor * light.Intensity)

    // Return the combination of the three reflection models to get the final shading
    ambient + diffuse + specular