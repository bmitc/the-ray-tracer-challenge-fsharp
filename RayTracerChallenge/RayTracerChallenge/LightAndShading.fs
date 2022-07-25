/// Functions and types for calculating light and shading of shapes
module RayTracer.LightAndShading

open Tuples
open Color
open Object

/// Represents a light source at a point and with an intensity
type Light<[<Measure>] 'PointUnit> =
    { /// The location of the light's point source
      Position  : Point<'PointUnit>
      /// The brightness and color of the light
      Intensity : Color }


////////////////////////////////////
// Phong reflection model //////////
////////////////////////////////////
// Ambient reflection: background lighting, or light reflected from other objects in the environment
// Diffuse reflection: light reflected from a matte surface
// Specular reflection: reflection of the light source itself and results in what is called a specular highlight,
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
             let reflectv = Tuples.reflect -lightv normalv
             let reflectDotEye = dot reflectv eyev
             if reflectDotEye <= 0.0
             then (diffuse, color(0.0, 0.0, 0.0))
             else // Compute the specular contribution
                  let factor = reflectDotEye ** material.Shininess
                  (diffuse, material.Specular * factor * light.Intensity)

    // Return the combination of the three reflection models to get the final shading
    ambient + diffuse + specular
