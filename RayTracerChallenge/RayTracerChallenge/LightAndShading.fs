module RayTracer.LightAndShading

open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Transformation
open RayTracer.Ray

let normalAt object worldPoint =
    match object.Transform with
    | None   -> normalize (worldPoint - point(0.0, 0.0, 0.0))
    | Some t -> let objectPoint = applyTransform (inverse t) worldPoint :?> Point
                let objectNormal = objectPoint - point(0.0, 0.0, 0.0)
                let worldNormal = applyTransposedTransform (inverse t) objectNormal :?> Vector
                normalize worldNormal

let reflect vector normal = vector - 2.0 * (dot vector normal) * normal

type Light = { Position : Point; Intensity : Color }

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