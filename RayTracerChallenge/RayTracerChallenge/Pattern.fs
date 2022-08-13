/// Pattern type and functions to apply a pattern's coloring scheme
module RayTracer.Pattern

open Utilities
open Tuples
open Perlin
open Color
open Transformation

/// Represents a pair of colors
[<Struct>]
type ColorPair =
    { ColorA: Color
      ColorB: Color }

/// Convenience function for creating a color pair
let colorPair colorA colorB = {ColorA = colorA; ColorB = colorB}

/// Represents a type of pattern
type Pattern =
    /// A striping pattern that alternates every other integral part in the X-coordinate
    | Stripe of ColorPair * Transform option
    /// A gradient pattern that linearly interpolates from one color to another in the X-coordinate
    | Gradient of ColorPair * Transform option
    /// A ring pattern that radially alternates every other integral radius value in the XZ-plane
    | Ring of ColorPair * Transform option
    /// A checker pattern that alternates between colors in a square checkered pattern
    | Checker of ColorPair * Transform option
    /// A blend pattern that blends two different patterns together by averaging their colors
    | Blend of Pattern * Pattern
    /// A perturb pattern that perturbates another pattern using Perlin noise
    | Perturb of Pattern

    with

    /// Get the singular transform for the pattern. If there is no singular transform, then this
    /// returns None, and the transforms that the pattern contains, such as in the Blend case,
    /// must be handled differently.
    member this.Transform =
        match this with
        | Stripe (_, transform)   -> transform
        | Gradient (_, transform) -> transform
        | Ring (_, ransform)      -> ransform
        | Checker (_, transform)  -> transform
        | Blend (_, _)            -> None
        | Perturb pattern         -> pattern.Transform

/// Convenience function for building a stripe pattern
let stripe colorA colorB transform = Stripe (colorPair colorA colorB, transform)

/// Convenience function for building a gradient pattern
let gradient colorA colorB transform = Gradient (colorPair colorA colorB, transform)

/// Convenience function for building a ring pattern
let ring colorA colorB transform = Ring (colorPair colorA colorB, transform)

/// Convenience function for building a ring pattern
let checker colorA colorB transform = Checker (colorPair colorA colorB, transform)

/// Gets the color for the given pattern components and a point
let rec patternColor pattern (point: Point<pattern>) =
    let p = convertPointUnits<pattern, 1> point
    match pattern with
    | Stripe (colorPair, _)   -> match floorInt(p.X) % 2 with                       // alternates between ColorA and ColorB accoring to the x-coordinate
                                 | 0 -> colorPair.ColorA
                                 | _ -> colorPair.ColorB
    | Gradient (colorPair, _) -> let distance = colorPair.ColorB - colorPair.ColorA // gets the "length" of the color interval
                                 let fraction = p.X - floor(p.X)                    // gets the fractional (i.e., decimal) part of the x coordinate
                                 fraction * distance + colorPair.ColorA             // interpolates between ColorA and ColorB over a unit interval on the X axis
    | Ring (colorPair, _)     -> let radiusXZ = sqrt(p.X * p.X + p.Z * p.Z)         // radius in XZ plane
                                 match floorInt(radiusXZ) % 2 with                  // alternate between ColorA and ColorB according to the radius
                                 | 0 -> colorPair.ColorA
                                 | _ -> colorPair.ColorB
    | Checker (colorPair, _)  -> let amplitude = floor(p.X) + floor(p.Y) + floor(p.Z)
                                 match floorInt(amplitude) % 2 with
                                 | 0 -> colorPair.ColorA
                                 | _ -> colorPair.ColorB
    | Blend (p1, p2)          -> blend (patternColor p1 point) (patternColor p2 point)
    | Perturb pattern         -> patternColor pattern (perturbPoint point)

/// Transform a point using a pattern's transform, if it has one.
/// Note that this is not intended to be used on Blend patterns or Perturnbed patterns
/// that a perturbing a Blend pattern.
let private patternTransformPoint (pattern: Pattern) (point: Point<object>) =
    match pattern.Transform with
    | Some transform -> point
                        |> applyTransform (inverse transform)
    | None           -> point
    |> convertPointUnits<object, pattern>

/// Applies a pattern to a given unitless point
let rec patternAt pattern (point: Point<object>) =
    match pattern with
    // Blend must be handled differently since it has two patterns, each with their own transform
    | Blend (p1, p2) ->
        let point1 = patternTransformPoint p1 point
        let point2 = patternTransformPoint p2 point
        blend (patternColor p1 point1) (patternColor p2 point2)
    // For perturb patterns that contain a blended pattern, we need to blend the individually
    // perturbed patterns
    | Perturb (Blend (p1, p2)) ->
        blend (patternAt (Perturb p1) point) (patternAt (Perturb p2) point)
    // Every other pattern can be handled the same
    | pattern ->
        match pattern.Transform with
        | Some t -> point
                    |> applyTransform (inverse t)
                    |> convertPointUnits<object, pattern>
                    |> patternColor pattern
        | None   -> point
                    |> convertPointUnits<object, pattern>
                    |> patternColor pattern
