/// Pattern type and functions to apply a pattern's coloring scheme
module RayTracer.Pattern

open Utilities
open Tuples
open Color
open Transformation

/// Represents a pair of colors
type ColorPair =
    { ColorA: Color
      ColorB: Color }

/// Represents a type of pattern
type PatternType =
    /// A striping pattern that alternates every other integral part in the X-coordinate
    | Stripe
    /// A gradient pattern that linearly interpolates from one color to another in the X-coordinate
    | Gradient
    /// A ring pattern that radially alternates every other integral radius value in the XZ-plane
    | Ring
    /// A checker pattern that alternates between colors in a square checkered pattern
    | Checker
    //| Peturb of PatternType
    //| Blend of PatternType * PatternType

/// Represents a pattern used to generate a certain type of color pattern
type Pattern =
    { Type      : PatternType
      ColorPair : ColorPair
      Transform : Transform option }

    with
        
    /// Gets the pattern's A color
    member this.ColorA = this.ColorPair.ColorA

    /// Gets the pattern's B color
    member this.ColorB = this.ColorPair.ColorB

/// Convenience function for creating a pattern
let pattern patternType colorPair transform = { Type = patternType; ColorPair = colorPair; Transform = transform }

let private patternCreator patternType colorA colorB transform =
    pattern patternType {ColorA = colorA; ColorB = colorB} transform

/// Convenience function for building a stripe pattern
let stripe = patternCreator Stripe

/// Convenience function for building a gradient pattern
let gradient = patternCreator Gradient

/// Convenience function for building a ring pattern
let ring = patternCreator Ring

/// Convenience function for building a ring pattern
let checker = patternCreator Checker

//let blended (pattern1: PatternType, pattern2: PatternType) = {Type = Blend (pattern1, pattern2); ColorPair = {ColorA = black; ColorB = black}; Transform = None}

/// Gets the color for the given pattern components and a point
let rec patternColor patternType colorPair (point: Point<'Unit>) =
    let p = convertPointUnits<'Unit, 1> point
    match patternType with
    | Stripe   -> match floorInt(p.X) % 2 with                       // alternates between ColorA and ColorB accoring to the x-coordinate
                  | 0 -> colorPair.ColorA
                  | _ -> colorPair.ColorB
    | Gradient -> let distance = colorPair.ColorB - colorPair.ColorA // gets the "length" of the color interval
                  let fraction = p.X - floor(p.X)                    // gets the fractional (i.e., decimal) part of the x coordinate
                  fraction * distance + colorPair.ColorA             // interpolates between ColorA and ColorB over a unit interval on the X axis
    | Ring     -> let radiusXZ = sqrt(p.X * p.X + p.Z * p.Z)         // radius in XZ plane
                  match floorInt(radiusXZ) % 2 with                  // alternate between ColorA and ColorB according to the radius
                  | 0 -> colorPair.ColorA
                  | _ -> colorPair.ColorB
    | Checker  -> let amplitude = floor(p.X) + floor(p.Y) + floor(p.Z)
                  match floorInt(amplitude) % 2 with
                  | 0 -> colorPair.ColorA
                  | _ -> colorPair.ColorB
    //| Peturb p -> patternColor p colorPair point
    //| Blend (p1, p2) -> blend (patternColor p1 colorPair point) (patternColor p2 colorPair point)

/// Applies a pattern to a given unitless point
let patternAt pattern point =
    patternColor pattern.Type pattern.ColorPair point
