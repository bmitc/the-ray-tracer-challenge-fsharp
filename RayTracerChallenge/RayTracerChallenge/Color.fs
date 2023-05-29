﻿/// Color type, color constants, and helper functions
module RayTracer.Color

open Utilities

/// Represents a color in terms of its red, green, and blue components.
/// A value of 1.0 means that the component is fully saturated (i.e., at 100%) and a
/// value of 0.0 means that the color component is not present (i.e., at 0%), but the
/// various values that a color component may take on are allowed to range beyond 0.0
/// and 1.0, which is for calculations.
[<StructuredFormatDisplay("rgb=({Red}, {Green}, {Blue})")>]
[<Struct; CustomEquality; NoComparison>]
type Color = { Red: float; Green: float; Blue: float } with

    /// Maps the operation to each element of the color
    static member mapElementwise op c =
        { Red   = op c.Red;
          Green = op c.Green;
          Blue  = op c.Blue }

    /// Maps the operation pairwise across two colors.
    /// For example, mapPaiwise (+) c1 c2 = {R = c1.R + c2.R; G = c1.G + c2.G; B = c1.B + c2.B},
    /// where the infix notation is only used for convenience here.
    static member mapPairwise op c1 c2 =
        { Red   = op c1.Red c2.Red;
          Green = op c1.Green c2.Green;
          Blue  = op c1.Blue c2.Blue }
    
    /// Add a constant to each element of a color
    static member ( + ) (a: float, c) = Color.mapElementwise ((+) a) c

    /// Multiply each element of a color by a constant
    static member ( * ) (a: float, c) = Color.mapElementwise ((*) a) c

    /// Divide each element of the vector by a constant
    static member ( / ) (c, a: float) = Color.mapElementwise (fun x -> x / a) c

    /// Add two colors
    static member ( + ) (c1, c2) = Color.mapPairwise (+) c1 c2

    /// Subtract two colors
    static member ( - ) (c1, c2) = Color.mapPairwise (-) c1 c2

    /// Multiply two colors, multiplying each element in one color by the corresponding
    /// positional element in the other color
    static member ( * ) (c1, c2) = Color.mapPairwise (*) c1 c2

    /// Negates each color component.
    static member ( ~- ) (c: Color) = -1.0 * c

    /// Overrides the Object.Equals method to provide a custom equality compare for Color records
    override this.Equals object =
        match object with
        | :? Color as color -> compareFloat this.Red   color.Red   &&
                               compareFloat this.Green color.Green &&
                               compareFloat this.Blue  color.Blue
        | _ -> false

    /// Overrides the Object.GetHashCode method, which is recommended when overriding Object.Equals
    override x.GetHashCode() = hash x // Re-use the built-in hash for records

/// Convenience function for creating a Color record
let inline color (r, g, b) = { Red = r; Green = g; Blue = b }

/// Computes the Hadamard (or Schur) product of the colors
let hadamardProduct c1 c2 =
    { Red   = c1.Red   * c2.Red;
      Green = c1.Green * c2.Green
      Blue  = c1.Blue  * c2.Blue }

/// Clamps a number to be in the range [min, max]
let clampNumber min max number =
    match number with
    | x when (x >= max) -> max
    | x when (x <= min) -> min
    | _                 -> number

/// Clamps a color's components to be in the range [min, max]
let clamp min max c =
    Color.mapElementwise (clampNumber min max) c

/// Blends two colors by averaging them together
let blend (color1: Color) (color2: Color) =
    (color1 + color2) / 2.0

//**********************************************************************
//***** Color constants ************************************************
//**********************************************************************

let black      = color (  0,   0,   0)
let white      = color (  1,   1,   1)
let red        = color (  1,   0,   0)
let green      = color (  0,   1,   0)
let blue       = color (  0,   0,   1)
let lightGray  = color (211, 211, 211) / 255.0
let gray       = color (128, 128, 128) / 255.0
let violet     = color (238, 130, 238) / 255.0
let skyBlue    = color (135, 206, 235) / 255.0
let paleGreen  = color (152, 251, 152) / 255.0
let purple     = color (128,   0, 128) / 255.0
let royalBlue  = color ( 65, 105, 225) / 255.0
let powderBlue = color (176, 224, 230) / 255.0
let yellow     = color (255, 255,   0) / 255.0
let pink       = color (255, 192, 203) / 255.0
let deepPink   = color (255,  20, 147) / 255.0
let hotPink    = color (255, 105, 180) / 255.0
