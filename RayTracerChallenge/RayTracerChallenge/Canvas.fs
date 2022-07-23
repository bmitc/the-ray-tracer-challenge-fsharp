/// Provides a canvas type that contains color pixels at (x,y)-coordinates
module RayTracer.Canvas

open Utilities
open Color

/// Private function to be used to process float<_> values in Canvas constructors.
/// Converts float<_> to int<pixels> by rounding to the nearest integer.
let private processFloat (x: float<_>) = x |> removeUnits |> roundToInt |> castIntUnit<pixels>

/// Private function to be used to process int<_> values in Canvas constructors.
/// Converts float<_> to int<pixels>.
let private processInt (x: int<_>) = x |> int |> castIntUnit<pixels>

(* Note: This is an interesting design challenge, designing the Canvas class and its constructors
         so as to communicate that a canvas represents a 2D grid of pixels. There are three main
         choices: (1) Force the construction of a canvas to use either int<pixels> or float<pixels>,
         converting non-pixels units int and float numbers to pixels units, (2) Allow int or float
         constructor arguments that have no units of measure (there is no way, even using generic
         units in the new type signature to allow generic units of measure which are then thrown
         away), or (3) Define a generic constructor units type variable as a type argument to the
         class itself, which then allows the constructors to take int, float, int<arbitrary>, or
         float<arbitrary> arguments.

         Option (3) was chosen because it allows the most flexibility and least amount of boilerplate.
         For example, tests can simply use Canvas(100, 200) to construct, while scripts might want
         to use Canvas(100<pixels>, 200<pixels>) to make the units of the canvas clear or even be
         derived from other defined bindings.
*)

/// Represents a 2D canvas of (x,y) pixels consisting of colors, where the origin (0,0) is at the top left,
/// x increases to the right, and y increases down. Any units provided to constructor arguments are normalized
/// to pixels, with no conversion multiplier used. Only the units are replaced. Thus, the units of the
/// (x,y)-coordinates should always be thought of pixels. Flexibility in the constructor arguments are there
/// for ease of use and to remove boilerplate when desired.
type Canvas<[<Measure>] 'GenericConstructorUnits>(width: int<pixels>, height: int<pixels>, initialColor: Color) =
    // Remove the units of measure so that they don't have to be dealt with internally
    // We keep the units in the primary constructor, even though they are immediately thrown away
    // to simply communicate that a canvas is a 2D grid of pixels.

    /// Width of the canvas, implicitly in pixels units
    let width = int width

    /// Height of the canvas, implicitly in pixels units
    let height = int height

    /// Internal 1D array for the canvas.
    /// A 1D array is used instead of a 2D array, which was the original implementation, to get
    /// access to better higher-order functions in the Array module and to take advantage of
    /// the parallelized functions in the Array.Parallel module.
    let array = Array.create (width * height) initialColor

    /// Create a square canvas with every pixel initialized to black
    new(side: int<'GenericConstructorUnits>) = Canvas(processInt side, processInt side, black)

    /// Create a square canvas with a float<pixels> side, which is rounded to the nearest
    /// integer int<pixels>, and every pixel initialized to black
    new(side: float<'GenericConstructorUnits>) = Canvas(processFloat side, processFloat side, black)

    /// Create a rectangular canvas with every pixel initialized to black
    new(width: int<'GenericConstructorUnits>, height: int<'GenericConstructorUnits>) = Canvas(processInt width, processInt height, black)

    /// Create a rectangular canvas with float<pixels> sides, which are rounded to the nearest
    /// integers int<pixels>, and every pixel initialized to black
    new(width: float<'GenericConstructorUnits>, height: float<'GenericConstructorUnits>) = Canvas(processFloat width, processFloat height, black)
    
    /// The width of the canvas, in pixels
    member _.Width = width

    /// The height of the canvas, in pixels
    member _.Height = height

    (* Note on canvas positions:
       If we place an (x,y) coordinate value at every position in a 2x3 canvas' array, we have
       [[(0,0); (1,0); (2,0)]
        [(0,1); (1,1); (2,1)]]
       So x determines the column index and y determines the row index. Note also that the coordinate
       origin is at the top-left of the array and that x and y increase as we move to the right and
       down, respectively.
    *)

    /// Allows indexing notation.
    /// If c is a Canvas, then c.[x,y] retrieves the (x,y)-coordinate of the canvas
    /// and c.[x,y] <- color sets the (x,y)-coordinate to the given color. The latter
    /// mutates the canvas.
    member _.Item
        with get(x, y) = array.[convert2DIndexTo1DIndex x y width]
        and  set(x, y) color = array.[convert2DIndexTo1DIndex x y width] <- color

    // The following are slicing extensions, which allows using F#'s slicing notation on Canvas objects
    // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/slices

    member _.GetSlice(xStart: int option, xFinish: int option, yStart: int option, yFinish: int option) =
        let xStart =
            match xStart with
            | Some v  -> v
            | None    -> 0
        let xFinish =
            match xFinish with
            | Some v  -> v
            | None    -> width - 1
        let yStart =
            match yStart with
            | Some v  -> v
            | None    -> 0
        let yFinish =
            match yFinish with
            | Some v  -> v
            | None    -> height - 1
        let start = convert2DIndexTo1DIndex xStart yStart width
        let finish = convert2DIndexTo1DIndex xFinish yFinish width
        array.[start..finish]

    member _.GetSlice(x: int, yStart: int option, yFinish: int option) =
        let yStart =
            match yStart with
            | Some v  -> v
            | None    -> 0
        let yFinish =
            match yFinish with
            | Some v  -> v
            | None    -> height - 1
        let start = convert2DIndexTo1DIndex x yStart width
        let finish = convert2DIndexTo1DIndex x yFinish width
        array.[start..finish]

    member _.GetSlice(xStart: int option, xFinish: int option, y: int) =
        let xStart =
            match xStart with
            | Some v  -> v
            | None    -> 0
        let xFinish =
            match xFinish with
            | Some v  -> v
            | None    -> width - 1
        let start = convert2DIndexTo1DIndex xStart y width
        let finish = convert2DIndexTo1DIndex xFinish y width
        array.[start..finish]

    /// Updates the canvas' pixels (via mutation and in parallel) according to the given function.
    /// The update function is f: (x: int) -> (y: int) -> (existingColor: Color) -> (newColor: Color)
    member this.UpdatePixels (f: int -> int -> Color -> Color) =
        Array.Parallel.iteri (fun i color -> let (x, y) = convert1DIndexTo2DIndex i width
                                             (this.[x, y] <- f x y color)) array
        this