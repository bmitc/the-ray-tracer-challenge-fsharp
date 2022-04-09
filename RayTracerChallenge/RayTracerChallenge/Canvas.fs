/// Provides a canvas type that contains color pixels at (x,y)-coordinates
module RayTracer.Canvas

open Utilities
open Color

/// Represents a 2D canvas of (x,y) pixels consisting of colors, where the origin (0,0) is at the top left,
// x increases to the right, and y increases down.
type Canvas(width: int, height: int, initialColor: Color) =
    /// Internal 1D array for the canvas.
    /// A 1D array is used instead of a 2D array, which was the original implementation, to get
    /// access to better higher-order functions in the Array module and to take advantage of
    /// the parallelized functions in the Array.Parallel module.
    let array = Array.create (width * height) initialColor

    /// Create a square canvas
    new(side) = Canvas(side, side, black)

    /// Create a rectangular canvas with every pixel initialized to black
    new(width, height) = Canvas(width, height, black)

    /// Create a rectangular canvas with float sides, which are rounded to the nearest integers,
    /// with every pixel initialized to black
    new(width, height) = Canvas(roundToInt width, roundToInt height, black)
    
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