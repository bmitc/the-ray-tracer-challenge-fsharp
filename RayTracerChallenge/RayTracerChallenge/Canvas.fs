module RayTracer.Canvas

open Utilities
open Color

/// Represents a 2D canvas of (x,y) pixels consisting of colors, where the origin (0,0) is at the top left,
// x increases to the right, and y increases down.
type Canvas(width: int, height: int, initialColor: Color) =
    (* To clarify the relationship between the width and height of a canvas and the dimensions
       of an array, note the following output from Array2D.create, which creates an array of
       the given dimensions.
       Array2D.create 2 3 0;;
       val it : int [,] = [[0; 0; 0]
                           [0; 0; 0]]
       Here, we consider 2 to be the height, or number of rows, and 3 to be the width, or
       number of columns. For dimensions of 2D arrays we have Array.length1, which will return
       the height (the first dimension) and Array2D.length2, which will return the width (the
       second dimension).
       *)

    /// Internal 2D array for the canvas
    let array = Array2D.create height width initialColor

    new(side) = Canvas(side, side, black)

    new(width, height) = Canvas(width, height, black)

    new(width: float, height: float) = Canvas(roundToInt width, roundToInt height, black)
    
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
        with get(x, y) = array.[y, x]
        and  set(x, y) color = array.[y, x] <- color

    member this.GetSlice(xStart: int option, xFinish: int option, yStart: int option, yFinish: int option) =
        let xStart =
            match xStart with
            | Some v  -> v
            | None    -> 0
        let xFinish =
            match xFinish with
            | Some v  -> v
            | None    -> this.Width - 1
        let yStart =
            match yStart with
            | Some v  -> v
            | None    -> 0
        let yFinish =
            match yFinish with
            | Some v  -> v
            | None    -> this.Height - 1
        array.[yStart..yFinish, xStart..xFinish] // Note that x and y are swapped when dealing directly with arrays

    member this.GetSlice(x: int, yStart: int option, yFinish: int option) =
        let yStart =
            match yStart with
            | Some v  -> v
            | None    -> 0
        let yFinish =
            match yFinish with
            | Some v  -> v
            | None    -> this.Height - 1
        array.[yStart..yFinish, x] // Note that x and y are swapped when dealing directly with arrays

    member this.GetSlice(xStart: int option, xFinish: int option, y: int) =
        let xStart =
            match xStart with
            | Some v  -> v
            | None    -> 0
        let yFinish =
            match xFinish with
            | Some v  -> v
            | None    -> this.Width - 1
        array.[y, xStart..yFinish] // Note that x and y are swapped when dealing directly with arrays

    /// Updates the canvas' pixels (via mutation) according to the given function.
    member this.UpdatePixels (f: int -> int -> Color -> Color) =
        // Note that iteri and array's first dimension is the row and second dimension is the column.
        // Thus, we need to swap those dimensions when thinking about (x,y) coordinates.
        // The function f processes (x,y) coordinates, and when we interact with arrays, these are swapped.
        Array2D.iteri (fun y x color -> (array.[y, x] <- f x y color)) array
        this