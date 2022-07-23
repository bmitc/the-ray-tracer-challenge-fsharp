/// FUnctions to create and write a canvas type to a PPM file
module RayTracer.PPM

open System.IO
open System.Text
open Utilities
open Canvas
open Color

/// Creates a PPM header string
let createHeader width height maxColorValue =
    sprintf "P3\n%d %d\n%d" width height (int maxColorValue)

/// Converts a color to a list of strings of the form "[<red>; <green>; <blue>]", where
/// the colors have been clamped to the range [0, maxColorValue] and rounded to integers.
let colorToPPMList maxColorValue (color: Color) =
    let c = clamp 0.0 maxColorValue (maxColorValue * color)
    [c.Red; c.Green; c.Blue]
    |> List.map roundToInt
    |> List.map string

/// Collapses a canvas done to a list of pixel color values, [r1; g1; b1; r2; g2; b2; ...].
/// Each row (constant y value) is collapsed to a list and the rows are processed startin
/// with (x,0) first and ending with (x,height-1).
let collapseCanvas maxColorValue (canvas: Canvas<_>) =
    [for y in 0 .. (canvas.Height - 1) do
        for pixel in canvas.[*, y] -> (colorToPPMList maxColorValue pixel)]
    |> List.concat

/// Convert a canvas to a PPM pixel string.
/// The string is limited to have a maximum width of 70 characters to ensure capatability with PPM tools.
let canvasToPPMPixels maxColorValue (c: Canvas<_>) =
    let maxWidth = 70 // Restrict the character width to 70 for PPM files
    let sb = StringBuilder("") // Use this to incrementally build the string as we recurse
    let rec helper (row: string) (count: int) (values: string list) =
        /// Checks to see if all pixels in the row have been accounted for
        let doneWithRow () = count/3 = c.Width

        /// Checks to see if the next pixel can fit on the row
        let canFit (row : string) (head : string) = row.Length + head.Length + 2 <= maxWidth // the 2 is for a space and newline
        
        match row, values with
        | _ , [] -> sb.Append("\n").Append(row).ToString()
        | "", head :: tail                                                 -> helper head (count + 1) tail                  // Starting with empty row
        | _,  head :: tail when (canFit row head) && not(doneWithRow())    -> helper (row + " " + head) (count + 1) tail    // Value can fit and not done with row
        | _,  head :: tail when (canFit row head) && (count/3 = c.Width)   -> sb.Append("\n").Append(row).Append(" ").Append(head) |> ignore
                                                                              helper "" 1 tail  // Value can fit and done with row
        | _,  head :: tail when not(canFit row head) && not(doneWithRow()) -> sb.Append("\n").Append(row) |> ignore
                                                                              helper head (count + 1) tail   // Value cannot fit and not done with row
        | _,  head :: tail when not(canFit row head) && doneWithRow()      -> sb.Append("\n").Append(row).Append("\n").Append(head) |> ignore
                                                                              helper "" 1 tail // Value cannot fit and done with row
        | _,  _                                                            -> helper row count values // Here only to satisfy pattern matching completion
    helper "" 1 (collapseCanvas maxColorValue c)

/// Converts a canvas to a PPM string, including the header and pixels
let canvasToPPM (canvas: Canvas<_>) =
    let maxColorValue = 255.0
    let header = createHeader canvas.Width canvas.Height maxColorValue
    let pixels = canvasToPPMPixels maxColorValue canvas
    sprintf "%s%s" header pixels

/// Writes the canvas to a PPM file
let writeToPPM (canvas: Canvas<_>) (filePath: string) =
    // Create the directory if needed
    Path.GetDirectoryName(filePath) |> Directory.CreateDirectory |> ignore
    use file = File.CreateText(filePath)
    file.Write (canvasToPPM canvas)