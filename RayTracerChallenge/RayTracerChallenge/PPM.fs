module RayTracer.PPM

open System
open System.IO
open RayTracer.Canvas
open RayTracer.Color
open System.Text

let createHeader width height maxColorValue =
    sprintf "P3\n%d %d\n%d" width height (int maxColorValue)

let colorToPPMString maxColorValue (clr : Color) =
    let c = clamp 0.0 maxColorValue (maxColorValue * clr)
    let round (x : float) = int(Math.Round x)
    sprintf "%d %d %d" (round c.Red) (round c.Green) (round c.Blue)

let colorToPPMList maxColorValue (clr : Color) =
    let c = clamp 0.0 maxColorValue (maxColorValue * clr)
    let round (x : float) = int(Math.Round x)
    [string (round c.Red); string (round c.Green); string (round c.Blue)]

/// Converts a canvas to a list of strings of the form "<red> <green> <blue>", where
/// the colors have been clamped to the range [0,maxColorValue] and rounded to integers.
let canvasToColorStrings maxColorValue (c : Canvas) =
    let (Canvas colorArray) = c
    [for rowIndex in 0 .. (c.height - 1) do
        for pixel in colorArray.[rowIndex, *] -> (colorToPPMString maxColorValue pixel)]

/// Collapses a canvas done to a list of pixel component values, [r1; g1; b1; r2; g2; b2; ...].
let collapseCanvas maxColorValue (c : Canvas) =
    let (Canvas colorArray) = c
    [for rowIndex in 0 .. (c.height - 1) do
        for pixel in colorArray.[rowIndex, *] -> (colorToPPMList maxColorValue pixel)]
    |> List.concat

let canvasToPixelsOld maxColorValue (c : Canvas) =
    let maxWidth = 70 // Restrict the character width to 70 for PPM files
    let rec helper (pixels : string) (row : string) (count : int) (values : string list) =
        let doneWithRow () = count/3 = c.width
        let canFit (row : string) (head : string) = row.Length + head.Length + 2 <= maxWidth
        match row, values with
        | _ , [] -> sprintf "%s\n%s" pixels row
        | "", head :: tail                                                 -> helper pixels head (count + 1) tail                  // Starting with empty row
        | _,  head :: tail when (canFit row head) && not(doneWithRow())    -> helper pixels (row + " " + head) (count + 1) tail    // Value can fit and not done with row
        | _,  head :: tail when (canFit row head) && (count/3 = c.width)   -> helper (pixels + "\n" + row + " " + head) "" 1 tail  // Value can fit and done with row
        | _,  head :: tail when not(canFit row head) && not(doneWithRow()) -> helper (pixels + "\n" + row) head (count + 1) tail   // Value cannot fit and not done with row
        | _,  head :: tail when not(canFit row head) && doneWithRow()      -> helper (pixels + "\n" + row + "\n" + head) "" 1 tail // Value cannot fit and done with row
        | _,  _                                                            -> helper pixels row count values // Here only to satisfy pattern matching completion
    helper "" "" 1 (collapseCanvas maxColorValue c)

let canvasToPixels maxColorValue (c : Canvas) =
    let maxWidth = 70 // Restrict the character width to 70 for PPM files
    let sb = StringBuilder("")
    let rec helper (row : string) (count : int) (values : string list) =
        let doneWithRow () = count/3 = c.width
        let canFit (row : string) (head : string) = row.Length + head.Length + 2 <= maxWidth
        match row, values with
        | _ , [] -> sb.Append("\n").Append(row).ToString()
        | "", head :: tail                                                 -> helper head (count + 1) tail                  // Starting with empty row
        | _,  head :: tail when (canFit row head) && not(doneWithRow())    -> helper (row + " " + head) (count + 1) tail    // Value can fit and not done with row
        | _,  head :: tail when (canFit row head) && (count/3 = c.width)   -> sb.Append("\n").Append(row).Append(" ").Append(head) |> ignore
                                                                              helper "" 1 tail  // Value can fit and done with row
        | _,  head :: tail when not(canFit row head) && not(doneWithRow()) -> sb.Append("\n").Append(row) |> ignore
                                                                              helper head (count + 1) tail   // Value cannot fit and not done with row
        | _,  head :: tail when not(canFit row head) && doneWithRow()      -> sb.Append("\n").Append(row).Append("\n").Append(head) |> ignore
                                                                              helper "" 1 tail // Value cannot fit and done with row
        | _,  _                                                            -> helper row count values // Here only to satisfy pattern matching completion
    helper "" 1 (collapseCanvas maxColorValue c)

let canvasToPPM (c : Canvas) =
    let maxColorValue = 255.0
    let header = createHeader c.width c.height maxColorValue
    let pixels = canvasToPixels maxColorValue c
    sprintf "%s%s" header pixels

let writeToPPM (c : Canvas) (filePath : string) =
    let directory = Path.GetDirectoryName(filePath)
    do Directory.CreateDirectory(directory) |> ignore
    let file = File.CreateText(filePath)
    file.Write (canvasToPPM c)
    file.Close()