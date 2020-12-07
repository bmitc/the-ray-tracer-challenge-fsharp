module RayTracer.Color

open RayTracer.Utilities

/// Record representing a color as red, green, blue components.
[<StructuredFormatDisplay("rgb=({Red}, {Green}, {Blue})")>]
[<CustomEquality; CustomComparison>]
type Color = {Red : float; Green : float; Blue : float} with

    static member mapElementwise op c =
        {Red = op c.Red;
         Green = op c.Green;
         Blue = op c.Blue}

    static member mapPairwise op c1 c2 =
        {Red   = op c1.Red c2.Red;
         Green = op c1.Green c2.Green;
         Blue  = op c1.Blue c2.Blue}
    
    static member (+) (a:float, c) = Color.mapElementwise ((+) a) c
    static member (*) (a:float, c) = Color.mapElementwise ((*) a) c
    //static member (./) (c, a:float) = Color.mapElementwise (fun x -> x / a) c

    static member (+) (c1, c2) = Color.mapPairwise (+) c1 c2
    static member (-) (c1, c2) = Color.mapPairwise (-) c1 c2
    static member (*) (c1, c2) = Color.mapPairwise (*) c1 c2

    /// Negates each color component.
    static member (~-) (c : Color) = -1.0 * c

    /// Overrides the Object.Equals method to provide a custom equality compare for Color records
    override x.Equals object =
        match object with
        | :? Color as color -> compareFloat x.Red color.Red &&
                               compareFloat x.Green color.Green &&
                               compareFloat x.Blue color.Blue
        | _ -> false

    override x.GetHashCode() = hash x

    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? Color as c -> [x.Red.CompareTo c.Red; x.Green.CompareTo c.Green; x.Blue.CompareTo c.Blue]
                               |> List.tryFind (fun result -> result <> 0)
                               |> function
                                   | Some x -> x
                                   | None   -> 0
            | _ -> 0

/// Constructs a Color record.
let color (r,g,b) = {Red=r; Green=g; Blue=b}

let black = color(0.0, 0.0, 0.0)
let white = color(1.0, 1.0, 1.0)
let red   = color(1.0, 0.0, 0.0)
let green = color(0.0, 1.0, 1.0)
let blue  = color(0.0, 0.0, 1.0)

/// Computes the Hadamard (or Schur) product of the color.
let hadamardProduct c1 c2 =
    {Red   = c1.Red   * c2.Red;
     Green = c1.Green * c2.Green
     Blue  = c1.Blue  * c2.Blue}

/// Clamps a number to be in the range [min, max].
let clampNumber min max num =
    match num with
    | x when (x >= max) -> max
    | x when (x <= min) -> min
    | _                 -> num

/// Clamps a color's components to be in the range [min, max].
let clamp min max c =
    Color.mapElementwise (clampNumber min max) c