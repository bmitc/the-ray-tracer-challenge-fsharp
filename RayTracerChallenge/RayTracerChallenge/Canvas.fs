module RayTracer.Canvas

open RayTracer.Color

(* 
   To clarify the relationship between the width and height of a canvas and the dimensions
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

/// Represents a canvas, which is a 2D array of colors (i.e., pixels). A canvas of height n
/// and width m is represented by an n x m array or matrix.
type Canvas = Canvas of Color [,] with

    /// The width of the canvas, which is the number of columns of the underlying array/matrix.
    member this.width =
        match this with
        | Canvas array -> Array2D.length2 array
    
    /// The height of the canvas, which is the number of rows of the underlying array/matrix.
    member this.height =
        match this with
        | Canvas array -> Array2D.length1 array

/// Creates a canvas of the given width and height. The underlying structure will be
/// a height x width array/matrix.
let canvas (width, height) = Canvas (Array2D.create height width (color(0.0, 0.0, 0.0)))

(* Note on canvas positions:
   If we place an (x,y) coordiante value at every position in a 2x3 canvas' array, we have
   [[(0,0); (1,0); (2,0)]
    [(0,1); (1,1); (2,1)]]
   So x determines the column index and y determines the row index. Note also that the coordinate
   origin is at the top-left of the array and that x and y increase as we move to the right and
   down, respectively.
*)

/// Reads the color pixel at the position (x, y) (column index, row index).
let readPixel (Canvas colorArray) (x, y) = colorArray.[y,x]

/// Write's a color pixel at the position (x, y) (column index, row index).
/// This returns a new canvas and does not modify the canvas given.
let writePixel (x, y) color (c : Canvas) =
    if (0 <= x) && (x <= c.width) && (0 <= y) && (y <= c.height)
    then let (Canvas colorArray) = c
         let colorArray = Array2D.copy colorArray
         colorArray.[y,x] <- color
         Canvas colorArray
    else c

/// Write's a color pixel at the position (x, y) (column index, row index).
/// This returns a new canvas and does not modify the canvas given.
let writePixelMut (x, y) color (c : Canvas) =
    if (0 <= x) && (x <= c.width) && (0 <= y) && (y <= c.height)
    then let (Canvas colorArray) = c
         colorArray.[y,x] <- color
         Canvas colorArray
    else c

let map func (Canvas colorArray) = Canvas (Array2D.map func colorArray)

let mapxy (func : int -> int -> Color -> Color) (Canvas colorArray) =
    Canvas (Array2D.mapi (fun y x clr -> func x y clr) colorArray)

let iterxy (func : int -> int -> Color -> unit) (Canvas colorArray) =
    Array2D.iteri (fun y x clr -> func x y clr) colorArray