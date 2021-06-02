
open System

// 1.1
type Sum<'a,'b> =
| Left of 'a
| Right of 'b

let v1: Sum<int list, bool option> = Left [1;2;3]
let v2: Sum<int list, bool option> = Right (Some(true))

let sumMap f g s = 
    match s with
    | Left x -> f x
    | Right y -> g y

// 1.2
type SumColl<'a, 'b> =
| Nil
| CLeft of 'a * SumColl<'a, 'b>
| CRight of 'b * SumColl<'a, 'b>

let a1: SumColl<bool list, int> = Nil
let a2: SumColl<bool list, int> = CLeft ([true], Nil)
let a3: SumColl<bool list, int> = CRight (3, Nil)

let rec ofList (sum: Sum<'a, 'b> list) = 
    match sum with
    | head :: tail -> 
        match head with 
        | Left a -> CLeft (a, ofList tail)
        | Right b -> CRight (b, ofList tail)
    | _ -> Nil

// 1.3
let reverse sumColl =
    let rec aux coll acc =
        match coll with 
        | CLeft (a, c) -> aux c (CLeft (a, acc))
        | CRight (b, c) -> aux c (CRight (b, acc))
        | Nil -> acc
    aux sumColl Nil

// 1.4
let ofList2 (sum: Sum<'a, 'b> list) =
    List.foldBack (fun sum acc -> 
        match sum with
        | Left a -> CLeft (a, acc)
        | Right b -> CRight (b, acc)
    ) sum Nil

// 1.5
let rec foldBackSumColl (f: 'a -> 'c -> 'c) (g: 'b -> 'c -> 'c) (sum: SumColl<'a, 'b>) (state: 'c) =
    match sum with
    | CLeft (a, x) -> f a (foldBackSumColl f g x state)
    | CRight (b, x) -> g b (foldBackSumColl f g x state)
    | Nil -> state


// 2.1
let f s =
    let l = String.length s
    let rec aux =
        function
        | i when i = l -> []
        | i -> s.[i] :: aux (i + 1)

    aux 0

let g s = 
    s |> f |>
    List.filter System.Char.IsLetter |>
    List.map System.Char.ToLower |>
    fun lst -> lst = List.rev lst

// f is of type string -> list<char>
// g is of type string -> bool
// f could be called toCharArray
// g could be called isMirrorable, as it checks whether the letters are the same if read from both left or right

// 2.2
let f2 s = [for i in 0 .. (String.length s - 1) do yield s.[i]]

// 2.3
let g2 s =
    (f >>
        (List.filter System.Char.IsLetter) >>
        (List.map System.Char.ToLower) >>
        (fun lst -> lst = List.rev lst)
    ) s

// 2.4
// With each call, it will append the result of the aux-function to the list, 
// but the result of this will not be available until the end of the list has been reached.
// To fix this, the list could be built on an accumulator, so there is only one recursive call in the stack.

let fTail s =
    let l = String.length s
    let rec aux acc =
        function
        | i when i = -1 -> acc
        | i -> aux (s.[i] :: acc) (i - 1)

    aux [] (l - 1)

// 2.5
let gOpt (s: string) =
    let endIndex = s.Length - 1
    let rec aux left right =
        if (left >= right) then
            true
        elif (s.[left] |> System.Char.IsLetter |> not) then
            aux (left + 1) (right)
        elif (s.[right] |> System.Char.IsLetter |> not) then
            aux (left) (right - 1)
        elif ((s.[left] |> System.Char.ToLower) = (s.[right] |> System.Char.ToLower)) then
            aux (left + 1) (right - 1)
        else
            false

    aux 0 endIndex

// 3.1
let calculateGoldenRatio i =
    let rec aux x number1 number2 =
        if (x = i) then
            number2 / number1
        else 
            aux (x + 1) (number2) (number1 + number2)
    aux 0 1.0 1.0

// 3.2
let grSeq1 = calculateGoldenRatio |> Seq.initInfinite |> Seq.cache;;

let grSeq = 
    (0.0, 1.0)
    |> Seq.unfold (fun x -> 
        Some ((fst x + snd x) / snd x , (snd x, (fst x + snd x)))
    )

// 3.3
let goldenRectangleSeq x =
    Seq.map (fun a -> (x * a) * x) grSeq

let goldenTriangleSeq x =
    Seq.map (fun b -> 
        let height = x * System.Math.Sqrt (b * b - 0.25)
        x * height * 0.5 
    ) grSeq

    
// 3.4
let goldenRectangleTriangle x = 
    let rec aux recSeq triSeq =
        seq {
            yield (Seq.head recSeq, Seq.head triSeq)
            yield! aux (Seq.tail recSeq) (Seq.tail triSeq)
        }
    aux (goldenRectangleSeq x) (goldenTriangleSeq x) 
    
// 4.1
module Question4Solution =
    type Color = 
    | Red
    | Green
    | Blue
    | Purple
    | Orange
    | Yellow

    type Shape = 
    | Square
    | Circle
    | Star
    | Diamond
    | Club
    | Cross

    type tile = Color * Shape

    let stringToColor = Map.ofList [("red", Red); ("green", Green); ("blue", Blue); ("purple", Purple); ("orange", Orange); ("yellow", Yellow);]
    let stringToShape = Map.ofList [("square", Square); ("circle", Circle); ("star", Star); ("diamond", Diamond); ("club", Club); ("cross", Cross);]

    let mkTile color shape = tile (stringToColor.[color], stringToShape.[shape])

    let tileToString ((color, shape): tile) = (color.ToString().ToLower()) + " " + (shape.ToString().ToLower())

    // 4.2
    let validTiles (list: tile list) (tile: tile) =
        List.map (fun x -> 
            let color = fst x
            let shape = snd x
            if (color = (tile |> fst)) then
                if (shape = (tile |> snd)) then
                    false
                else
                    true
            else
                if (shape = (tile |> snd)) then
                    true
                else
                    false
        ) list
        |> List.contains false
        |> not

    // 4.3
    type coord = Coord of int * int
    type board = Board of Map<coord, tile>
    type direction =
    | Left
    | Right
    | Up
    | Down

    let moveCoord (Coord(x,y)) (dir: direction) =
        match dir with
        | Left -> Coord(x - 1, y)
        | Right -> Coord(x + 1, y)
        | Up -> Coord(x, y - 1)
        | Down -> Coord(x, y + 1)

    let collectTiles (Board(map): board) coord (dir: direction) =
        let rec aux (c: coord) acc =
            match (Map.tryFind c map) with
            | Some x -> aux (moveCoord c dir) (x :: acc)
            | None -> acc
        aux coord []
        |> List.rev
        
    // 4.4
    let placeTile (c, t) (Board(map)) =
        let collectedL = collectTiles (Board(map)) c Left
        let collectedR = collectTiles (Board(map)) c Right
        let collectedU = collectTiles (Board(map)) c Up
        let collectedD = collectTiles (Board(map)) c Down
        let collectedHori = collectedL @ collectedR
        let collectedVert = collectedU @ collectedD
        if (validTiles collectedHori t && validTiles collectedVert t) then
            Some (Board(Map.add c t map))
        else
            None

        

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code