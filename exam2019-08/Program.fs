
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



[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code