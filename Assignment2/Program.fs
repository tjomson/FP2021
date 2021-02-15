open System

// Exercise 2.1
let downto1 n = 
    if n = 0 then []
    else [ for i in 1 .. n -> n - i + 1 ]
        
let downto2 = function
    | 0 -> []
    | n -> [ for i in 2 .. n -> n - i + 2 ]

// Exercise 2.2
let removeOddIdx xs =
    let rec concat (xs: List<'a>) (ys: List<'a>) n =
        match n with 
        | -1 -> ys
        | n when n % 2 = 0 -> concat xs (xs.[n]::ys) (n - 1)
        | _ -> concat xs ys (n - 1)
    concat xs [] (xs.Length - 1)

// Exercise 2.3
let combinePair = function
    | (x:List<'a>) when x.Length % 2 = 0 -> [for i in 0 .. 2 .. x.Length - 1 -> (x.[i],x.[i + 1])]
    | x -> [for i in 0 .. 2 .. x.Length - 2 -> (x.[i],x.[i + 1])]

// Exercise 2.4
type complex = float * float

let mkComplex x y = complex(x,y)

let complexToPair (c: complex) = (fst(c), snd(c))

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code