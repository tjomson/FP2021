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
    xs -> [ for i in 0 .. ([]@xs).Length - 1 ->  (xs.[i], xs.[i+1]) ]

let combinePair2 = function
    | 


[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code