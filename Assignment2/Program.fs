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
type complex = {fst: float; snd: float}

let mkComplex x y = {fst = x; snd = y}

let complexToPair c = (c.fst, c.snd)

// Exercise 2.5
let (.+.) x y = {fst = x.fst + y.fst; snd = x.snd + y.snd}

let (.*.) x y = {fst = x.fst * y.fst - x.snd * y.snd; snd = x.snd * y.fst + x.fst * y.snd}

// Exercise 2.6
let negate x = {fst = -x.fst; snd = -x.snd}
let (.-.) x y = x .+. negate(y)

let invert x = {fst = x.fst / (x.fst**2.0 + x.snd**2.0); snd = -x.snd / (x.fst**2.0 + x.snd**2.0)}
let (./.) x y = x .*. invert(y)

let c1 = {fst = 2.0; snd = 3.0}
let c2 = {fst = 4.0; snd = 5.0}

// Exercise 2.7
let explode1 (s: string) =
    s.ToCharArray()
    |> List.ofArray

let explode2 s =
    let rec concat (s: string) = function
        | x when s.Length = 0 -> x
        | x -> s.[s.Length - 1] :: x
            |> concat (s.Remove(s.Length - 1))
    concat s [] 

// Exercise 2.8
let implode (cs: char list) =
    let folder = fun c s -> (string c) + s
    List.foldBack folder cs ""

let implodeRev (cs: char list) =
    let folder = fun s c -> (string c) + s
    List.fold folder "" cs

// Exercise 2.9
let toUpper s =
    s
    |> explode1 
    |> List.map(fun x -> Char.ToUpper(x))
    |> implode

// Exercise 2.10
let rec ack = function
    | (m,n) when m = 0 -> n + 1
    | (m,n) when m > 0 && n = 0 -> ack(m-1, 1)
    | (m,n) when m > 0 && n > 0 -> ack(m-1, ack(m, n - 1))
    | _ -> -1

// Exercise 2.11
let time f =  
    let start = System.DateTime.Now  
    let res = f ()  
    let finish = System.DateTime.Now  
    (res, finish - start)

let timeArg1 f a =
    time(fun x -> f a)

// Exercise 2.12
let rec downto3 f n e =
    if (n > 0) 
        then (downto3 f) (n-1) (f n e)
    else e

let fac n = 
    downto3 (fun n a -> n * a) n 1

let range g n = 
    downto3 (fun n a -> g(n)::a) n []

// Exercise 2.13
type word = (char * int) list
let hello: word = [('H',4); ('E',1); ('L',1); ('L',1); ('O',1)]

// Exercise 2.14
type squareFun = {w: word; pos: int; acc: int}
// type squareFun = word -> int -> int -> int


let singleLetterScore s =
    snd(s.w.[s.pos]) + s.acc

let doubleLetterScore s =
    snd(s.w.[s.pos]) * 2 + s.acc

let tripleLetterScore s =
    snd(s.w.[s.pos]) * 3 + s.acc


// Exercise 2.15
let doubleWordScore s = 
    s.acc * 2

let tripleWordScore s = 
    s.acc * 3

// Exercise 2.16
let containsNumbers (s: squareFun) =
    let generateResult (s: squareFun) (b: bool) =
        if (b)
            then - s.acc
        else s.acc
    s.w
    |> List.map (fun x -> Char.IsNumber(fst(x)))
    |> List.contains true
    |> generateResult s





[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code