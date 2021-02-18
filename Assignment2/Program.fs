open System

// Exercise 2.1
let downto1 n = 
    if n = 0 then []
    else [ for i in 1 .. n -> n - i + 1 ]
        
let downto2 = function
    | 0 -> []
    | n -> [ for i in 1 .. n -> n - i + 1 ]

// Exercise 2.2
// let removeOddIdx xs =
//     let rec concat (xs: List<'a>) (ys: List<'a>) n =
//         match n with 
//         | -1 -> ys
//         | n when n % 2 = 0 -> concat xs (xs.[n]::ys) (n - 1)
//         | _ -> concat xs ys (n - 1)
//     concat xs [] (xs.Length - 1)

let rec removeOddIdx = 
    function
    | x::y::z -> x::removeOddIdx(z)
    | z->z

// Exercise 2.3
// let combinePair = function
//     | (x:List<'a>) when x.Length % 2 = 0 -> [for i in 0 .. 2 .. x.Length - 1 -> (x.[i],x.[i + 1])]
//     | x -> [for i in 0 .. 2 .. x.Length - 2 -> (x.[i],x.[i + 1])]

let rec combinePair =
    function
    | x::y::z -> (x,y)::combinePair(z)
    | z -> []

// Exercise 2.4
// type complex = {fst: float; snd: float}
type complex = float * float

let mkComplex x y = complex(x,y)

let complexToPair c = (fst(c), snd(c))

// Exercise 2.5
let (|+|) (x: complex) (y: complex) = complex(fst(x) + fst(y), snd(x) + snd(y))

let (|*|) x y = complex(fst(x) * fst(y) - snd(x) * snd(y), snd(x) * fst(y) + fst(x) * snd(y))

// Exercise 2.6
let negate x = complex(-fst(x), -snd(x))
let (|-|) x y = x |+| negate(y)

let invert x = complex (fst(x) / (fst(x)**2.0 + snd(x)**2.0), -snd(x) / (fst(x)**2.0 + snd(x)**2.0))
let (|/|) x y = x |*| invert(y)

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

let singleLetterScore (w:word) (pos:int) (acc:int) =
    snd(w.[pos]) + acc

let doubleLetterScore (w:word) (pos:int) (acc:int) =
    snd(w.[pos]) * 2 + acc

let tripleLetterScore (w:word) (pos:int) (acc:int) =
    snd(w.[pos]) * 3 + acc


// Exercise 2.15
let doubleWordScore (w:word) (pos:int) (acc:int) = 
    acc * 2

let tripleWordScore (w:word) (pos:int) (acc:int) = 
    acc * 3

// Exercise 2.16
let containsNumbers (w:word) (pos:int) (acc:int) =
    let generateResult (acc: int) (b: bool) =
        if (b)
            then - acc
        else acc
    w
    |> List.map (fun x -> (x |> fst |> Char.IsNumber))
    |> List.contains true
    |> generateResult acc





[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code