
// 1.1
type Peano =
| O
| S of Peano

let toInt a =
    let rec aux acc p =
        match p with
        | O -> acc
        | S x -> aux (acc + 1u) x
    aux 0u a
        
let fromInt x =
    let rec aux acc remain =
        match remain with
        | 0u -> acc
        | y -> aux (S(acc)) (y - 1u)
    aux O x

// 1.2
// let rec add a b =
//     match a with 
//     | O -> b
//     | S(x) -> S(add x (S(x)))
let rec add a b =
    match a with 
    | O -> b
    | S(x) -> S(add x b) 

let rec mult a b =
    match a with 
    | O -> O
    | S(x) -> add b (mult x b)

let rec pow a b =
    match b with
    | O -> S(O)
    | S(x) -> mult a (pow a x)

// 1.3
let tailAdd a b =
    let rec aux x acc =
        match x with 
        | O -> acc
        | S(x) -> aux x (S(acc))
    aux a b

let tailMult a b =
    let rec aux x acc =
        match x with
        | O -> acc
        | S(x) -> aux x (tailAdd b acc)
    aux a O

let tailPow a b =
    let rec aux exp acc =
        match exp with
        | O -> acc
        | S(x) -> aux x (tailMult a acc)
    aux b (S(O))


// 1.4
let rec loop f acc p =
    match p with 
    | O -> acc
    | S(x) -> loop f (f acc) x

// 1.5
let loopAdd a b =
    loop S a b

let loopMult a b =
    loop (loopAdd a) O b

let loopPow a b =
    loop (loopMult a) (S(O)) b

// 2.1
let rec f x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

let rec g xs =
    function
    | []    -> xs = []
    | y::ys -> 
        match f y xs with
        | Some xs' -> g xs' ys
        | None     -> false

// f has type 'a -> list<'a> -> option<list<'a>>
// g has type list<'a> -> list<'a> -> bool
// f returns a list where the first occurence of x has been removed, if x is not present in the list, it returns none.

// 2.2
// the when-restrictions are not as such part of the pattern match, so the compiler thinks that
// there is missing a case of y::ys even though in practice, x is always either equal or not equal to y

let rec f2 x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys -> 
        match f2 x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None
// 2.3
let rec fOpt x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys -> 
        fOpt x ys
        |> Option.map (fun ys' -> y::ys')

let rec gOpt xs =
    function
    | []    -> xs = []
    | y::ys -> 
        fOpt y xs
        |> Option.map (fun xs' -> gOpt xs' ys)
        |> Option.defaultValue false


// 2.4

      

// 3.1
let calculatePi (i: uint64) =
    let rec aux index acc =
        match index with
        | curr when i < curr -> 
            acc
        | 0UL ->
            aux (index + 1UL) 3m
        | curr when curr % 2UL = 1UL -> 
            aux (curr + 1UL) (acc + 4m / ((2UL * curr * (2UL * curr + 1UL) * (2UL * curr + 2UL)) |> decimal))
        | curr -> 
            aux (curr + 1UL) (acc - 4m / ((2UL * curr * (2UL * curr + 1UL) * (2UL * curr + 2UL)) |> decimal))
    aux 0UL 0m

// 3.2
let piSeq =
    Seq.unfold (fun i -> 
        Some (calculatePi i, i + 1UL)
    ) 0UL

// 3.3
let circleArea (r: float) =
    let rDec = r |> decimal
    Seq.map (fun pi ->
        pi * rDec * rDec
    ) piSeq

let sphereVolume (r: float) =
    let rDec = r |> decimal
    Seq.map (fun pi ->
        4m/3m * pi * rDec * rDec * rDec
    ) piSeq

// 3.4
let circleSphere (r: float) =
    let rec aux cirSeq sphSeq =
        seq {
            yield (Seq.head cirSeq, Seq.head sphSeq)
            yield! aux (Seq.tail cirSeq) (Seq.tail sphSeq)
        }
    aux (circleArea r) (sphereVolume r)
    
// 3.5
let parallelPi numberOfProcesses iterationsPerProcess =
    let rec aux index acc tasks =



// 4.1
type Dir = Left | Right
type Tape<'a> = {
    pos: int
}