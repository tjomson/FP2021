// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// 1.1
type 'a bintree =
| Leaf
| Node of 'a bintree * 'a * 'a bintree

let insert x t =
    let rec aux (bintree) =
        match bintree with
        | Node (left, value, right) -> 
            if (value < x) then
                (Node(left, value, aux (right)))
            else
                (Node(aux (left), value, right))
        | Leaf -> Node(Leaf, x, Leaf)

    aux t

// 1.2
let fromList lst =
    let rec aux remainLst acc =
        match remainLst with
        | [] -> acc
        | x::xs -> aux xs (insert x acc)
    aux lst Leaf

// 1.3
let fold f acc t =
    let rec aux ac curr =
        match curr with
        | Leaf -> ac
        | Node(left, value, right) -> aux (f (aux ac left) value) right 

    aux acc t

let foldBack f acc t =
    let rec aux ac curr =
        match curr with
        | Leaf -> ac
        | Node(left, value, right) -> aux (f (aux ac right) value) left

    aux acc t

let inOrder t: 'a list =
    foldBack (fun acc ele -> 
        ele :: acc
    ) [] t

// 1.4
let rec badMap f =
    function
    | Leaf -> Leaf
    | Node (l, y, r) -> Node (badMap f l, f y, badMap f r)



// 2.1

let rec foo = 
    function
    | [x] -> [x]
    | x :: y :: xs when x > y -> y :: (foo (x :: xs))
    | x::xs -> x :: foo xs


// 3.1
type bigint = BigInt of Map<int, int>

let fromString (nums: string) =
    let rec aux acc remain =
        match remain with
        | [] -> acc
        | head :: tail -> 
            aux (Map.add (Map.count acc) (head |> string |> int) acc) tail
    
    aux Map.empty (Seq.toList nums)
    |> BigInt

// 3.2
let add (BigInt(a)) (BigInt(b)) =
    let rec aux index spillover acc =
        if (index = Map.count a) then
            acc
        else 
            let bVal = defaultArg (b.TryFind index) 0
            let newVal = a.[index] + bVal + spillover
            if (newVal > 9) then
                aux (index + 1) 1 (Map.add index (newVal - 9) acc)
            else 
                aux (index + 1) 0 (Map.add index (newVal) acc)
    aux 0 0 (Map.empty)
    |> BigInt
        
// 3.3
let multSingle (BigInt(x)) y =
    let rec aux index spillover acc =
        if (index = Map.count x) then
            acc
        else
            let newVal = x.[index] * y
            // let spill = 

// 4.1
type 'a llist =
| Cons of (unit -> ('a * 'a llist))


let step (Cons(ll)) =
    ll ()

let cons x ll =
    Cons (fun () -> (x, ll))

// 4.2
let init f =
    let rec aux i = 
        Cons (fun () -> (f i, aux (i + 1)))
    aux 0
    
// 4.3
let map (f: 'a -> 'b) (Cons(x)) =
    let value = x ()
    Cons (fun () -> (f (value |> fst), (value |> snd)))


[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code