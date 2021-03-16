// Exercise 5.1
let sum m n =
    let rec aux acc i =
        match i with
            | 0 -> acc
            | x -> aux (acc + x + m) (x - 1)
    aux m n

// Exercise 5.2
let length lst =
    let rec aux acc l =
        match l with 
            | [] -> acc
            | _::y -> aux (acc + 1) y
    aux 0 lst

// Exercise 5.3
let foldBack f lst acc =
    let rec aux f lst conti =
        match lst with 
        | [] -> conti acc 
        | x::xs -> aux f xs (fun r -> conti(f x r))
    aux f lst id 


(* Exercise 5.4 *)

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)

    aux 1 x

let factC x =
    let rec aux x conti =
        match x with
            | 0 -> conti 1
            | n -> aux (n - 1) (fun r -> conti (n * r))
    aux x id



(* Compare the running time between factA and factC. Which solution is faster and why? 
   Both are linear, as they both need to run the number of recursions equal to the given number, as they both say n-1 each time.
*)

(* Exercise 5.5 *)

let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA x =
    let rec aux x acc1 acc2 =
        match x with
            | 0 -> 0
            | 1 -> acc2
            | n -> aux (n - 1) acc2 (acc1 + acc2)
    aux x 0 1

let fibC x =
    let rec aux v conti =
        match v with
            | 0 -> conti 0
            | 1 -> conti 1
            | n -> aux (n - 1) (fun a -> aux (n-2) (fun b -> conti(a + b)))
    aux x id


(* Compare the running time of fibW, fibA and fibC
   fibW and fibA both have linear run times. This is because they make recursions/iterations equal to the number given to the function.
   fibC has an exponential runtime of 2^n as eacj recursive call creates two new recursive calls, until the base-case is reached.

*)

(* Exercise 5.6 *)

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(* TODO *)
(* The call bigListK id 130000 causes a stack overflow. 
   Analyse the problem and describe exactly why this happens. 
   Why is this not an iterative function?

   The function is not tail recursive, so when it starts to evaluate the continuations, it will fill up the stack.
   The fixed version looks like this:
*)
let rec bigListOK c =
    function
    | 0 -> c []
    | n -> bigListOK (fun res -> c(1 :: res)) (n - 1)


(* Exercise 5.7 *)

type word = (char * int) list

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | CharToInt of cExp     (* NEW: Cast to integer *)

and cExp =
   | C  of char             (* Character literal *)
   | CV of aExp             (* Character lookup at word index *)
   | ToUpper of cExp        (* Convert character to upper case *)
   | ToLower of cExp        (* Convert character to lower case *)
   | IntToChar of aExp      (* NEW: Cast to character *)

let rec arithEvalSimple (a: aExp) (w: word) (s:Map<string,int>) = 
  match a with 
    | N a -> a   
    | V a ->
        if (s.TryFind(a).IsSome) then 
            s.TryFind(a).Value
        else 0
    | WL -> w.Length
    | PV a -> w.[arithEvalSimple a w s] |> snd
    | Add (a,b) -> arithEvalSimple a w s + arithEvalSimple b w s
    | Sub (a,b) -> arithEvalSimple a w s - arithEvalSimple b w s
    | Mul (a,b) -> arithEvalSimple a w s * arithEvalSimple b w s
    | CharToInt c -> charEvalSimple c w s |> int

and charEvalSimple (c: cExp) (w: word) (s: Map<string,int>) = 
    match c with 
        | C c -> c
        | ToUpper c -> charEvalSimple c w s |> System.Char.ToUpper
        | ToLower c -> charEvalSimple c w s |> System.Char.ToLower
        | CV c -> w.Item(arithEvalSimple c w s) |> fst
        | IntToChar a -> arithEvalSimple a w s |> char



let arithEvalTail a w s cont = failwith "not implemented"

let charEvalTail c w s cont = failwith "not implemented"

let charEval c w s  = charEvalTail c w s id