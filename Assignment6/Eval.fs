module Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H',4); ('E',1); ('L',1); ('L',1); ('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    // Exercise 6.7
    let add (a: SM<int>) (b: SM<int>)  = 
        a >>= fun x -> b >>= fun y -> ret (x + y)

    let sub (a: SM<int>) (b: SM<int>)  = 
        a >>= fun x -> b >>= fun y -> ret (x - y)

    let mul (a: SM<int>) (b: SM<int>)  = 
        a >>= fun x -> b >>= fun y -> ret (x * y)

    let modulo (a: SM<int>) (b: SM<int>)  = 
        a >>= fun x -> b >>= fun y -> 
            if y = 0 then
                fail DivisionByZero
            else 
                ret (x % y)
   
    // Assignment 6.8
    let div (a: SM<int>) (b: SM<int>) = 
        a >>= fun x -> b >>= fun y ->
            if y = 0 then
                fail DivisionByZero
            else 
                ret (x / y)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    


    // Assignment 6.9
    let rec arithEval (a: aExp) : SM<int> = 
        match a with
            | N x -> ret x 
            | V x -> lookup x
            | WL -> wordLength
            | PV x -> (arithEval x) >>= fun b -> pointValue b
            | Add (x, y) -> add (arithEval x) (arithEval y) 
            | Sub (x, y) -> sub (arithEval x) (arithEval y) 
            | Mul (x, y) -> mul (arithEval x) (arithEval y) 
            | Div (x, y) -> div (arithEval x) (arithEval y)
            | Mod (x, y) -> modulo (arithEval x) (arithEval y)
            | CharToInt c -> charEval c >>= fun x -> x |> int |> ret

    and charEval c : SM<char> = 
        match c with 
            | C x -> ret x
            | CV x -> x |> arithEval >>= fun v -> v |> characterValue
            | ToUpper x -> charEval x >>= fun v -> v |> System.Char.ToUpper |> ret
            | ToLower x -> charEval x >>= fun v -> v |> System.Char.ToLower |> ret
            | IntToChar a -> arithEval a >>= fun x -> x |> char |> ret

    let rec boolEval b : SM<bool> = 
        match b with 
            | TT -> ret true
            | FF -> ret false
            | AEq (a, b) -> arithEval a >>= fun x -> arithEval b >>= fun y -> ret (x = y)
            | ALt (a, b) -> arithEval a >>= fun x -> arithEval b >>= fun y -> ret (x < y)
            | Not x -> boolEval x >>= fun y -> y |> not |> ret
            | Conj (a, b) -> boolEval a >>= fun x -> boolEval b >>= fun y -> ret (x && y)
            | IsLetter c -> charEval c >>= fun x -> x |> System.Char.IsLetter |> ret
            | IsDigit c -> charEval c >>= fun x -> x |> System.Char.IsDigit |> ret


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval (stmnt: stm) : SM<unit> = 
        match stmnt with
            | Declare s -> declare s
            | Ass (s, a) -> arithEval a >>= fun i -> update s i
            | Skip -> ret ()
            | Seq (x, y) -> 
            | ITE (b, x, y) ->
            | While (b, x) ->


(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    