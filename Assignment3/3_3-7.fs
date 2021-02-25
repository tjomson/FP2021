module aExp

    // Exercise 3.3
    type word = (char * int) list
    type squareFun = word -> int -> int -> int

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)

    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"

    let hello: word = [('H',4); ('E',1); ('L',1); ('L',1); ('O',1)]

    let rec arithEval (a: aExp) (w: word) (s:Map<string,int>) = 
      match a with 
        | N a -> a   
        | V a ->
            if (s.TryFind(a).IsSome) then 
                s.TryFind(a).Value
            else 0
        | WL -> w.Length
        | PV a -> w.[arithEval a w s] |> snd
        | Add (a,b) -> arithEval a w s + arithEval b w s
        | Sub (a,b) -> arithEval a w s - arithEval b w s
        | Mul (a,b) -> arithEval a w s * arithEval b w s

    // Exercise 3.4
    type cExp =
       | C  of char      (* Character value *)
       | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
       | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
       | CV of aExp      (* Character lookup at word index *)

    let rec charEval (c: cExp) (w: word) (s: Map<string,int>) = 
        match c with 
            | C c -> c
            | ToUpper c -> charEval c w s |> System.Char.ToUpper
            | ToLower c -> charEval c w s |> System.Char.ToLower
            | CV c -> w.Item(arithEval c w s) |> fst

    // Exercise 3.5
    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit  of cExp     (* check for constant *)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


    let rec boolEval (b: bExp) (w: word) (s: Map<string, int>) = 
        match b with
            | TT -> true
            | FF -> false
            | AEq (a, b) -> arithEval a w s = arithEval b w s
            | ALt (a, b) -> arithEval a w s < arithEval b w s
            | Not b -> not (boolEval b w s)
            | Conj (a, b) -> (boolEval a w s) && (boolEval b w s)
            | IsLetter c -> charEval c w s |> System.Char.IsLetter
            | IsDigit c -> charEval c w s |> System.Char.IsDigit

    // Exercise 3.6
    type stmnt =
       | Skip                        (* does nothing *)
       | Ass of string * aExp        (* variable assignment *)
       | Seq of stmnt * stmnt        (* sequential composition *)
       | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
       | While of bExp * stmnt       (* while statement *)

    let rec evalStmnt (stm: stmnt) (w: word) (s: Map<string, int>) = 
        // let removeAndAdd (x: string) (v: int) =
        //     s.Remove(x).Add(x,v)
        match stm with
            | Skip -> s
            | Ass (x, a) -> arithEval a w s |> (fun (v) -> s.Remove(x).Add(x,v)) 
            | Seq (stm1, stm2) -> evalStmnt stm2 w (evalStmnt stm1 w s)
            | ITE (guard, stm1, stm2) -> 
                if (boolEval guard w s) then
                    evalStmnt stm1 w s
                else
                    evalStmnt stm2 w s
            | While (guard, stm) -> 
                if (boolEval guard w s) then
                    evalStmnt (While(guard, stm)) w (evalStmnt stm w s)
                else
                    s

           




    // Exercise 3.7
    let stmnt2SquareFun (stm: stmnt): squareFun = 
        fun w pos acc -> (evalStmnt stm w (Map[("_pos_", pos); ("_acc_", acc)])).["_result_"]








    let singleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithSingleLetterScore))
    let doubleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleLetterScore))
    let tripleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleLetterScore))

    let doubleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleWordScore))
    let tripleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleWordScore))

    let containsNumbers : squareFun = 
        stmnt2SquareFun 
            (Seq (Ass ("_result_", V "_acc_"),
                  While (V "i" .<. WL,
                         ITE (IsDigit (CV (V "i")),
                              Seq (Ass ("_result_", V "_result_" .*. N -1),
                                   Ass ("i", WL)),
                              Ass ("i", V "i" .+. N 1)))))
                         

    type square2 = (int * stmnt) list

    let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
    let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
    let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

    let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
    let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

    let calculatePoints2 : square2 list -> word -> int = failwith "not implemented"
    