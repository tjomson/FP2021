﻿module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    // Exercise 6.1
    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = s.vars.Tail}))
        
    // Exercise 6.2
    let wordLength : SM<int> = 
        S (fun s -> Success (s.word.Length, s))

    // Exercise 6.3
    let characterValue (pos : int) : SM<char> = 
        S (fun s -> 
            if (s.word.Length <= pos || pos < 0) then
                Failure (IndexOutOfBounds pos)
            else
                Success (s.word.[pos] |> fst, s)    
        ) 

    // Exercise 6.4
    let pointValue (pos : int) : SM<int> = 
        S (fun s -> 
            if (s.word.Length <= pos || pos < 0) then
                Failure (IndexOutOfBounds pos)
            else
                Success (s.word.[pos] |> snd, s)    
        )

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    // Exercise 6.5
    let update (x : string) (v : int) : SM<unit> = 
        let rec aux processed remaining =
            match remaining with
                | []      -> None
                | m :: ms -> 
                    match Map.tryFind x m with
                        | Some _ -> Some (processed @ (Map.add x v m) :: ms )
                        | None   -> aux (processed @ m :: []) ms
        S (fun s ->
            match (aux [] s.vars) with
                | Some a -> Success((),{s with vars = a})
                | None   -> Failure (VarNotFound x)
          )

    // Exercise 6.6
    let declare (var : string) : SM<unit> = 
        S (fun s ->
            if (s.vars.Length = 0) then
                Failure(IndexOutOfBounds 0)
            elif (Set.contains var s.reserved) then
                Failure(ReservedName var)
            elif (Map.containsKey var s.vars.Head) then
                Failure (VarExists var)
            else
                Success((),{s with vars = (Map.add var 0 s.vars.Head) :: s.vars.Tail})
        )




              

    