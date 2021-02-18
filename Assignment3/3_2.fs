// Exercise 3.2
module AexpState

type aExp =
    | N of int
    | V of string
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)


type state = Map<string, int>

let rec arithEvalState (a: aExp) (s: Map<string,int>) =
    match a with
        | N a -> a   
        | V a -> 
            if (s.TryFind(a).IsSome) then 
                s.TryFind(a).Value
            else 0
        | Add (a,b) -> arithEvalState a s + arithEvalState b s
        | Sub (a,b) -> arithEvalState a s - arithEvalState b s
        | Mul (a,b) -> arithEvalState a s * arithEvalState b s