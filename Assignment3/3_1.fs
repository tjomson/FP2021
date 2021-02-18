// Exercise 3.1
module AExpSimple

type aExp =
    | N of int
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalSimple = function
    | N a -> a
    | Add (a,b) -> arithEvalSimple(a) + arithEvalSimple(b)
    | Sub (a,b) -> arithEvalSimple(a) - arithEvalSimple(b)
    | Mul (a,b) -> arithEvalSimple(a) * arithEvalSimple(b)
