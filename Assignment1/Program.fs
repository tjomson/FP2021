// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = System.Math.Pow(x,n)

// Exercise 1.3
let rec sum = function
    | 0 -> 0
    | n when n > 0 -> n + sum(n - 1)
    | n -> n + sum(n + 1)

// Exercise 1.4
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)

// Exercise 1.5
    // (System.Math.PI, fact -1) = Stack overFlow
    // fact(fact 4) = int
    // power(System.Math.PI, fact 2) = float
    // (power, fact) = (function, function)

// Exercise 1.6
let dup = fun s -> s + s + ""

// Exercise 1.7
let rec dupn s = function
    | 1 -> s + ""
    | n -> s + dupn s (n - 1)

// Exercise 1.8
let rec bin = function
    | (n,k) when k = 0 || n = k -> 1
    | (n,k) -> bin(n-1, k-1) + bin(n-1,k)

// Exercise 1.9
    // 1: int * int -> int
    // 2: When x = 0
    // 3: 
        // f(2, 3) = f(2 - 1, 2 * 3)
        // f(1, 6) = f(1 - 1, 1 * 6)
        // f(0, 6) = 6
    // 4: Multiplies x and y as many times as there x is big, with x -1 each time, and y being the product of the previous x and y.

// Exercise 1.10
    // 1: (bool, int) -> int
    // 2: Causes a stack . If it could run, it would be 0 though
    // 3: This will result in a stack overflow, due to fact

// Exercise 1.11
//TODO

// Assignment 1.12
let empty (letter: char, pointValue: int) = fun (pos:int) -> 
    (letter, pointValue)

// Assignment 1.13
let add (newPos: int) (letter: char, pointValue: int) word = fun (pos:int) ->



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
