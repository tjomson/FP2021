// Exercise 1.1
let sqr x = x * x
// Exercise 1.2
let pow x n = System.Math.Pow(x,n)
// Exercise 1.3
let rec sum = function
    | 0 -> 0
    | n -> n + sum(n - 1)
// Exercise 1.4
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)
// Exercise 1.5
    // (System.Math.PI, fact -1)
    // fact(fact 4)
let rec fact = function    
    | 0 -> 1    
    | n -> n * fact(n-1)
let rec power = function    
    | (x,0) -> 1.0                    
    | (x,n) -> x * power(x,n-1)


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
