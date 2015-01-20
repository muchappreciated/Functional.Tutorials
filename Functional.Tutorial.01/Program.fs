open System;

// 1.1
let g = fun n -> n + 4

// 1.2
let h (x, y) =
    Math.Sqrt (x * x + y * y)

// 1.3

// 1.4
let rec f = function
    | 0 -> 0
    | n -> n + f (n-1)

// 1.5
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)

// 1.6
let rec sum (m, n) = 
    match (m, n) with
    | (m, 0) -> m
    | (m, n) -> (m + n) + sum(m, n-1)


[<EntryPoint>]
let main argv = 
    printfn "1.1: %i" (g 3)
    printfn "1.2: %f" (h (3.0, 5.0))
    printfn "1.3: who knows"
    printfn "1.4: %i" (f(4))
    printfn "1.5: %i" (fib(4))
    printfn "1.6: %i" (sum(4, 10))


    Console.Read() |> ignore
    0 // return an integer exit code
