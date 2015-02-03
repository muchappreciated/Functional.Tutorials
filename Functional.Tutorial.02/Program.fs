open System;

// 2.1
let f = function
    | n when n % 2 = 0 -> true
    | n when n % 3 = 0 -> true
    | _ -> false

// 2.2
let rec pow (s, n) =
    match (s, n) with
    | (s, 0) -> s
    | (s, 1) -> s
    | (s, n) -> s + pow(s, n - 1)


// 2.3
let isIthChar (str:string, i, ch) =
    if str.[i] = ch then
        true
    else
        false

// 2.4
let occFromIth(str:string, i, ch) =
    if i > str.Length then
        0
    else
        str.Substring(i)
        |> Seq.filter(fun c -> c = ch)
        |> Seq.length

// 2.5
let occInString(str:string, ch) =
    occFromIth(str, 0, ch)

// 2.6
let notDivisible (d, n) =
    not(d % n = 0)

// 2.7.1
// This one does work, I'm just stupid.
(* let rec test (a, b, c) =
    if a < b then
        notDivisible(c, a) && test(a + 1, b, c)
    else
        notDivisible(c, a) *)

let rec test (a, b, c) =
    match (a, b) with
    | _ when a < b -> notDivisible(c, a) && test(a + 1, b, c)
    | _ -> notDivisible(c, a)

// 2.7.2
let prime (i) =
    match i with
    | 1 -> true
    | 2 -> true
    | _ when i < 1 -> false
    | _ -> test(2, (float)i |> Math.Sqrt |> Math.Floor |> (int), i)

let rec nextPrime (i) =
    if prime(i) then
        i
    else
        nextPrime(i + 1)

// 2.8
let rec bin (n, k) =
    match (n, k) with
    | (row, 0) -> 1
    | (row, col) when col = n -> 1
    | (row, col) -> bin(row - 1, col - 1) + bin(row - 1, k)
    
    
// 2.9
// let rec f = function
//     | (0, y) -> y
//     | (x, y) -> f(x-1, x*y)

// 2.10
// let test(c, e) = if c then e else 0


// 2.11
let VAT (n, x) = x + x * float n/100.0
    
let unVAT (n, x) = x / ((100.0 + float n) / 100.0)

// 2.12
let min f = 0
    
// 2.13
let curry f = fun a -> fun b -> f (a, b)
let uncurry f = fun(a, b) -> f a b

[<EntryPoint>]
let main argv = 

    printfn "2.1: %A" (f 24)
    printfn "2.1: %A" (f 27)
    printfn "2.1: %A" (f 29)
    
    printfn ""
    printfn "2.2: %A" (pow("T", 5))
    printfn "2.2: %A" (pow("T", 1))

    printfn ""
    printfn "2.3: %A" (isIthChar("Test", 2, 's'))
    printfn "2.3: %A" (isIthChar("Test", 1, 'v'))

    printfn ""
    printfn "2.4: %A" (occFromIth("huifawehyuiawfhuiawf", 3, 'h'))

    printfn ""
    printfn "2.5: %A" (occInString("jkasdhjkdjawd", 'j'))

    printfn ""
    printfn "2.6: %A" (notDivisible(2, 5))
    printfn "2.6: %A" (notDivisible(3, 9))

    printfn ""
    printfn "2.7 (1): %A" (test(4, 6, 2)) // false
    printfn "2.7 (1): %A" (test(9, 9, 5)) // true
    printfn "2.7 (2): %A" (prime(2))
    printfn "2.7 (2): %A" (prime(11))
    printfn "2.7 (2): %A" (prime(23))
    printfn "2.7 (2): %A" (prime(24))
    printfn "2.7 (2): %A" (prime(90))
    
    printfn ""
    printfn "2.7 (3): %A" (nextPrime(24))

    printfn ""
    printfn "2.8: %A" (bin(4, 0)) // 1
    printfn "2.8: %A" (bin(3, 2)) // 3

    printfn ""
    printfn "2.11: %A" (unVAT(23, (VAT (23, 5.0))) = 5.0)

    printfn ""
    printfn "2.12: %A" ()

    printfn ""
    printfn "2.13: %A" (curry(fun _ -> 1 + 2))
    printfn "2.13: %A" (uncurry(curry(fun _ -> 1 + 66)))

    Console.Read() |> ignore
    0 // return an integer exit code
