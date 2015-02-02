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
let rec test (a, b, c) =
    if a < b then
        notDivisible(a, c) && test(a + 1, b, c)
    else
        notDivisible(c, a)

// 2.7.2
let prime (i) =
    match i with
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ when i < 1 -> false
    | _ -> test(2, i, i)

let rec nextPrime (i) =
    if prime(i) then
        i + 1
    else
        nextPrime(i + 1)
    
    
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
    printfn "2.7 (1): %A" (test(5, 4, 1))
    printfn "2.7 (2): %A" (prime(23)) // Not working
    printfn "2.7 (2): %A" (prime(24)) // Not working
    // printfn "2.7 (3): %A" (nextPrime(24))


    Console.Read() |> ignore
    0 // return an integer exit code
