module Primes

let LSqrt n =
    let nF = (float n)
    int64 (sqrt nF)

let isPrime n =
    let rec isPrime n acc =
        if acc = 1L then true
        else if n % acc = 0L then false
        else isPrime n (acc - 1L)

    if n = 1L then false else isPrime n (LSqrt n)

let getPrimes () =
    let rec getNext acc =
        seq {
            if (isPrime acc) then
                yield acc
                yield! getNext (acc + 1L)
            else
                yield! getNext (acc + 1L)
        }

    getNext 2

for el in getPrimes () do
    printfn $"%d{el}"
