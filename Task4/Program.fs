module Primes


let LSqrt n =
    let nF = (float n)
    int (sqrt nF)

let isPrime n =
    let rec isPrime n acc =
        if acc = 1L then true
        else if n % acc = 0L then false
        else isPrime n (acc - 1L)

    if n = 1 then false else isPrime n (LSqrt n)

let getPrimes () =
    let rec getNext acc =
        seq {
            if (isPrime acc) then
                yield acc
                yield! getNext (acc + 1)
            else
                yield! getNext (acc + 1)
        }

    getNext 2

for el in getPrimes () do
    printfn $"%d{el}"
