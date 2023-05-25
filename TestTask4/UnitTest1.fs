module TestTask4

open FsUnit
open NUnit.Framework
open Primes

let primesSeq = [ for i in 0..5 -> Seq.item i (getPrimes ()) ]

[<Test>]
let TestPrimes () =
    primesSeq |> should equal [ 2L; 3L; 5L; 7L; 11L; 13L ]
