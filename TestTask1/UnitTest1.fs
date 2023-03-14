module TestTask1

open NUnit.Framework
open FsCheck
open hw2.Task1
open FsUnit

let testCountEvenNumbers list =
    (countEvenNumbers1 list = countEvenNumbers2 list)
    && (countEvenNumbers2 list = countEvenNumbers3 list)

[<Test>]
let testCountEvenNumbers3 () =
    countEvenNumbers3 [ 1; 2; 3; 4; 5 ] |> should equal 2

[<Test>]
let testWithFsCheck () =
    Check.QuickThrowOnFailure testCountEvenNumbers
