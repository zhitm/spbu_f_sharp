module LambdaInterpreterTest

open Lambda
open NUnit.Framework
open FsUnit

[<Test>]
let testReductionAppLambda1 () =
    reduce (Application(Lambda("a", Variable "a"), Variable "a"))
    |> should equal (Variable "a")

[<Test>]

let testReductionAppLambda2 () =
    reduce (Application(Lambda("a", Variable "b"), Variable "a"))
    |> should equal (Variable "b")

[<Test>]

let testReductionAppLambda3 () =
    reduce (Application(Lambda("a", Variable "b"), Variable "b"))
    |> should equal (Variable "b")

[<Test>]
let testReductionAppLambda4 () =
    reduce (Application(Lambda("a", Variable "c"), Variable "b"))
    |> should equal (Variable "c")


[<Test>]
let testReductionVariable () =
    reduce (Variable "a") |> should equal (Variable "a")

[<Test>]
let testReductionApplication () =
    reduce (Application(Variable "a", Variable "b"))
    |> should equal (Application(Variable "a", Variable "b"))

[<Test>]
let testGetNewVariable1 () =
    getNewVariable "x" [ "x"; "y"; "xx" ] |> should equal "xxxx"

[<Test>]
let testGetNewVariable2 () =
    getNewVariable "x" [ "a"; "y"; "x" ] |> should equal "xx"

[<Test>]
let testReductionLambda () =
    reduce (Lambda("a", (Application(Lambda("a", Variable "c"), Variable "b"))))
    |> should equal (Lambda("a", Variable "c"))
