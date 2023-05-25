module LambdaInterpreterTest

open Lambda
open NUnit.Framework
open FsUnit

[<Test>]
let testReductionAppLambda1 () =
    reduceWhileNecessary (Application(Lambda("a", Variable "a"), Variable "a"))
    |> should equal (Variable "a")

[<Test>]

let testReductionAppLambda2 () =
    reduceWhileNecessary (Application(Lambda("a", Variable "b"), Variable "a"))
    |> should equal (Variable "b")

[<Test>]

let testReductionAppLambda3 () =
    reduceWhileNecessary (Application(Lambda("a", Variable "b"), Variable "b"))
    |> should equal (Variable "b")

[<Test>]
let testReductionAppLambda4 () =
    reduceWhileNecessary (Application(Lambda("a", Variable "c"), Variable "b"))
    |> should equal (Variable "c")

[<Test>]
let testReductionAppLambda5 () =
    let t =
        Application(Application(Lambda("x", Lambda("y", Variable "y")), Variable "a"), Variable "b")

    reduceWhileNecessary (t) |> should equal (Variable "b")



[<Test>]
let testReductionVariable () =
    reduceWhileNecessary (Variable "a") |> should equal (Variable "a")

[<Test>]
let testReductionApplication () =
    reduceWhileNecessary (Application(Variable "a", Variable "b"))
    |> should equal (Application(Variable "a", Variable "b"))

[<Test>]
let testGetNewVariable1 () =
    getNewVariable "x" [ "x"; "y"; "varxx" ] |> should equal "varvarxxvarxx"

[<Test>]
let testGetNewVariable2 () =
    getNewVariable "x" [ "a"; "y"; "x" ] |> should equal "varxx"

[<Test>]
let testReductionLambda () =
    reduceWhileNecessary (Lambda("a", (Application(Lambda("a", Variable "c"), Variable "b"))))
    |> should equal (Lambda("a", Variable "c"))

[<Test>]
let testAlphaConversion () =
    let t =
        reduceWhileNecessary (
            Application(Lambda("x", Lambda("y", Application(Variable "x", Variable "y"))), Variable "y")
        )

    reduceWhileNecessary t
    |> should equal (Lambda("varyy", Application(Variable "y", Variable "varyy")))
