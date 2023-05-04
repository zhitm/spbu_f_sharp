module TestWorkflows.Calculate

open NUnit.Framework
open Workflows
open FsUnit


[<Test>]
let ``"1"+"Ъ" is None`` () =
    calculate {
        let! x = "1"
        let! y = "Ъ"
        let z = x + y
        return z
    }
    |> should equal None


[<Test>]
let ``"1"+"2" is Some 3`` () =
    calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    |> should equal (Some 3)
