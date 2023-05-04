module TestWorkflows.Round

open NUnit.Framework
open Workflows
open FsUnit

[<Test>]
let ``2.0 / 12.0 / 3.5 is 0.048`` () =
    rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return! a / b
    }
    |> should (equalWithin 0.001) 0.048
    
[<Test>]
let ``1.12345 is 1.123`` () =
    rounding 3 {
        let! a = 1.12345
        return! a
    }
    |> should (equalWithin 0.001) 1.123
    
[<Test>]
let ``1/9 is 0.1111`` () =
    rounding 4 {
        let! a = 1.0/9.0
        return! a
    }
    |> should (equalWithin 0.001) 0.1111
    
    
[<Test>]
let ``"1"+"Ъ" is None`` () =
    calculate {
        let! x = "1"
        let! y = "Ъ"
        let z = x + y
        return z
    }
    |> should equal None

