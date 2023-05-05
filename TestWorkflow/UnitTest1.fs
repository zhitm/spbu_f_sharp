module TestWorkflow

open Workflow
open NUnit.Framework
open FsUnit


[<Test>]
let ``Test on empty list`` () =
    evalMoves (0, 0) [] |> should equal (Some(0, 0))


[<Test>]
let ``Test on out of borders with list size of 1`` () =
    evalMoves (0, 0) [Left 5] |> should equal None
    
    
[<Test>]
let ``Test on list size of 2`` () =
    evalMoves (0, 0) [Right 5; Top 10] |> should equal (Some(5, 10))
    
[<Test>]
let ``Test on out of borders with list size of 2`` () =
    evalMoves (0, 0) [Right 5;  Bottom 10] |> should equal None
8

