module TestMap

open supermap
open NUnit.Framework
open FsUnit

let addOne x = x + 1
let twice x = x * 2

[<Test>]
let ``Test on 2 args and 2 functions`` () =
    supermap [ addOne; twice ] [ 1; 5 ] |> should equal [ 10; 6; 2; 2 ]

[<Test>]

let ``Test on 1 args and 2 functions`` () =
    supermap [ addOne; twice ] [ 2 ] |> should equal [ 4; 3 ]
