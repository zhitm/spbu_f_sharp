module TestTask3


open FsUnit
open NUnit.Framework
open hw2.ExpressionTree

let tree =
    Tree(Plus, Tree(Multiply, Leaf(3), Leaf(4)), Tree(Minus, Leaf(7), Leaf(4)))

[<Test>]
let TestCalculation () = calculate tree |> should equal 15
