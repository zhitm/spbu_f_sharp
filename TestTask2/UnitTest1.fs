module TestTask2

open FsUnit
open NUnit.Framework
open hw2.TreeMap

let tree = Node(2, Node(1, Empty, Empty), Node(3, Empty, Empty))
let treeAfterMap = Node(4, Node(2, Empty, Empty), Node(6, Empty, Empty))

[<Test>]
let TestTreeMap () =
    let newTree = treeMap tree (fun x -> 2 * x)
    newTree |> should equal treeAfterMap
