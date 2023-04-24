module TestBrackets

open NUnit.Framework
open Brackets.BracketsChecker
open FsUnit

[<Test>]
let Test1 () = parseString "{}" |> should be True

[<Test>]
let Test2 () = parseString "{)" |> should be False

[<Test>]
let Test3 () = parseString "{([])}" |> should be True

[<Test>]
let Test4 () = parseString "{([)]}" |> should be False

[<Test>]
let Test5 () = parseString "{}[]()" |> should be True


[<Test>]
let Test6 () = parseString "NOT A STRING" |> should be False

