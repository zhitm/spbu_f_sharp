module TestPointFree

open PointFree
open NUnit.Framework
open FsCheck

let test (l: list<int>, x: int) =
    fun1 x l = fun2 x l && fun1 x l = fun3 x l

Check.Quick test