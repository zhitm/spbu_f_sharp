module TestNetwork

open NUnit.Framework
open FsUnit
open LocalNetwork

[<Test>]
let ``Test on virus-safe PC`` () =
    let c = Computer(Hard, "1")
    c.shouldIGetInfected |> should equal false

[<Test>]
let ``Test on virus-unsafe PC`` () =
    let c = Computer(Weak, "1")
    c.shouldIGetInfected |> should equal true

[<Test>]
let ``Test simulation on weak`` () =
    let c1 = Computer(Weak, "1")
    let c2 = Computer(Weak, "2")
    let c3 = Computer(Weak, "3")
    let neighbours = [ (c1, c2); (c1, c2); (c1, c3) ]
    let infected = simulate [ c1 ] neighbours 1 [ c1; c2; c3 ]
    infected |> should equal [c1; c3; c2]

[<Test>]
let ``Test simulation on hard`` () =
    let c1 = Computer(Hard, "1")
    let c2 = Computer(Hard, "2")
    let c3 = Computer(Hard, "3")
    let neighbours = [ (c1, c2); (c1, c2); (c1, c3) ]
    let infected = simulate [ c1 ] neighbours 1 [ c1; c2; c3 ]
    infected |> should equal []
