module TestNetwork

open NUnit.Framework
open FsUnit
open LocalNetwork


[<Test>]
let ``Test on virus-safe PC``() =
    shouldIGetInfected(probabilities[Hard]) |> should equal false
    
[<Test>]
let ``Test on virus-unsafe PC``() =
    shouldIGetInfected(probabilities[Weak]) |> should equal true