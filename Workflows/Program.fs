module Workflows

open System

type CalculateBuilder() =

    member this.Bind(x: string, f) =
        let num = Int32.TryParse x

        match num with
        | true, number -> f number
        | _ -> None

    member this.Return(x) = Some x

let calculate = CalculateBuilder()


type RoundingBuilder(nDigits: int) =
    member this.Bind(value: float, f) = f <| Math.Round(value, nDigits)
    member this.Return(x: float) = Math.Round(x, nDigits)
    member this.ReturnFrom(x: float) = Math.Round(x, nDigits)

let rounding order = RoundingBuilder(order)
