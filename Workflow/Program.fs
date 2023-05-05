module Workflow

type OptionBuilder() =
    member _.Bind(v, f) = Option.bind f v
    member _.Return v = Some v
    member _.Zero() = None

let opt = OptionBuilder()


type movement =
    | Left of int
    | Right of int
    | Top of int
    | Bottom of int


let getNewPos (x, y) movement =
    opt {
        match movement with
        | Left a ->
            if (x - a >= 0 && y >= 0) then
                return ((x - a), y)
        | Right a ->
            if (x + a >= 0 && y >= 0) then
                return ((x + a), y)
        | Bottom a ->
            if (x >= 0 && y - a >= 0) then
                return (x, (y - a))
        | Top a ->
            if (x >= 0 && y + a >= 0) then
                return (x, (y + a))
    }

let rec evalMoves (x, y) listOfMovement =

    match listOfMovement with
    | head :: tail ->
        opt {
            let! pos = getNewPos (x, y) head
            let! res = evalMoves pos tail
            return res
        }
    | [] -> opt { return (x, y) }
