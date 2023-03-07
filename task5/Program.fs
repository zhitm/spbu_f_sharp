let indexOf list el =
    let rec indexOf list el mismatchElementsCount =
        match list with
        | [] -> None
        | head :: _ when head = el -> Some mismatchElementsCount
        | _ :: tail -> indexOf tail el (mismatchElementsCount + 1)

    indexOf list el 0

printfn $"%A{indexOf [ 0..9 ] -1}"
printfn $"%A{indexOf [ 0..9 ] 1}"
