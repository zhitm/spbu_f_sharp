let indexOf list el =
    let rec indexOf list el sizeBefore =
        match list with
        | [] -> None
        | head :: _ when head = el -> Some sizeBefore
        | _ :: tail -> indexOf tail el (sizeBefore + 1)

    indexOf list el 0

printfn $"{indexOf [ 0..9 ] -1 = None}"
printfn $"{indexOf [ 0..9 ] 1}"
