let reverse list =
    let rec reverse list acc =
        match list with
        | [] -> acc
        | head :: tail -> reverse tail (head :: acc)

    reverse list []

printfn $"%A{reverse [ 1..10 ]}"
