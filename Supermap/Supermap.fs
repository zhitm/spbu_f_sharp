module supermap

let mapOneArg arg funList =
    let rec mapOneArg arg funList acc =
        match funList with
        | [] -> acc
        | head :: tail ->
            let newElement = head (arg)
            mapOneArg arg tail (newElement :: acc)

    mapOneArg arg funList []

let supermap funList argList =
    let rec supermap funList argList acc =
        match argList with
        | [] -> acc
        | head :: tail -> supermap funList tail acc @ (mapOneArg head funList)

    supermap funList argList []


let addOne x = x + 1
let twice x = x * 2

let m = supermap [ addOne; twice ] [ 1; 5 ]

printfn "%A" m
