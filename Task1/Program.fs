module hw2.Task1

let isEven x = x % 2 = 0
let countEvenNumbers1 list = (List.filter isEven list).Length

let countEvenNumbers2 list =
    list |> List.filter isEven |> List.length

let countEvenNumbers3 list =
    List.map (fun el -> if isEven el then 1 else 0) list |> List.sum

printfn $"{countEvenNumbers1 []}"
