let fibonacci n =
    let rec fibonacci n last beforeLast =
        if n = 0 then last
        else fibonacci (n-1) (last+beforeLast) last
    fibonacci n 0L 1L

printfn $"{fibonacci 6}"