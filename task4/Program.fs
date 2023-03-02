let rec logPowerOf2 n =
    if n = 0L then
        1L
    elif n % 2L = 1L then
        let pow = logPowerOf2 (n / 2L)
        pow * pow * 2L
    else
        let pow = logPowerOf2 (n / 2L)
        pow * pow

let listOfPowers n m =
    let rec listOfPowers n m previous =
        match m - n with
        | 1L -> [ logPowerOf2 m ]
        | _ ->
            let newElement = previous * 2L
            newElement :: (listOfPowers (n + 1L) (m) newElement)

    listOfPowers n (n + m) (logPowerOf2 n)

printfn $"%A{listOfPowers 3L 7L}"
