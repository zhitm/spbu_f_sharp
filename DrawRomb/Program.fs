module Romb


let repeatLine times str =
    let rec grow result acc =
        if acc > 0 then grow (result + str) (acc - 1) else result

    grow "" times

let createLine spaceCntLeft spaceCntMiddle =
    repeatLine spaceCntLeft " " + "*" + repeatLine spaceCntMiddle " " + "*" + "\n"

let createFirstLine n = repeatLine (n - 1) " " + "*" + "\n"

let spacesCntMiddle i n = (n - i) * 2 - 1

let createUsualLine lineIndFromCenter n =
    createLine (lineIndFromCenter - 1) (spacesCntMiddle lineIndFromCenter n)

let drawRomb n =
    let rec recDrawRomb n lineIndFromCenter acc =
        if (lineIndFromCenter = n) then
            (createFirstLine n) + acc + (createFirstLine n)
        else if (lineIndFromCenter = 1) then
            recDrawRomb n (lineIndFromCenter + 1) ((createUsualLine lineIndFromCenter n))
        else
            recDrawRomb
                n
                (lineIndFromCenter + 1)
                ((createUsualLine lineIndFromCenter n)
                 + acc
                 + createUsualLine lineIndFromCenter n)

    recDrawRomb n 1 ""

printfn "%s" (drawRomb 6)
