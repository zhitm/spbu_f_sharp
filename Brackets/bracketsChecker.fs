module Brackets.BracketsChecker

let pairs = Map [ '(', ')'; '{', '}'; '[', ']' ]

let isOpen bracket = Seq.contains bracket pairs.Keys
let isClosed bracket = Seq.contains bracket pairs.Values
let parseString str =
    let rec parse str (acc: char list) =
        match str with
        | head :: _ when not (isOpen head || isClosed head) -> false
        | head :: tail when isOpen head -> parse tail (head :: acc)
        | head :: tail when isClosed head ->
            match acc with
            | [] -> false
            | accHead :: accTail when pairs[accHead] = head -> parse tail accTail
            | _ -> false
        | [] -> acc.IsEmpty

    parse (Seq.toList str) []
