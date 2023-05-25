module Brackets.BracketsChecker

let pairs = Map [ '(', ')'; '{', '}'; '[', ']' ]

let isOpen bracket = Seq.contains bracket pairs.Keys
let isClosed bracket = Seq.contains bracket pairs.Values

let parseString str =
    let rec parse str (acc: char list) =
        match str with
        | head :: tail when not (isOpen head || isClosed head) -> parse tail acc
        | head :: tail when isOpen head -> parse tail (head :: acc)
        | head :: tail when not (isOpen head) ->
            match acc with
            | [] -> false
            | accHead :: accTail when pairs[accHead] = head -> parse tail accTail
            | _ -> false
        | _ :: _ -> false
        | [] -> acc.IsEmpty


    parse (Seq.toList str) []
