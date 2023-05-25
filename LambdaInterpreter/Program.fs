module Lambda

type Variable = string

type Term =
    | Variable of string
    | Lambda of string * Term
    | Application of Term * Term

let rec termToString term : string =
    match term with
    | Variable name -> name
    | Lambda(var, body) -> $"(λ {var}.{termToString (body)})"
    | Application(firstTerm, secondTerm) -> $"{termToString (firstTerm)} {termToString (secondTerm)}"

let getFreeVariables term =
    let rec getFreeVariables term boundList freeList =
        match term with
        | Variable var ->
            if List.exists ((=) var) boundList then
                freeList
            else
                var :: freeList
        | Lambda(var, body) -> getFreeVariables body (var :: boundList) freeList
        | Application(term1, term2) ->
            (getFreeVariables term1 boundList freeList)
            @ (getFreeVariables term2 boundList freeList)

    getFreeVariables term [] []

let rec getNewVariable variable freeVariables =
    let rawVariable = $"var{variable}{variable}"

    if not <| List.exists ((=) rawVariable) freeVariables then
        rawVariable
    else
        getNewVariable rawVariable freeVariables

/// <summary>
/// Substitute the term t instead of variable x in Term.
/// </summary>
/// <param name="x">Variable name.</param>
/// <param name="t">Term to substitute.</param>
let rec substitute x t =
    function
    | Variable name as var -> if name = x then t else var
    | Lambda(y, n) as lambda ->
        if y = x then
            lambda
        else
            let TFreeVars = getFreeVariables t
            let NFreeVars = getFreeVariables n

            let isXInNFreeVariables = List.exists ((=) x) NFreeVars
            let isYInTFreeVariables = List.exists ((=) y) TFreeVars

            if isXInNFreeVariables && isYInTFreeVariables then
                let z = getNewVariable y <| TFreeVars @ NFreeVars
                let S = substitute x t (substitute y (Variable z) n)
                Lambda(z, S)
            else
                Lambda(y, substitute x t n)
    | Application(term1, term2) -> Application(substitute x t term1, substitute x t term2)



let rec reduce =
    function
    | Variable _ as var -> var
    | Lambda(var, term) -> Lambda(var, reduce term)
    | Application(Lambda(name, body), term) -> substitute name term body
    | Application(leftTerm, rightTerm) -> Application(reduce leftTerm, reduce rightTerm)

let rec reduceWhileNecessary term =
    let reduced = reduce term

    if term = reduced then
        term
    else
        reduceWhileNecessary reduced
