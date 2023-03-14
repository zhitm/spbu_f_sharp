module hw2.ExpressionTree

type Operator =
    | Plus
    | Minus
    | Divide
    | Multiply

type BinaryExpressionTree<'a> =
    | Leaf of 'a
    | Tree of operator: Operator * left: BinaryExpressionTree<'a> * right: BinaryExpressionTree<'a>

let rec calculate expressionTree =
    match expressionTree with
    | Leaf leaf -> leaf
    | Tree (operator, left, right) ->
        match operator with
        | Plus -> calculate left + calculate right
        | Minus -> calculate left - calculate right
        | Divide -> calculate left / calculate right
        | Multiply -> calculate left * calculate right
