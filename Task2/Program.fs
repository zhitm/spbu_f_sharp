module hw2.TreeMap

type Tree<'a> =
    | Empty
    | Node of value: 'a * left: Tree<'a> * right: Tree<'a>


let rec treeMap tree func =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) -> Node(func value, treeMap left func, treeMap right func)
