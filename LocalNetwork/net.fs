module LocalNetwork

open System

type OS =
    | Windows
    | Linux
    | MacOs
    | Weak
    | Hard

let probabilities =
    dict[Windows, 0.7
         Linux, 0.1
         MacOs, 0.5
         Weak, 1
         Hard, 0]


type Computer(os: OS, ip: string) =
    member this.ip = ip
    member this.OS = os
    member this.probability = probabilities[os]
    member this.canBecomeInfected = this.probability <> 0
    static member private rand = Random()

    member this.shouldIGetInfected =
        (float (Computer.rand.Next(1, 100))) <= this.probability * (float 100)


let rec makeStep (infected: List<Computer>) (neighbours: List<Computer * Computer>) (newInfected: List<Computer>) =
    if neighbours = [] then
        newInfected
    else
        match neighbours.Head with
        | a, b
        | b, a when
            List.contains a infected
            && not (List.contains b infected)
            && (b.shouldIGetInfected)
            ->
            makeStep infected neighbours.Tail (b :: newInfected)

        | _ -> makeStep infected neighbours.Tail newInfected

let rec printElements (ls: List<Computer>) =
    if not ls.IsEmpty then
        printfn $"{ls.Head.ip}"
        printElements ls.Tail

let getCntCanBeInfected computers =
    List.sumBy (fun (comp: Computer) -> if comp.canBecomeInfected then 1 else 0) computers

let union left right =
    List.append left right |> Seq.distinct |> List.ofSeq

let rec simulate (infected: List<Computer>) (neighbours: List<Computer * Computer>) step listOfComputers =
    System.Threading.Thread.Sleep(100)
    let currentInfected = union infected (makeStep infected neighbours [])
    printfn $"infected on {step}: "
    printElements currentInfected

    if List.length infected <> getCntCanBeInfected listOfComputers then
        simulate currentInfected neighbours (step + 1) listOfComputers
    else
        infected


let c1 = Computer(Linux, "1")
let c2 = Computer(Windows, "2")
let c3 = Computer(Linux, "3")
let c4 = Computer(MacOs, "4")
let c5 = Computer(Windows, "5")
let c6 = Computer(MacOs, "6")

let neighbours = [ (c1, c2); (c1, c6); (c2, c3); (c3, c4); (c4, c5) ]

simulate [ c1 ] neighbours 1 [ c1; c2; c3; c4; c5; c6 ]
