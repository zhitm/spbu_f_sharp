module PhoneBook
open System
open System.IO
open System.Text.Json

type Command =
    | FindByNumber of string
    | FindByName of string
    | PrintAll
    | Add of string * string
    | Save of string
    | ReadFromFile of string
    | Help
    | Exit

type Record = { Name: string; Number: string }

let findByNumber records number =
    records
    |> Seq.tryFind (function
        | record -> record.Number = number)

let findByName records name =
    records
    |> Seq.tryFind (function
        | record -> record.Name = name)

let printRecords records =
    records |> Seq.iter (fun record -> printfn "%s: %s" record.Name record.Number)

let printRecord record =
    match record with
    | None -> printfn "no such record"
    | Some r -> printfn "%s: %s" r.Name r.Number


let addRecord records name number =
    { Name = name; Number = number } :: records

let saveToFile records path =
    let json = JsonSerializer.Serialize records
    File.WriteAllText(path, json)


let loadFromFile path =
    match File.Exists path with
    | true -> Some(File.ReadAllText(path) |> JsonSerializer.Deserialize<List<Record>>)
    | false -> None

let help () =
    printfn
        """Commands:
* findByName -- find record by a given name
* add --- add record to phone book
* findByNumber --- find record by a given phone
* print --- print all records in phone book
* save --- save all records to path
* load --- load all records from path
* help --- get list of commands
* exit --- exit from application
"""

let getCommand str =
    printf str
    Console.ReadLine()


let rec startInteraction records command =
    match command with
    | "exit" -> Environment.Exit(0)
    | "help" ->
        help()
        startInteraction records (getCommand "> ")
    | "add" ->
        let name = getCommand "Name: "
        let number = getCommand "Number: "
        let newRecords = addRecord records name number
        startInteraction newRecords (getCommand "> ")

    | "findByName" ->
        let name = getCommand "Name: "
        printRecord (findByName records name)
        startInteraction records (getCommand "> ")

    | "findByNumber" ->
        let number = getCommand "Number: "
        printRecord (findByNumber records number)
        startInteraction records (getCommand "> ")

    | "print" -> printRecords records
    | "save" ->
        let path = getCommand "Path: "
        saveToFile records path
        startInteraction records (getCommand "> ")
    | "load" ->
        let path = getCommand "Path: "

        let newRecords =
            match loadFromFile path with
            | None -> records
            | Some x -> x

        startInteraction newRecords (getCommand "> ")
    | _ -> printfn "Unknown command"

help()
startInteraction [] (getCommand "> ")
