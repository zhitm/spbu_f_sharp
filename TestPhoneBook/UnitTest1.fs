module TestPhoneBook

open FsUnit
open NUnit.Framework
open PhoneBook

let records =
    [ { Name = "Maria"; Number = "111" }; { Name = "Petr"; Number = "222" } ]

[<Test>]
let ``Test serialization`` () =
    let filename = "records.txt"
    saveToFile records filename
    loadFromFile filename |> should equal (Some records)


[<Test>]
let ``Test find by name if exist`` () =
    findByName records "Maria"
    |> should equal (Some { Name = "Maria"; Number = "111" })
    
[<Test>]
let ``Test find by name if non exist`` () =
    findByName records "eflsrgjaeiorgjioewrgjioqerjiogerjiogwejiorgjioe"
    |> should equal None
    
[<Test>]
let ``Test find by number if exist`` () =
    findByNumber records "111"
    |> should equal (Some { Name = "Maria"; Number = "111" })
    
[<Test>]
let ``Test find by number if non exist`` () =
    findByNumber records "12345678"
    |> should equal None
    
[<Test>]
let ``loadFromFile to non existing file`` () =
   loadFromFile "!!!.txt" |> should equal None