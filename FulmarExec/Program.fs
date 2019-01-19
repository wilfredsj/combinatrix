module FulmarLauncher

open Fulmar.Types
open Fulmar.API
open Fulmar.Constructors

      
let consoleLoop2 =
  let rec consoleLoop' first = 
    let input = if first then "" else System.Console.ReadLine()
    userAction "0" consoleLogger input |> ignore
    consoleLoop' false
  consoleLoop' true |> ignore
  ()

 // TODO
 // 7 show loop when repeating

[<EntryPoint>]
let main argv =
  consoleLoop2
  0
