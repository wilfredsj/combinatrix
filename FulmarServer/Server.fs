open System
open System.Threading
open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils

open System
open System.Net

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Fulmar.API
open Fulmar.Types


// Taken from Suave Websockets sample

let createWebsocket (getSession : HttpContext -> WebSocket -> string) (webSocket : WebSocket) (context: HttpContext) =
  socket {
    let mutable loop = true
    
    while loop do
      let! msg = webSocket.read()
      match msg with
      | (Text, data, true) ->
        let str = UTF8.toString data
        let sess = getSession context webSocket
        printfn "%s sent %s" sess str
        let output = userAction sess concatStringer str |> fun o -> o.state
        let mutable responses : string list = []
        let mutable newInventory : string list = []
        output 
        |> List.iter(
          function
          | StateMessage(s) -> responses <- s :: responses
          | InventoryUpdate(kb) -> newInventory <- kb.name :: newInventory
          | _ -> ()) 
        let toString list =
          if List.isEmpty list then
            None
          else
            list |> String.concat("<br><p>") |> Some
        let responseOpt = responses |> toString
        let newInvStr = newInventory |> toString
        let jsonify key value = sprintf "{\"%s\": \"%s\"}" key value
        context.clientIpTrustProxy.ToString() |> printfn "IP = %s"

        // the response needs to be converted to a ByteSegment
        let toBytes key resp = 
          resp
          |> jsonify key
          |> System.Text.Encoding.ASCII.GetBytes
          |> ByteSegment

        match responseOpt with
        | Some(response) ->
          printfn "Response = %s" response
          do! webSocket.send Text (toBytes "resp" response) true
        | _ -> ()
        match newInvStr with
        | Some(inv) ->
          do! webSocket.send Text (toBytes "inv" inv) true
        | _ -> ()

      | (Close, _, _) ->
        let sess = getSession context webSocket
        printfn "Closed: Session #%s" sess
        let emptyResponse = [||] |> ByteSegment
        do! webSocket.send Close emptyResponse true
        loop <- false
      | _ -> ()
    }

let mutable nextSession = 0L
let sessionIds = new System.Collections.Generic.Dictionary<WebSocket, string>()
let addSession (ctx : HttpContext) (ws : WebSocket) = 
  if sessionIds.ContainsKey ws then
    sessionIds.Item ws
  else 
    let sessionId = sprintf "||%i||" nextSession
    ctx.clientIpTrustProxy.ToString()
    |> printfn "Opened: Session %s from %s" sessionId
    sessionIds.Item ws <- sessionId
    nextSession <- nextSession + 1L
    sessionId

let app : WebPart = 
  choose [
    path "/websocket" >=> handShake (createWebsocket addSession)
    GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
    NOT_FOUND "Found no handlers." ]

let rec argsParse homeDir args =
  match args with
  | [] -> homeDir
  | "-h" :: dir :: xs -> argsParse (Some dir) xs
  | x :: xs -> argsParse homeDir xs

[<EntryPoint>]
let main args =
  let homeDir = argsParse None (args |> List.ofArray)
  let config = 
    let log_ = Targets.create Warn [||]
    let bindings_ = 
        [ HttpBinding.create HTTP IPAddress.Loopback 80us
          HttpBinding.createSimple HTTP "0.0.0.0" 8080 ]
    { defaultConfig with logger = log_; bindings = bindings_; homeFolder = homeDir }
  startWebServer config app
  0
