namespace Fulmar

open Fulmar.Types

module Constructors =

  let singleExpr single = { head = single; tail = [] }
  let birdExpr = Bird >> singleExpr
  let freeExpr = Free >> singleExpr

  let compound expressionList = 
    match expressionList with
    | [] -> birdExpr Identity
    | [h] -> h
    | h :: tl -> 
      { head = h.head; tail = h.tail @ tl }

  let decompound expr =
    (singleExpr expr.head) :: expr.tail 
    
  let toSolver expr = 
    { current = { expression = expr; nextFree = 0 }; previous = []; length = 1 }

  let rec padArgs current final expr =
    if current < final then
      let e' = { expr with tail = expr.tail @ [freeExpr current] }
      padArgs (current+1) final e'
    else
      expr

  let kb init chain = { initial = init; chain = chain } 

  let solver birds = 
    let expr = 
      match birds with
      | [] -> birdExpr Identity
      | [b] -> birdExpr b
      | b :: btail -> { head = Bird(b); tail = btail |> List.map(birdExpr) }
    expr |> toSolver

  let consoleLogger = { state = (); logger = fun () s -> printfn "%s" s }
  
  let private maybeAdd opt exp = 
    match opt with 
    | Some(c) -> { c with tail = c.tail @ [exp] }
    | None -> exp
    
  type ParseState = { currentElt : ExpressionElement option; exactMatch : KnownBird option; stack : ExpressionElement list; candidates : KnownBird list }
  
  let newState birdList = { currentElt = None; exactMatch = None; stack = []; candidates = birdList }
  let enstack birdList c state = { stack = c :: state.stack; currentElt = None; exactMatch = None; candidates = birdList } 

  let destack birdList c s stail =
    let c' = { s with tail = s.tail @ [c] }
    { stack = stail; currentElt = Some(c'); candidates = birdList; exactMatch = None }
  
  let parseExpression (birdList : KnownBird list) string =
    let enstack' = enstack birdList
    let destack' = destack birdList
    let rec parseNew' state (acc : char list) chList =
      match chList with
      | [] -> 
        match acc with
        | [] -> 
          state.currentElt |> Ok
        | xs -> 
          match state.exactMatch with
          | Some(bird) ->              
            let exp = birdExpr bird
            maybeAdd state.currentElt exp |> Some |> Ok
          | None -> 
            xs 
            |> List.rev 
            |> System.String.Concat 
            |> sprintf "Unprocessable characters '%s'" 
            |> Error
      | ch :: tail ->
        match ch with
        | '(' -> 
          match acc with
          | [] -> 
            match state.currentElt with
            | None -> 
              sprintf "Bad format - is there a leading '('?" 
              |> Error
            | Some(c) ->
              let s' = enstack' c state
              parseNew' s' [] tail
          | xs -> 
            match state.exactMatch with
            | Some(bird) ->              
              let exp = birdExpr bird
              let c' = maybeAdd state.currentElt exp
              let s' = enstack' c' state
              parseNew' s' [] tail
            | None -> 
              xs 
              |> List.rev 
              |> System.String.Concat 
              |> sprintf "Unprocessable characters '%s'" 
              |> Error
        | ')' ->
          match acc with
          | [] -> 
            match state.currentElt with
            | None -> 
              sprintf "Nothing inside brackets - is there exactly '()'?" 
              |> Error
            | Some(c) ->
              match state.stack with
              | [] ->
                sprintf "Bad format - is there a ')' before a '('?" 
                |> Error
              | s :: stail -> 
                let s' = destack' c s stail
                parseNew' s' [] tail
          | xs -> 
            match state.exactMatch with
            | Some(bird) ->        
              match state.stack with
              | [] ->
                sprintf "Bad format - is there a ')' before a '('?" 
                |> Error
              | s :: stail -> 
                let exp = birdExpr bird
                let c' = maybeAdd state.currentElt exp
                let s' = destack' c' s stail
                parseNew' s' [] tail  
            | None -> 
              xs 
              |> List.rev 
              |> System.String.Concat 
              |> sprintf "Unprocessable characters '%s'" 
              |> Error
        | _ ->
          let protoname = (ch :: acc) |> List.rev |> System.String.Concat
          let birdCandidates = state.candidates |> List.filter(fun b -> b.symbol.StartsWith(protoname))
          let actualBirds = birdCandidates |> List.filter(fun b -> b.symbol = protoname)
          match (birdCandidates, actualBirds) with
          | ([],_) ->
            match state.exactMatch with
            | None -> 
              protoname
              |> sprintf "No symbol found starting with '%s'" 
              |> Error
            | Some(b) ->
              let exp = birdExpr b
              let c' = maybeAdd state.currentElt exp
              parseNew' { state with currentElt = Some(c'); exactMatch = None } [] (ch :: tail)
          | ([bird],[b']) ->
            let exp = birdExpr bird
            let c' = maybeAdd state.currentElt exp
            parseNew' { state with currentElt = Some(c'); exactMatch = None } [] tail
          | (birds, _) ->
            let s' = { state with exactMatch = actualBirds |> List.tryHead }
            parseNew' s' (ch :: acc) tail
    let chars = string |> List.ofSeq
    parseNew' (newState birdList) [] chars

  let parseExpressionLogger loggerState (birdList : KnownBird list) string =
    match parseExpression birdList string with
    | Error(e) -> (logString loggerState e, None)
    | Ok(e) -> (loggerState, e)

  let emptyBirdMap = { symbolMap = Map.empty; evalMap = Map.empty; crib = [] }