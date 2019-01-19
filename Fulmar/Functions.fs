namespace Fulmar

open Fulmar.Types
open Fulmar.Constructors

module Functions =
  let findBird birdMap = fun b -> Map.tryFind b birdMap.symbolMap |> Option.map(fun b -> b.bird)

  let expandOnly map bird tail =
    let evaluation = Map.find bird map
    let argsNeeded = bird.order
    let actual = tail |> List.length
    if actual < argsNeeded then
      None
    else
      let (inputTail, rem) = tail |> List.splitAt(argsNeeded)
      evaluation inputTail |> Option.map(fun newList -> (newList @ rem))


  let expand map nextFree bird tail =
    let evaluation = Map.find bird map
    let argsNeeded = bird.order
    let actual = tail |> List.length
    let rec padArgsTo target nextFree' current list =
       if current < target then
         padArgsTo target (nextFree'+1) (current+1) (freeExpr(nextFree') :: list)
       else
         (nextFree', list)
    let adjusted = 
      if actual < argsNeeded then
        let (nextFree'', free) = padArgsTo argsNeeded nextFree actual []
        let inputTail = tail @ (free |> List.rev)
        Some((inputTail, [], nextFree''))
      else
        Some((tail, tail |> List.skip(argsNeeded), nextFree))

    adjusted |> Option.bind(fun (inputTail, rem, nextFree'') -> evaluation inputTail |> Option.map(fun newList -> (newList @ rem, nextFree'')))

  let expandHead birdMap state =
    match state.expression.head with
    | Bird(b) -> expand birdMap state.nextFree b state.expression.tail |> Option.map(fun (el, i) -> { expression = compound el; nextFree = i })
    | Free(_) -> None

  let expandHeadOrReturn birdMap expr = 
    match expr.head with
    | Bird(b) -> expandOnly birdMap b expr.tail |> Option.map(compound) |> Option.defaultValue(expr)
    | Free(_) -> expr

  let expandTail birdMap expr =
    let newTail = 
      expr.tail 
      |> List.map(expandHeadOrReturn birdMap)
    { expr with tail = newTail }  

  let maybeSolve birdMap state = 
    match expandHead birdMap state.current with
    | Some(x) ->
        Some({current = x; previous = state.current :: state.previous; length = 1 + state.length })
    | None ->
      let tailExpanded = expandTail birdMap state.current.expression
      if tailExpanded = state.current.expression then
        None
      else
        let ns = { expression = tailExpanded; nextFree = state.current.nextFree }
        Some({current = ns; previous = state.current :: state.previous; length = 1 + state.length })

  let hasDuplicate chain = chain.previous |> List.contains(chain.current)
        
  let rec toFinal timeout birdMap state =
    let next = maybeSolve birdMap state 
    match next with
    | None -> Ok(state)
    | Some(n) -> 
      if hasDuplicate n then
        Error(Looping n)
      elif n.length >= timeout then
        Error(Timeout (n,timeout))
      else
        toFinal timeout birdMap n

  let rec isProper expr =
    match expr.head with
    | Bird(_) -> false
    | Free(_) ->
      expr.tail |> List.forall(isProper)

  let rec birdCount acc expr =
    match expr.head with
    | Bird(_) -> expr.tail |> List.fold(birdCount) (acc + 1)
    | Free(_) -> expr.tail |> List.fold(birdCount) (acc)
    
  let rec lastFree acc expr =
    match expr.head with
    | Bird(_) -> expr.tail |> List.fold(lastFree) (acc)
    | Free(i) -> expr.tail |> List.fold(lastFree) (if i > acc then i else acc)

  let cribbedBirdMap birds = 
    birds
    |> Map.toList 
    |> List.map(fun (b,f) -> 
          expand birds 0 b [] 
          |> Option.map(fst >> compound) 
          |> Option.defaultValue(birdExpr Identity) 
          |> fun expr -> (b, expr))
  
  let findExisting crib f =
    crib |> List.tryPick(fun (b, e) -> if e = f.expression && b.order = f.nextFree then Some(b) else None)

  let deResult res =
    match res with
    | Ok(a) -> a
    | Error(b) -> b
    
  let categorize birdMap crib state' =
    state'
    |> toFinal 50 birdMap
    |> Result.map(fun state -> 
      if state.previous |> List.isEmpty then
        NonUseful state
      else
        let initial = state.previous |> List.last |> fun x -> x.expression
        if birdCount 0 initial >= 2 then
          let final = state.current
          if isProper final.expression then
            match findExisting crib final with
            | Some(b) -> (b, kb initial state) |> Known 
            | None -> kb initial state |> Proper 
          else
            NonUseful state
        else
          Trivial)
    |> Result.mapError(Unsolveable)
    |> deResult

  let rec permute (input : ExpressionElement array) elt =
    let head = match elt.head with 
               | Bird(_) -> failwith "'permute' should only be called on proper birds"
               | Free(i) -> input.[i]
    let tail = elt.tail |> List.map(permute input)
    head :: tail |> compound 


  let makeFunction numArgs expr (tail : ExpressionElement list) = 
    let tailArr = tail |> Array.ofList
    if tailArr |> Array.length < numArgs then 
      None
    else
      Some(permute tailArr expr |> decompound)

  let prettyPrintBird b = 
    match b with
    | Free(i) -> sprintf "x<sub>%i</sub>" i
    | Bird(b) -> b.symbol
    

  let rec prettyPrint init expr =
    let headStr = expr.head |> prettyPrintBird
    let tail = expr.tail |> List.map(prettyPrint false) 
    let raw = headStr :: tail |> Seq.ofList |> String.concat(" ")
    if init || (tail |> List.isEmpty) then raw else sprintf "(%s)" raw

  let printCompound = prettyPrint true

  let printSolver state =
    (state.current :: state.previous) |> List.rev |> List.map(fun x -> x.expression) |> List.map(printCompound)

  let printSolverMaximal state =
    let finalArgs = state.current.nextFree
    let steps = (state.current :: state.previous) |> List.rev
    let render e = 
      let f = e.nextFree
      let e' = 
        if f < finalArgs then padArgs f finalArgs e.expression else e.expression
      printCompound e'
    let init = steps |> List.head |> render
    let padding = init |> Seq.map(fun c -> ' ') |> System.String.Concat
    steps |> List.tail |> List.mapFold(fun s e -> 
      let str = sprintf "%s = %s" (if s then init else padding) (render e)
      (str, false)) true |> fst

  let birdLambdaToExpression bird lambda = 
    let evalMap = [(bird, lambda)] |> Map.ofList
    expand evalMap 0 bird [] 
    |> Option.map(fst >> compound) 
    |> Option.defaultValue(birdExpr Identity) 

  let extend birdMap bird birdFunc = 
    let s = bird.symbol
    let evalMap' = Map.add bird birdFunc birdMap.evalMap
    let crib = 
      expand evalMap' 0 bird [] 
      |> Option.map(fst >> compound) 
      |> Option.defaultValue(birdExpr Identity) 
    let birdCrib = { bird = bird; operator = birdFunc; crib = crib }
    let symbolMap' = Map.add bird.symbol birdCrib birdMap.symbolMap
    let crib' = (bird, crib) :: birdMap.crib 
    { symbolMap = symbolMap'; evalMap = evalMap'; crib = crib' }

  let renderKnownBirds crib =
    crib |> List.map(fun (b, c) ->
      sprintf "%s: %s -> %s" b.name b.symbol (printCompound c))

  let renderKnownBirdEqual (b, rhs) =
    let lhs = birdExpr b :: (List.init(b.order) freeExpr) |> compound
    sprintf "%s: %s = %s" b.name (printCompound lhs) (printCompound rhs)
    
  let renderKnownBirdsEqual crib =
    crib |> List.map(renderKnownBirdEqual)