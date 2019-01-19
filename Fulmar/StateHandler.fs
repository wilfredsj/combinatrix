namespace Fulmar

open Fulmar.Types
open Fulmar.AdventureTypes
open Fulmar.Functions
open Fulmar.Constructors
open Fulmar.AdventureConstructors
open Fulmar.AdventureFunctions
open Fulmar.Birds
open System.Collections.Generic

module StateHandler =
  open System

  type SearchState =
  | InitialText
  | SelectStart
  | EnterBird
  | Continue
  | PickSymbolForBird of UsefulBird
  | NameTheBird of (UsefulBird*string)
  | Exit

  let enter x = (EnterBird, x)
  let continueQuery x = (Continue, x)
  
  let categorizePrompt progress state category =
    match category with
    | Trivial ->
      (EnterBird, logString state progress.text.alreadyKnown)
    | NonUseful s -> 
      (printCompound s.current.expression) 
      |> sprintf progress.text.improperCombinator
      |> logString state
      |> enter
    | Unsolveable u -> 
      match u with
      | Timeout(s, n) -> 
        (printCompound s.current.expression) 
        |> sprintf "Can't be solved within %i steps. Last known state was %s." n
        |> logString state
        |> enter
      | Looping(s) -> 
        let s' = printSolverMaximal s |> logStrings state
        sprintf "This expression is non-terminating." 
        |> logString s'
        |> enter
    | Known(b, useful) -> 
      let s' = printSolverMaximal useful.chain |> logStrings state
      let s'' = (printCompound useful.initial, printCompound useful.chain.current.expression) ||> sprintf "%s => %s" |> logString s'
      (b.name, b.symbol) 
      ||> sprintf progress.text.equivalentCombinator
      |> logString s''
      |> enter
    | Proper (useful) -> 
      let expr = useful.chain.current.expression
      let s' = printSolverMaximal useful.chain |> logStrings state
      let s'' = 
        (useful.initial |> padArgs 0 useful.chain.current.nextFree |> printCompound, printCompound expr) 
        ||> sprintf "%s = %s"
        |> logString s'
      
      (PickSymbolForBird useful, logString s'' progress.text.newCombinator)
      
      
  let printInventory progress =
    mapPrintWithText progress.birds.crib renderKnownBirdEqual progress.text.knownBirds

  let printGoals progress =
    let (remaining, finished) = progress.goalList.goals |> List.partition(isRemaining)
    let printFin = mapPrintWithText finished prettyPrintDoneGoal "These operators are now defined:"
    let printRem = mapPrintWithText remaining prettyPrintGoal "We need to define the following operators:"
    printFin >> printRem

  let parseBird progress output input = 
    if input = ":inv" || input = ":inventory" then
      printInventory progress output
      |> enter
    elif input = ":reset" || input = ":restart" then
      (InitialText, output)
    elif input = ":goals" || input = ":targets" then
      printGoals progress output
      |> enter
    else
      let (output', expression) = parseExpressionLogger output (progress.birds.crib |> List.map(fst)) input
      match expression with
      | None -> 
         (sprintf "Failed to understand expression '%s'" input) :: "Please try again" :: []
         |> logStrings output'
         |> enter
      | Some({head = Bird(b); tail = []}) -> 
         let details = progress.birds.crib |> List.find(fun (b', _) -> b'= b) |> snd
         (sprintf progress.text.repeatSymbol b.symbol b.name) :: "Definition:"  :: (renderKnownBirdEqual (b, details)) :: []
         |> logStrings output'
         |> enter
      | Some(expr) ->
         expr 
         |> toSolver 
         |> categorize progress.birds.evalMap progress.birds.crib 
         |> categorizePrompt progress output'
  
  let pickSymbol progress output input ub =
    if input = "" then
      logString output progress.text.dontName
      |> enter
    else
      match progress.birds.symbolMap |> Map.tryFind(input) with
      | Some(existing) ->
        sprintf "That symbol '%s' is already used by the %s (%s). Please enter another:" input existing.bird.name existing.bird.symbol
        |> logString output
        |> fun l -> (PickSymbolForBird(ub), l)
      | None ->
        "What name should we use?"|> logString output
        |> fun l -> (NameTheBird(ub, input), l)

  let addBird progress newBird newEval = 
    { progress with birds = extend progress.birds newBird newEval }

  let addGoal progress birdExpr =
    let addGoal' gl (newBird, newExpr) =
      { gl with goals = (goal newBird newExpr) :: gl.goals; remaining = gl.remaining + 1 }
    { progress with goalList = addGoal' progress.goalList birdExpr }

  let easyFilter = ["B"; "T"; "I"]
  let easyTargets = ["R"; "C"; "C*"]
  let mediumFilter = ["B"; "T"; "M"; "K"]
  let btmTargets = ["R"; "C"; "W"; "S"]
  let bcwFilter = ["B"; "C"; "W"; "K"]
  let bcwTargets = ["R"; "T"; "M"; "S"]
  let SKBasis= ["S"; "K"] 
  let SKTargets = ["I"; "W"; "T"; "M"]
  
  let redFilter (x : string list) = System.String.Concat x

  let diffInput = [("Easy", easyFilter, easyTargets); ("Medium", mediumFilter, btmTargets); ("Alternate", bcwFilter, bcwTargets); ("Hard", SKBasis, SKTargets)]
  let difficulties = diffInput |> List.map(fun (a,b,c) -> (a, b, redFilter b, a.ToLower(System.Globalization.CultureInfo.CurrentCulture), c))
  let diffStrings = difficulties |> List.map(fun (a,_,c,_,_) -> sprintf "  %s (%s)" a c)

  let pickName progress output input ub sym =
    let newName = if input = "" then sym else input
    let expr = ub.chain.current.expression
    let numArgs = (lastFree 0 expr) + 1
    let newBird = { name = newName; symbol = sym; order = numArgs }
    let output' = logBird output newBird
    let birdEval = makeFunction numArgs expr
    let progress' = addBird progress newBird birdEval
    let (progress'', goal) = updateGoals ub progress'
    let doneText = sprintf progress.text.newBirdConfirmation newName sym
    let goalText = goal |> Option.map(fun g -> sprintf "%i goals remaining" progress''.goalList.remaining)
    let (returnFunc, promptText) = 
      if progress''.goalList.remaining = 0 then
        (continueQuery, "All goals are completed. Type 'ok' to return to start, anything else to continue as usual")
      else
        (enter, progress.text.newBirdPrompt)
    let text = doneText :: Option.toList goalText @ [promptText]
    text 
    |> logStrings output'
    |> fun x -> (progress'', returnFunc x)

  let init progress output =
    "Choose a starting point:" :: diffStrings
    |> logStrings output
    |> fun x -> (progress, (SelectStart, x))

  let selectStart progress output (input : string) =
    let i' = input.ToLower(System.Globalization.CultureInfo.CurrentCulture) 
    let (symbols, goals) = 
      difficulties 
      |> List.tryFind(fun (a,b,c,lower, g) -> lower = i') 
      |> Option.map(fun (a,b,c,lower, g) -> (b, g))
      |> Option.defaultValue (easyFilter, easyTargets)
    let (progress', o) = 
      birdOperatorMap 
      |> Map.toList
      |> List.filter(fun (b,_) -> symbols |> List.contains(b.symbol))
      |> List.fold(fun (p, oo) (kb, eval) -> 
           let p' = addBird p kb eval
           let o' = logBird oo kb
           (p', o')) (nullProgress, output)
    let progress'' =
      goals
      |> List.collect(fun sym -> 
          birdOperatorMap 
          |> Map.tryPick(fun b e -> 
            if b.symbol = sym then
              Some((b, birdLambdaToExpression b e))
            else None)
          |> Option.toList)
      |> List.fold(addGoal) progress'
    let o' = printInventory progress'' o
    let o'' = printGoals progress'' o'
    progress.text.newBirdPromptFirst
    |> logString o''
    |> fun x -> (progress'', enter x)

  let maybeReset progress output input =
    if input = "ok" || input = "OK" || input = "Ok" then
      (progress, (InitialText, output))
    else
      progress.text.newBirdPromptFirst
      |> logString output
      |> fun x -> (progress, enter x)


  let stateAction progress promptState output input =
    match promptState with
    | InitialText -> init progress output
    | SelectStart -> selectStart progress output input
    | Continue -> maybeReset progress output input
    | EnterBird -> 
        (progress, parseBird progress output input)
    | PickSymbolForBird(ub) ->
        (progress, pickSymbol progress output input ub)
    | NameTheBird(ub, sym) ->
        pickName progress output input ub sym
    | Exit ->
        (progress, (Exit, output))

  type StateKey = string

  let userState = new Dictionary<StateKey, AdventureState*SearchState>()


  let consoleLoop () =
    let rec consoleLoop' progress promptState first = 
      match promptState with 
      | Exit -> ()
      | z -> 
        let input = if first then "" else System.Console.ReadLine()
        let (p', (ps', _)) = stateAction progress z consoleLogger input
        consoleLoop' p' ps' false
    consoleLoop' nullProgress InitialText true