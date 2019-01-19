namespace Fulmar

open Types
open AdventureTypes
open Functions
open Constructors

module AdventureConstructors = 
  let goal bird expr = { target = bird; targetExpr = expr; state = Unknown }
  let find ub state goal =
    match state with
    | None ->
      if ub.chain.current.expression = goal.targetExpr then
        let newGoal = { goal with state = Found((ub, prettyPrint true ub.initial))}
        (newGoal, Some(newGoal))
      else 
        (goal, None)
    | _ -> (goal, state)

  let isRemaining goal = 
    match goal.state with
    | Unknown -> true
    | _ -> false
    
  let isRemainingInt goal = 
    match goal.state with
    | Unknown -> 1
    | _ -> 0

  let updateGoals' ub gls = 
    let (goals', x) = gls.goals |> List.mapFold(find ub) None
    match x with
    | None -> ({ gls with goals = goals' }, x)
    | Some(_) -> ({ goals = goals'; remaining = List.sumBy(isRemainingInt) goals' }, x)
  
  let updateGoals ub astate = 
    let (newGoals, updated) = updateGoals' ub astate.goalList
    ({ astate with goalList = newGoals }, updated)
  let nullGoals = { goals = []; remaining = 0 }
  
  let private birdText = {
    newBirdConfirmation = "Ok! We now can reference our new bird (%s) with the symbol %s";
    newBirdPrompt = "Please enter a new bird";
    newBirdPromptFirst = "Let's try to find a bird. Please enter an expression:";
    dontName = "Ok, let's not name this bird";
    knownBirds = "These are the birds we know about:";
    alreadyKnown = "We already know this bird";
    improperCombinator = "Can't be reduced further than %s. This is not a proper bird";
    equivalentCombinator = "This is a proper bird, but it's equivalent to the %s (%s)";
    newCombinator = "Useful - let's use this to define a new proper bird. What symbol shall we use?"
    repeatSymbol = "'%s' represents a bird we already know about, a %s"
  }

  let private combinatorText = {
    newBirdConfirmation = "Ok! We now can reference our new combinator (%s) with the symbol %s";
    newBirdPrompt = "Please input another expression";
    newBirdPromptFirst = "Let's try to find a new combinator. Please enter an expression:";
    dontName = "Ok, not naming this combinator";
    knownBirds = "These are the combinators that we know about:";
    alreadyKnown = "We already know this combinator";
    improperCombinator = "Can't be reduced further than %s. This is not a proper combinator";
    equivalentCombinator = "This is a proper combinator, but it's equivalent to the %s (%s)";
    newCombinator = "Useful - let's use this to define a new combinator. What symbol shall we use?";
    repeatSymbol = "'%s' represents a combinator we already know about, a %s"
  }

  let nullProgress = { birds = emptyBirdMap; goalList = nullGoals; text = combinatorText }

