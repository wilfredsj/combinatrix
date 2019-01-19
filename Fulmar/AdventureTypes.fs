namespace Fulmar

open Types

module AdventureTypes =
  type GoalState = 
  | Unknown
  | Found of (UsefulBird*string)

  type Goal = { target : KnownBird; targetExpr : ExpressionElement; state : GoalState }

  type GoalList = { goals : Goal list; remaining : int }

  type AdventureText = {
    newBirdConfirmation : Printf.StringFormat<string->string->string>;
    newBirdPrompt : string;
    newBirdPromptFirst : string;
    dontName : string;
    knownBirds : string;
    alreadyKnown : string;
    improperCombinator : Printf.StringFormat<string->string>;
    equivalentCombinator : Printf.StringFormat<string->string->string>;
    newCombinator : string;
    repeatSymbol : Printf.StringFormat<string->string->string>
  }
  
  type AdventureState = { birds : BirdMap; goalList : GoalList; text : AdventureText }