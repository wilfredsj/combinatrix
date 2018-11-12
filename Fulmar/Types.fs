namespace Fulmar

module Types = 
  type KnownBird = { name : string; symbol : string; order : int }

  type LeafBird =
    | Free of int
    | Bird of KnownBird
  
  type ExpressionElement = { head : LeafBird; tail : ExpressionElement list } 
  
  type ExpressionState = { expression : ExpressionElement; nextFree : int }

  type ExpressionChain = { current : ExpressionState; previous : ExpressionState list; length : int } 
    
  let Identity = { name = "Identity"; symbol = "I"; order = 1 }

  type UsefulBird = { initial : ExpressionElement; chain : ExpressionChain }

  type UnsolveableChain =
  | Looping of ExpressionChain
  | Timeout of ExpressionChain * int

  type Category = 
  | Trivial
  | NonUseful of ExpressionChain
  | Unsolveable of UnsolveableChain
  | Proper of UsefulBird
  | Known of (KnownBird*UsefulBird)

  type CribbedBird = { bird : KnownBird; operator : ExpressionElement list -> ExpressionElement list option; crib : ExpressionElement }

  type BirdMap = { symbolMap : Map<string, CribbedBird>; evalMap : Map<KnownBird,  ExpressionElement list -> ExpressionElement list option>; crib : (KnownBird * ExpressionElement) list }

  type StringState<'A> = { state : 'A; logger : 'A -> string -> 'A } with
      member this.log str = { this with state = this.logger this.state str }

  let logString stateLogger str = { stateLogger with state = stateLogger.logger stateLogger.state str }
  let logStrings stateLogger strings = strings |> List.fold(logString) stateLogger
