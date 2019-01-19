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
  
  type StateTransition =
  | StateMessage of string
  | InventoryUpdate of KnownBird
  | GoalUpdate

  type BirdMap = { symbolMap : Map<string, CribbedBird>; evalMap : Map<KnownBird,  ExpressionElement list -> ExpressionElement list option>; crib : (KnownBird * ExpressionElement) list }

  type StringState<'A> = { state : 'A; logger : 'A -> StateTransition -> 'A } with
      member this.message str = { this with state = this.logger this.state str }
      member this.log = StateMessage >> this.message
  
  let logBird (stateLogger : StringState<_>) = InventoryUpdate >> stateLogger.message
  let logString (stateLogger : StringState<_>) = stateLogger.log
  let logStrings stateLogger = List.fold(logString) stateLogger
