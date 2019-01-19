namespace Fulmar

open Types
open AdventureTypes
open Functions
open Constructors

module AdventureFunctions =
  let prettyPrintGoal b = 
    let rhs = b.targetExpr |> prettyPrint true
    let lhs = ((birdExpr b.target) :: (List.init b.target.order freeExpr)) |> compound |> prettyPrint true
    sprintf "%s = %s" lhs rhs

  let prettyPrintDoneGoal b = 
    match b.state with
    | Found(ub, e) ->
      let rhs = b.targetExpr |> prettyPrint true
      let lhs = ((birdExpr b.target) :: (List.init b.target.order freeExpr)) |> compound |> prettyPrint true
      let middle = ub.initial |> prettyPrint true
      sprintf "%s = %s = %s" lhs middle rhs
    | _ -> ""

  let mapPrintWithText list lambda text output =
    if List.isEmpty list then
      output
    else
      let o' = logString output text
      list 
      |> List.map(lambda)
      |> logStrings o'

