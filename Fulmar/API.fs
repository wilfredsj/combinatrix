namespace Fulmar

open Fulmar.Types
open Fulmar.AdventureConstructors
open Fulmar.StateHandler

module API =
  let concatStringer = { state = []; logger = fun l str -> str :: l }

  let userAction userId output input =
    let (progress, promptState) = 
      if userState.ContainsKey userId then 
        userState.Item userId
      else
        (nullProgress, InitialText)
    let (p', (ps', output')) = stateAction progress promptState output input
    userState.Item userId <- (p', ps')
    output'

