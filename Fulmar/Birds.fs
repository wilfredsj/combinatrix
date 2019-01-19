namespace Fulmar

open Fulmar.Types
open Fulmar.Constructors

module Birds = 
  let Bluebird = { name = "Bluebird"; symbol = "B"; order = 3 }
  let bluebird tail = 
    match tail with 
      | x :: y :: z :: _ -> Some(x :: compound([y;z]) :: [])
      | _ -> None

  let Mockingbird = { name = "Mockingbird"; symbol = "M"; order = 1 }
  let mocking tail = 
    match tail with 
      | x :: _ -> Some([x; x])
      | _ -> None

  let identity (tail : ExpressionElement list) = 
    match tail with 
      | x :: _ -> Some([x])
      | _ -> None

  let Cardinal = { name = "Cardinal"; symbol = "C"; order = 3 }
  let cardinal (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: z :: _ -> Some(x :: z :: y :: [])
      | _ -> None

  let Cardinal' = { name = "Cardinal*"; symbol = "C*"; order = 4 }
  let cardinal' (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: z :: w :: _ -> Some(x :: y :: w :: z :: [])
      | _ -> None

  let Robin = { name = "Robin"; symbol = "R"; order = 3 }
  let robin (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: z :: _ -> Some(y :: z :: x :: [])
      | _ -> None

  let Thrush = { name = "Thrush"; symbol = "T"; order = 2 }
  let thrush (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: _ -> Some(y :: x :: [])
      | _ -> None

  let Kestrel = { name = "Kestrel"; symbol = "K"; order = 2 }
  let kestrel (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: _ -> Some(x :: [])
      | _ -> None

  let Lark = { name = "Lark"; symbol = "L"; order = 2 }
  let lark (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: _ -> Some(x :: compound([y;y]) :: [])
      | _ -> None

  let Warbler= { name = "Warbler"; symbol = "W"; order = 2 }
  let warbler (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: _ -> Some(x :: y :: y :: [])
      | _ -> None

  let Starling = { name = "Starling"; symbol = "S"; order = 3 }
  let starling (tail : ExpressionElement list) = 
    match tail with 
      | x :: y :: z :: _ -> Some(x :: z :: compound([y;z]) :: [])
      | _ -> None

  let birdOperatorMap = 
    [(Bluebird, bluebird); 
    (Mockingbird, mocking);
    (Cardinal, cardinal);
    (Cardinal', cardinal');
    (Thrush, thrush);
    (Kestrel, kestrel);
    (Robin, robin);
    (Lark, lark);
    (Starling, starling);
    (Warbler, warbler);
    (Identity, identity)] 
    |> Map.ofList

  let mToFinal = Functions.toFinal 50 birdOperatorMap
