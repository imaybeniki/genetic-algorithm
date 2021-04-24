module GeneticAlgorithm.Logic.Simulator

open GeneticAlgorithm.Logic.WorldPos
open GeneticAlgorithm.Logic.World
open GeneticAlgorithm.Logic.Actors


// GameState is a standard object used to represent the game's state at a specific point in time
type GameState = { World : World; Player : Actor }

// check boundaries
let isValidPos pos (world: World): bool = 
  pos.X >= 1 && pos.Y >= 1 && pos.X <= world.MaxX && pos.Y <= world.MaxY

// uses the pipe forward operator (|>) to invoke Seq.exists with world.Actors as the first parameter of the seq.Exists
let hasObstacle pos (world: World) : bool =
  world.Actors
  |> Seq.exists(fun actor -> pos = actor.Pos)

let moveActor world actor xDiff yDiff = 
  let pos = newPos (actor.Pos.X + xDiff) (actor.Pos.Y + yDiff)

  if (isValidPos pos world) && not (hasObstacle pos world) then
    let actor = { actor with Pos = pos }
    match actor.ActorKind with
    | Squirrel _ -> { world with Squirrel = actor }
    | Tree -> { world with Tree = actor }
    | Acorn -> { world with Acorn = actor }
    | Rabbit -> { world with Rabbit = actor }
    | Aspen -> { world with Aspen = actor }
  else
    world

// uses Seq.tryFind to search the world.Actors array for an actor at the specified position
// if Some actor is there return the result of the getChar function
// otherwise use . to indicate empty space
let getCharacterAtCell(x, y) (world:World) =
  let actorAtCell =
    world.Actors
    |> Seq.tryFind(fun actor -> actor.Pos.X = x && actor.Pos.Y = y)

  match actorAtCell with
  | Some actor -> getChar actor
  | None -> '.'

// discriminated union containing all types of player input except the Exit command
type GameCommand =
  | MoveLeft | MoveRight
  | MoveUp | MoveDown
  | MoveUpLeft | MoveUpRight
  | MoveDownLeft | MoveDownRight
  | Wait
  | Restart

// takes in a prior state and a Command, then matches it based on the command and returns the new state
let playTurn state player getRandomNumber command =
  let world = state.World
  match command with 
  | MoveLeft -> { state with World = moveActor world player -1 0 }
  | MoveRight -> { state with World = moveActor world player 1 0 } 
  | MoveUp -> { state with World = moveActor world player 0 -1 } 
  | MoveDown -> { state with World = moveActor world player 0 1 }
  | MoveUpLeft  -> { state with World = moveActor world player -1 -1 }
  | MoveUpRight -> { state with World = moveActor world player 1 -1 }
  | MoveDownLeft -> { state with World = moveActor world player -1 1 } 
  | MoveDownRight -> { state with World = moveActor world player 1 1 }
  | Wait ->
    printfn "Time Passes..."
    state
  | Restart ->
    let world = makeWorld 13 13 getRandomNumber
    { World = world; Player = world.Squirrel }

type Command =
  | Action of GameCommand
  | Exit