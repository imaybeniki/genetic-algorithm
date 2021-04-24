module GeneticAlgorithm.Logic.World

open System
open GeneticAlgorithm.Logic.Actors
open GeneticAlgorithm.Logic.WorldPos

    // returns a random position with x and y as type System.Random
    // defined as method because otherwise F# will not re-evaluate the results each time (to achieve memoization) 
  let getRandomPos(maxX:int32, maxY:int32, random: Random): WorldPos =
    let x = random.Next(maxX) + 1
    let y = random.Next(maxY) + 1
    newPos x y

    // sequences (seq) are an F# version of an immutable collection that can be iterated over
    // Actor seq returns sequence of zero to many Actor instances
  let generate (maxX:int32, maxY:int32, random: Random): Actor seq =
    let pos = getRandomPos(maxX, maxY, random)
    seq {
      yield createSquirrel pos
    }

    // manages the game board and arrangement of actors within it
  type World (maxX: int32, maxY: int32, random: Random) = 
    let actors = generate(maxX, maxY, random)
    member this.Actors = actors
    member this.MaxX = maxX
    member this.MaxY = maxY

    member this.GetCharacterAtCell(x, y) =
        //  mutable variables can be assigned new value after initial assignment
      let mutable char = '.'
      for actor in this.Actors do
        if actor.Pos.X = x && actor.Pos.Y = y then
          char <- actor.Character
        // 'char' below ends method - loads char variable into memory and returns
      char