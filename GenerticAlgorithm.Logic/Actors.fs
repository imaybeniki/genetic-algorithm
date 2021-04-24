module GeneticAlgorithm.Logic.Actors

open GeneticAlgorithm.Logic.WorldPos

  [<AbstractClass>]
  // Define a constructor on Actor that takes in a WorldPos. 
  // Pos member returns the pos argument from the constructor. 
  type Actor(pos: WorldPos) =
    member this.Pos = pos
    abstract member Character: char

  // Squirrel inherits Actor and invokes its constructor
  // exposes hasAcorn parameter via HasAcorn member
  // overrides Character value (represents the squirrel class with S character)
  type Squirrel(pos: WorldPos, hasAcorn: bool) =
    inherit Actor(pos)
    member this.HasAcorn = hasAcorn
    override this.Character = 'S'

    // expose createSquirrel function that creates a new Squirrel instance at the specified pos
  let createSquirrel pos = new Squirrel(pos, false)

