module GeneticAlgorithm.Logic.Actors

open GeneticAlgorithm.Logic.WorldPos

type ActorKind =
  | Squirrel of hasAcorn:bool
  | Tree
  | Acorn
  | Rabbit
  | Aspen

type Actor =
  { Pos : WorldPos
    ActorKind : ActorKind }

let getChar actor =
  match actor.ActorKind with
  | Squirrel _ -> 'S'
  | Tree _ -> 't'
  | Acorn _ -> 'a'
  | Rabbit _ -> 'R'
  | Aspen _ -> 'A'
