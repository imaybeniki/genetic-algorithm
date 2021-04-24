module GeneticAlgorithm.Logic.World

open GeneticAlgorithm.Logic.Actors
open GeneticAlgorithm.Logic.WorldPos

type World =
  { MaxX : int
    MaxY : int
    Squirrel : Actor
    Tree : Actor
    Aspen : Actor
    Acorn : Actor
    Rabbit : Actor }

  member this.Actors = [| this.Squirrel; this.Tree; this.Aspen; this.Acorn; this.Rabbit |]
  
// grabs a random position within the acceptable range.
let getRandomPos(maxX:int32, maxY:int32, getRandom): WorldPos =
    let x = getRandom maxX
    let y = getRandom maxY
    newPos x y

// builds array of randomly-positioned entities by repeatedly generating random positions, then specifying he ActorKind of the entity
// for the squirrel it's false indicating that the Squirrel does not have the acorn initially
let buildItemsArray (maxX:int32, maxY:int32, getRandom): Actor array =
    [| 
        { Pos = getRandomPos(maxX, maxY, getRandom); ActorKind = Squirrel false }
        { Pos = getRandomPos(maxX, maxY, getRandom); ActorKind = Tree }
        { Pos = getRandomPos(maxX, maxY, getRandom); ActorKind = Aspen }
        { Pos = getRandomPos(maxX, maxY, getRandom); ActorKind = Acorn }
        { Pos = getRandomPos(maxX, maxY, getRandom); ActorKind = Rabbit }
    |]

// searches all actors to see if any rules are violated. 
// rules: after generation, no actor can start in a corner and no actor can start adjacent to any other actor.
let hasInvalidlyPlacedItems (items: Actor array, maxX: int32, maxY: int32): bool =
  let mutable hasIssues = false

  for itemA in items do
    // Don't allow items to spawn in corners
    if (itemA.Pos.X = 1 || itemA.Pos.X = maxX) && (itemA.Pos.Y = 1 || itemA.Pos.Y = maxY) then
      hasIssues <- true

    for itemB in items do
      if itemA <> itemB then

        // Don't allow two objects to start next to each other
        if isAdjacentTo itemA.Pos itemB.Pos then
          hasIssues <- true

  hasIssues

// builds a candidate set of arranged actors
let generate (maxX:int32, maxY:int32, getRandom): Actor array =
  let mutable items: Actor array = buildItemsArray(maxX, maxY, getRandom)

  // It's possible to generate items in invalid starting configurations. Make sure we don't do that.
  while hasInvalidlyPlacedItems(items, maxX, maxY) do
    items <- buildItemsArray(maxX, maxY, getRandom)

  items

// grabs the list of actors and returns a World instance with those actors
let makeWorld maxX maxY random =
  let actors = generate(maxX, maxY, random)
  { MaxX = maxX
    MaxY = maxY
    Squirrel = actors.[0]
    Tree = actors.[1]
    Aspen = actors.[2]
    Acorn = actors.[3]
    Rabbit = actors.[4] }

let getCharacterAtCell(x, y) (world:World) =
  let actorAtCell =
    world.Actors
    |> Seq.tryFind(fun actor -> actor.Pos.X = x && actor.Pos.Y = y)

  match actorAtCell with
  | Some actor -> getChar actor
  | None -> '.'