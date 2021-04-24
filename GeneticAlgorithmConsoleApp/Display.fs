module GeneticAlgorithmConsoleApp.Display

open System
open GeneticAlgorithm.Logic.World

  let printCell char isLastCell =
    if isLastCell then
      printfn "%c" char
    else
      printf "%c" char

  let displayWorld (world: World) =
    printfn ""
    for y in 1..world.MaxX do
    for x in 1..world.MaxY do   
      let char = world.GetCharacterAtCell(x, y)
      printCell char (x = world.MaxX)

  let getUserInput(): ConsoleKeyInfo =
    printfn ""
    printfn "Press R to regenerate or X to exit"  
    Console.ReadKey(true)  