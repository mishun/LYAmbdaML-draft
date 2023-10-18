module LYAmbdaML.Test.Main

open System
open Expecto


[<EntryPoint>]
let main argv =
    //TestCombinators.customCheck() ; 0
    runTestsInAssemblyWithCLIArgs [] argv
