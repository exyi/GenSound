// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Main
open GenSoundCore
//[<EntryPoint>]
let main argv = 
    ignore (generateSound())
    printfn "%A" argv
    0 // return an integer exit code

