module GenSoundCore
open Fable.Core
open Func
open Types
open Pattern
open GenMusic


let rythm = getRythm 4.0 2.9 3
let cMajor = [|0;2;4;5;7;9;11|]
let cMinor = [|0;2;3;5;7;8;10|]

//let melody = getMelody rythm scaleSettings 20 |> Seq.map (fun x -> [|x|]) |> createNotes rythm
let melody = createMusic 50 20 scaleSettings
let chords = detectHarmony melody scaleSettings


let x = max 2 1