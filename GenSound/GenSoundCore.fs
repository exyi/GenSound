module GenSoundCore
open Fable.Core
open Func
open Types
open Pattern
open GenMusic


let rythm = getRythm 4.0 2.9 3
let cMajor = [|0;2;4;5;7;9;11|]
let cMinor = [|0;2;3;5;7;8;10|]

let scaleSettings = {
    IntervalWeights = [| 1.0; 5.0; 5.0; 3.0; 3.0; 0.7; 0.7; 1.0 |]
    ToneWeights =     [| 1.7; 1.5; 1.6; 1.0; 1.6; 1.5; 1.0 |]
    }

let patternSettings: PatternSettings = {
    Length = [| 1 |]
    Weights = [| 1. |]
    divCoef = 3.0
    recursionCoef = 2.5
    }

//let melody = getMelody rythm scaleSettings 20 |> Seq.map (fun x -> [|x|]) |> createNotes rythm
let melody = createMusic 50 20 scaleSettings patternSettings

let x = max 2 1