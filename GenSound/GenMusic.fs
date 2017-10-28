module GenMusic

open Func
open Types
open Pattern

let convertTone (key: int array) {Length=len; Notes=notes} =
    {Length=len; Notes = notes |> Array.map(fun tone -> key.[tone %% key.Length] + 12 * (tone / key.Length))}

let scaleSettings = {
    JumpWeights =     [| 1.0; 5.0; 5.0; 3.0; 3.0; 0.7; 0.7; 1.0 |]
    ToneWeights =     [| 1.7; 1.5; 1.6; 1.0; 1.6; 1.5; 1.0 |]
    IntervalWeights = [| 5.0; 1.0; 3.0; 2.0; 4.0; 1.0; 0.4 |]
    }

(*    List.init 2 (fun i -> getTactRythm (divCoef/recursionCoef) recursionCoef) // recurse to two tacts
                |> List.collect id // collect them to one list
                |> List.map (fun l -> l / 2.0) // divide length by 2
*)

let createMusic length patternNum scaleSettings =
    let patterns =
        List.init (patternNum / 10) (fun i -> getPatterns scaleSettings 3 10)
        |> List.collect id
    let nextPattern = Array.create patternNum 1.
    let mutable music: Note array = [||]
    for i in 1 .. length do
        music <-
            (pickRandom nextPattern patterns
            |> Array.append music)
    music