module GenMusic

open Func
open Types
open Pattern

let detectHarmony (melody: Note seq) scaleSettings =
    let chords = [|0..scaleSettings.ToneWeights.Length - 2|] |> Array.map (fun x -> Array.map ((+)x) [|0;2;4|])
    ()

let convertTone (key: int array) {Length=len; Notes=notes} =
    {Length=len; Notes = notes |> Array.map(fun tone -> key.[tone %% key.Length] + 12 * (tone / key.Length))}


let createMusic length patternNum scaleSettings patternSettings =
    let mutable nextPattern = Array.create patternNum (Array.create patternNum 1.)
    let pNums = Array.create (patternNum/10) 10
    let patterns =
        List.init pNums.Length (fun i -> getPatterns scaleSettings pNums.[i] patternSettings)
        |> List.collect id
    let mutable x = 0
    for i in 1 .. pNums.Length do
        for j in x + 1 .. x + pNums.[i] do
            for k in x + 1 .. x + pNums.[i] do
                nextPattern.[j].[k] <- (nextPattern.[j].[k] * 10.)
        x <- x + pNums.[i]
    let mutable music: Note array = [||]
    let mutable pos: int = 0
    for i in 1 .. length do
        pos <- pickRandom nextPattern.[pos] {1 .. patternNum}
        music <- (patterns.[pos].Notes |> Array.append music)
    music