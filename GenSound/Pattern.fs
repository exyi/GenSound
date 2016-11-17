module Pattern

open Func
open Types

let getMelody rythm scaleSettings toneRange =
    let chooseTone previous toneImportance jumpBar =

        let possibleTones = [-scaleSettings.IntervalWeights.Length + 1 + previous ..  
                    scaleSettings.IntervalWeights.Length - 1 + previous]
        let allIntervalWeights = possibleTones |> Seq.map (fun tone ->
                                                           let interval = abs (tone - previous)
                                                           scaleSettings.IntervalWeights.[interval])
        let allToneWeights = possibleTones |> Seq.map(fun i ->
                    scaleSettings.ToneWeights.[i %% scaleSettings.ToneWeights.Length])
        let totalWeights = 
            Seq.map2 (*) allIntervalWeights (allToneWeights |> Seq.map (fun x -> x ** toneImportance))
                    // multiply w_tone with w_interval, power to toneImportance
            |> Seq.mapi (fun i weight ->
                 let tone = possibleTones.[i]
                 if abs tone < abs previous then weight
                        else weight * (toneReturnCoef (float (abs tone) / float toneRange)))
                        // reduce probability of going too far from origin
            |> Seq.mapi (fun i weight ->
                 let tone = previous - possibleTones.[i]
                 if jumpBar >= abs tone then weight
                        else weight / (abs tone - jumpBar |> float |> (*)0.1 |> (+)1. |> exp))
                        // reduce probability of jumps over jumpbar
            |> Seq.toArray
        let resultTone = pickRandom totalWeights possibleTones
        // TODO: introduce interval
        resultTone

    let startingTone = 
         pickRandom [1.;1.;1.] [0;2;4] // reimplementation
//        chooseTone 0 5.0 10
    rythm 
        |> Seq.scan (fun (jumpBar, previous) toneLength ->
            let next = chooseTone previous (1. + toneLength) jumpBar
            (jumpBar - abs (next - previous) + 4, next)) (2, startingTone)
        |> Seq.map snd



let getRythm divCoef recursionCoef length =
    let rec getTactRythm divCoef recursionCoef =
        if randomReal() > divCoef then [1.0]
        else List.init 2 (fun i -> getTactRythm (divCoef/recursionCoef) recursionCoef) // recurse to two tacts
                |> List.collect id // collect them to one list
                |> List.map (fun l -> l / 2.0) // divide length by 2
    List.init length (fun i -> getTactRythm divCoef recursionCoef) |> List.collect id

let getSpeed (noteArray: Note array) =
    let mutable speed = 0.
    for i in 0 .. noteArray.Length-1 do
        speed <- speed + noteArray.[i].Length
    speed
    
let getPitch (noteArray: Note array) =
    let mutable pitch = 0.
    for i in 0 .. noteArray.Length-1 do
        pitch <- pitch + Seq.averageBy (fun elem -> float elem) noteArray.[i].Notes
    pitch

let createPattern noteArray = { Pattern.Notes = noteArray; Pitch = getPitch noteArray ; Speed = getSpeed noteArray }

let getPatterns  scaleSettings count patternSettings =
    let rythm = (getRythm patternSettings.divCoef patternSettings.recursionCoef
                    (pickRandom patternSettings.Weights patternSettings.Length))
    let Patterns = 
        List.init count
                (fun i -> getMelody rythm scaleSettings 20
                        |> Seq.map (fun x -> [|x|]) |> createNotes rythm |> createPattern)
    
    Patterns