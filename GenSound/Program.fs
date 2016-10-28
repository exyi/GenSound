// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
let random = new System.Random()
type ScaleSettings  = { IntervalWeights: float array; ToneWeights: float array}

let inline (%%) n m = ((n % m) + m) % m

let pickRandom weights elements =
    let totalW = Seq.sum weights
    let num = random.NextDouble() * totalW
    Seq.scan (+) 0. weights // accumulate weights by (+)
    |> Seq.skip 1
    |> Seq.zip elements
    |> Seq.find(fun (e, w) -> w > num) // get first with accumulate weight > num
    |> fst

let normalDistribution μ σ x =
    (2. * σ ** 2. * System.Math.PI) ** -0.5 * System.Math.E ** -((x - μ) ** 2. / (2. * σ ** 2.))
let sigmoid value = 1.0/(1.0 + exp(-value))

let toneReturnCoef x = 1. - min 1. ((max 0. (x - 0.4)) ** 2.0) // reduce probability of going too far from origin, p = 1 for 0 dist, p > 0.95 for 0.6 range, p ~= 0.7 for 1 range
let getMelody rythm scaleSettings toneRange =
    let chooseTone previous toneImportance jumpBar =

        let possibleTones = [-scaleSettings.IntervalWeights.Length + 1 + previous .. scaleSettings.IntervalWeights.Length - 1 + previous]
        let allIntervalWeights = possibleTones |> Seq.map (fun tone ->
                                                           let interval = abs (tone - previous)
                                                           scaleSettings.IntervalWeights.[interval])
        let allToneWeights = possibleTones |> Seq.map(fun i -> scaleSettings.ToneWeights.[i %% scaleSettings.ToneWeights.Length])
        let totalWeights = 
            Seq.map2 (*) allIntervalWeights (allToneWeights |> Seq.map (( ** )toneImportance)) // multiply w_tone with w_interval, power to toneImportance
            |> Seq.mapi (fun i weight ->
                 let tone = possibleTones.[i]
                 if abs tone < abs previous then weight else weight * (toneReturnCoef (float (abs tone) / float toneRange))) // reduce probability of going too far from origin
            |> Seq.mapi (fun i weight ->
                let tone = previous - possibleTones.[i]
                weight * (jumpBar - abs tone |> float |> (/)3.0 |> sigmoid)) // reduce probability of jumps over jumpbar
            |> Seq.toArray
        let resultTone = pickRandom totalWeights possibleTones
        // TODO: introduce interval
        resultTone

    let startingTone = 
        // pickRandom [1;1;1] [0;2;4] // reimplementation
        chooseTone 0 5.0 10
    rythm 
        |> Seq.scan (fun (jumpBar, previous) toneLength ->
            let next = chooseTone previous (1. + toneLength) jumpBar
            (jumpBar - abs (next - previous) + 3, next)) (2, startingTone)
        |> Seq.map snd

let getRythm divCoef recursionCoef length =
    let rec getTactRythm divCoef recursionCoef =
        if random.NextDouble() > divCoef then [1.0]
        else List.init 2 (fun i -> getTactRythm (divCoef/recursionCoef) recursionCoef) // recurse to two tacts
                |> List.collect id // collect them to one list
                |> List.map (fun l -> l / 2.0) // divide length by 2
    List.init length (fun i -> getTactRythm divCoef recursionCoef) |> List.collect id

let scaleSettings = {
    IntervalWeights = [| 1.0; 5.0; 5.0; 3.0; 3.0; 0.7; 0.7; 1.0 |]
    ToneWeights =     [| 1.7; 1.5; 1.6; 1.0; 1.6; 1.5; 1.0 |]
    } 

let rythm = getRythm 2.0 2.0 3
let melody = getMelody rythm scaleSettings 20 |> Seq.toArray

let x = max 2 1

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code