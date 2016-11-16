module GenSoundCore
open Fable.Core

let random = new System.Random()
[<Emit("Math.random()")>]
let randomReal () = random.NextDouble()
type ScaleSettings  = { JumpWeights: float array; ToneWeights: float array; IntervalWeights: float array }
type Note = { Length: float; Notes: int array }
let createNotes rythm melody = Seq.zip melody rythm |> Seq.map (fun (m,r) -> { Length = r; Notes = m }) |> Seq.toArray


let inline (%%) n m = ((n % m) + m) % m

let pickRandom weights elements =
    let totalW = Seq.sum weights
    let num = randomReal ()  * totalW
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

        let possibleTones = [-scaleSettings.JumpWeights.Length + 1 + previous .. scaleSettings.JumpWeights.Length - 1 + previous]
        let allIntervalWeights = possibleTones |> Seq.map (fun tone ->
                                                           let interval = abs (tone - previous)
                                                           scaleSettings.JumpWeights.[interval])
        let allToneWeights = possibleTones |> Seq.map(fun i -> scaleSettings.ToneWeights.[i %% scaleSettings.ToneWeights.Length])
        let totalWeights = 
            Seq.map2 (*) allIntervalWeights (allToneWeights |> Seq.map (fun x -> x ** toneImportance)) // multiply w_tone with w_interval, power to toneImportance
            |> Seq.mapi (fun i weight ->
                 let tone = possibleTones.[i]
                 if abs tone < abs previous then weight else weight * (toneReturnCoef (float (abs tone) / float toneRange))) // reduce probability of going too far from origin
            |> Seq.mapi (fun i weight ->
                let tone = previous - possibleTones.[i]
                if jumpBar >= abs tone then weight else weight / (abs tone - jumpBar |> float |> (*)0.1 |> (+)1. |> exp)) // reduce probability of jumps over jumpbar
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

let detectHarmony (melody: Note seq) scaleSettings =
    let rateChord tone chord (intervalWeights:float array) =
        chord |> Seq.map (fun tt -> intervalWeights.[abs (tone - tt) %% intervalWeights.Length]) |> Seq.sum
    
    let chords = [|0..scaleSettings.ToneWeights.Length - 1|] |> Array.map ((+) -8) |> Array.map (fun x -> Array.map ((+)x) [|0;2;4|])

    let getWeightedChords {Length=len;Notes=notes} =
        chords |> Array.map (fun ch ->
            notes |> Seq.map (fun note -> rateChord note ch scaleSettings.IntervalWeights) |> Seq.min)
    
    // choose one chord per tact
    // TODO: better chord timing
    let r = melody |> Seq.scan (fun l n -> l + n.Length) 0.0 |> Seq.skip 1
            |> Seq.map floor // compute tact index
            |> Seq.zip melody
            |> Seq.groupBy snd // group notes by tact
            |> Seq.map (fun (tactIndex,notes) ->
                let weights = notes |> Seq.map fst |> Seq.map (fun n -> Array.map ((*)n.Length) (getWeightedChords n)) |> Seq.fold (Array.map2 (+)) (Array.replicate chords.Length 0.0)
                let (chord,_) = Seq.zip chords weights |> Seq.maxBy snd
                chord
            )

    r |> Seq.map (fun notes -> {Note.Notes = notes; Length = 1.0}) |> Seq.toArray

let convertTone (key: int array) {Length=len; Notes=notes} =
    {Length=len; Notes = notes |> Array.map(fun tone -> key.[tone %% key.Length] + 12 * (tone / key.Length))}

let scaleSettings = {
    JumpWeights =     [| 1.0; 5.0; 5.0; 3.0; 3.0; 0.7; 0.7; 1.0 |]
    ToneWeights =     [| 1.7; 1.5; 1.6; 1.0; 1.6; 1.5; 1.0 |]
    IntervalWeights = [| 5.0; 1.0; 3.0; 2.0; 4.0; 1.0; 0.4 |]
    }

let rythm = getRythm 4.0 2.9 3
let cMajor = [|0;2;4;5;7;9;11|]
let cMinor = [|0;2;3;5;7;8;10|]

let melody = getMelody rythm scaleSettings 20 |> Seq.map (fun x -> [|x|]) |> createNotes rythm
let chords = detectHarmony melody scaleSettings


let x = max 2 1