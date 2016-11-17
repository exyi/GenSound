module Func

open Fable.Core
open Types

let random = new System.Random()
[<Emit("Math.random()")>]
let randomReal () = random.NextDouble()



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

let toneReturnCoef x = 1. - min 1. ((max 0. (x - 0.4)) ** 2.0)
    // reduce probability of going too far from origin, p = 1 for 0 dist, p > 0.95 for 0.6 range, p ~= 0.7 for 1 range
