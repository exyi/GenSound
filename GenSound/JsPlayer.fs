module JsPlayer
open GenSoundCore
open Fable.Core
open Fable.Core.JsInterop

[<Emit("window.setTimeout($1, $0)")>]
let setTimeout time callback = System.Threading.Tasks.Task.Delay(System.TimeSpan.FromMilliseconds(time)).ContinueWith(fun x -> callback()) |> ignore

type SynthInfo = { name: string }
[<Emit("MIDI.loadPlugin({instrument: $0, onsuccess: $1})")>]
let init instrument callback = ()

[<Emit("MIDI.noteOn($0, $1, $2, $3)")>]
let noteOn (channel:int) (note:int) (volume:int) (delay:float)  =
    ()

[<Emit("MIDI.noteOff($0, $1, $2)")>]
let noteOff (channel:int) (note:int) (delay:float)  =
    ()

[<Emit("console.log($0)")>]
let log (string:obj) = ()

let playStream notes speed =
    let convertNote num =
        let names = [|"C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B"|]
        (names.[num %% names.Length], num / names.Length + 3)

    let times = notes |> Seq.map (fun x -> x.Length) |> Seq.scan (+) 1.
    for (time, {Length=length; Notes= notes}) in (notes |> Seq.zip times) do
        for note in notes do
            noteOn 0 (note + 48) 128 (speed * time)
            noteOff 0 (note + 48) (speed * (time + length))


log melody

let notes = melody 
            |> Seq.map (convertTone cMinor)

init "acoustic_grand_piano" (fun () -> 
    log "piano loaded"
    playStream notes 2.0
    )


