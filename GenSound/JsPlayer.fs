module JsPlayer
open Types
open Func
open Pattern
open Harmony
open GenMusic
open GenSoundCore
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

[<Emit("typeof ($1)")>]
let typeof obj : string = jsNative

[<Emit("this")>]
let getJsThis () = jsNative

[<Emit("$0")>]
let dynamicConvert ff = jsNative

[<Emit("Vue")>]
let Vue : vuejs.VueStatic = jsNative

[<Emit("Object.getOwnPropertyNames($0)")>]
let Object_getOwnPropertyNames obj : string array = jsNative 

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

[<Emit("createMidiUrl($0)")>]
let createMidiUrl (notes: Note [] []) = ""

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

type VoiceVM = {
    Notes: Note[]
    InstrumentName: string
}
type SongVM = {
    Length: float
    Voices: VoiceVM array
    GeneratedNotes: Note [] []
}
type PlaybackViewModel = {
    IsPlaying: bool
    CurrentTime: float
}
type ViewModel = {
    Song: SongVM option
    Player: PlaybackViewModel
}

let createSong () =
    let voices = generateSound ()
    {
        SongVM.Length = voices |> Seq.map(Seq.sumBy (fun {Note.Length = len} -> len)) |> Seq.max
        Voices = voices |> Array.map (fun v -> {VoiceVM.Notes=v;InstrumentName="piano"})
        GeneratedNotes = voices
    }

let createViewModel () = {ViewModel.Song=None;Player={PlaybackViewModel.IsPlaying=false;CurrentTime=0.0}}

// these return new instance of VM
let methods = [
    ("play", fun x ->
        { x with Player={x.Player with IsPlaying=true}})
    ("pause", fun x ->
        { x with Player={x.Player with IsPlaying=false}})
    ("generate", fun x ->
        { x with Song = Some (createSong ())})
]

// these return some value that can be used in binding
let computed = [
    ("getMidiHref", function
                    | {Song=Some {GeneratedNotes=notes}} -> createMidiUrl notes |> Some
                    | _ -> None)
]

// Javascript

let applyVM (oldVM) (newVM:obj) = 
    for prop in Object_getOwnPropertyNames newVM do
        Vue.set (oldVM, prop, newVM?(prop)) |> ignore

let createObject (map: (seq<string * _>)) =
    let o = obj()
    for (name, v) in map do
        o?(name) <- v
    o

let vueMethods

//let mutable vue : vuejs.Vue = null
//vue <- vuejs.createVue "#app" vm (createObject (methods |> Seq.map (fun (n, m) -> n, (fun arg -> applyVM vue (m (dynamicConvert vue))))))

//let notes = melody |> Array.map (convertTone cMinor)
//let chordNotes = chords |> Array.map (convertTone cMinor)
//
//init "acoustic_grand_piano" (fun () -> 
//    log "piano loaded"
//    playStream notes 2.0
//    playStream chordNotes 2.0
//    )
