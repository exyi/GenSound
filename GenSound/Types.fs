module Types


type ScaleSettings  = { IntervalWeights: float array; ToneWeights: float array}
type PatternSettings  = { Length: int array; Weights: float array; divCoef: float; recursionCoef: float }
type Note = { Length: float; Notes: int array }
type Pattern = { Notes: Note array; Speed: float; Pitch: float }