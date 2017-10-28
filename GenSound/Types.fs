module Types


type ScaleSettings  = { JumpWeights: float array; ToneWeights: float array; IntervalWeights: float array }
type Note = { Length: float; Notes: int array }