open System.Numerics

let prettyNumber (n:float) =
  let t = truncate n
  let s = n - t
  match s with
  | 0.0 -> sprintf "%d" (int t)
  | _ -> sprintf "%.3f" n

let prettyComplex (c:complex) =
  match c.r, c.i with
  | r,    0.0 -> prettyNumber r
  | 0.0,  1.0 -> "i"
  | 0.0, -1.0 -> "-i"
  | 0.0,    i -> prettyNumber i + "i"
  | r,    1.0 -> prettyNumber r + " + i"
  | r,   -1.0 -> prettyNumber r + " - i"
  | r,      i when i >= 0.0
              -> prettyNumber r + " + " + prettyNumber i + "i"
  | r,      i -> prettyNumber r + " - " + prettyNumber (abs i) + "i"

type QuantumState (a: complex, l: string) =
  member x.Amplitude = a
  member x.Label = l
  override x.ToString() =
    let f(a: complex) =
      if a.r <> 0.0 && a.i <> 0.0
        then "(" + prettyComplex a + ")"
        else prettyComplex a
    f(x.Amplitude) + "|" + x.Label + ">"

type QS = QuantumState
let QS a l = QS (a, l)

let toPair (qs: QS) = (qs.Amplitude, qs.Label)

let fromPair (a:complex) (l:string) = QS a l

let applyQS f (qs: QS) = QS (f qs.Amplitude) qs.Label

[<EntryPoint>]
let main argv =
  printfn "%A" <| QS (complex (sqrt(0.5)) 0.0) "+"
  printfn "%A" <| QS (complex 0.0 (sqrt(0.5))) "+"
  printfn "%A" <| QS (complex 1.0 0.0) "+"
  printfn "%A" <| QS (complex -1.0 0.0) "+"
  printfn "%A" <| QS (complex 0.0 1.0) "+"
  printfn "%A" <| QS (complex 0.0 -1.0) "+"
  printfn "%A" <| QS (complex 1.0 -1.0) "+"
  printfn "%A" <| QS (complex 0.0 0.0) "+"
  printfn "%A" <| QS (complex (sqrt(0.5)) (sqrt(0.5))) "+"
  printfn "%A" <| QS (complex -(sqrt(0.5)) (sqrt(0.5))) "+"
  printfn "%A" <| QS (complex (sqrt(0.5)) -(sqrt(0.5))) "+"
  printfn "%A" <| QS (complex -(sqrt(0.5)) -(sqrt(0.5))) "+"
  0 // return an integer exit code
