module internal Ploeh.Numsense.Indonesian

open Ploeh.Numsense.InternalDsl

let rec internal toIndonesianImp x =
  
  let simplify prefix factor x =
    let remainder = x % factor
    if remainder = 0
    then prefix
    else sprintf "%s-%s" prefix (toIndonesianImp (remainder))

  let format suffix factor x =
    let prefix = sprintf "%s-%s" (toIndonesianImp (x / factor)) suffix
    simplify prefix factor x

  match x with
    |  x when x < 0 -> sprintf "minus %s" (toIndonesianImp -x)
    |  0 -> "nol"
    |  1 -> "satu"
    |  2 -> "dua"
    |  3 -> "tiga"
    |  4 -> "empat"
    |  5 -> "lima"
    |  6 -> "enam"
    |  7 -> "tujuh"
    |  8 -> "delapan"
    |  9 -> "sembilan"
    | 10 -> "sepuluh"
    | 11 -> "sebelas"
    | Between 12 20 x -> format "-belas" 10 x
    | Between 20 100 x -> format "-puluh" 10 x
    | Between 100 1000 x -> format "-ratus" 10 x
    | Between 1000 10000 x -> format "-ribu" 10 x
    | Between 1000000 10000000 x -> format "-juta" 10 x
    | _ -> format "-miliar" 1000000000 x

let internal tryParseIndonesianImp (x : string) =
  match x with
  | "nol" -> Some 0
  | _ -> None
