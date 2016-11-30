module Ploeh.Numsense.IndonesianExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("nol", 0)>]
let ``tryParseIndonesian returns correct result`` (indonesian, expected) = 
  let actual = Numeral.tryParseIndonesian indonesian
  Some expected =! actual

[<Theory>]
[<InlineData(0, "nol")>]
let ``toIndonesian returns correct result`` (i, expected) =
    let actual = Numeral.toIndonesian i
    expected =! actual