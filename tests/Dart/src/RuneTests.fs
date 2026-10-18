module Fable.Tests.Dart.Rune

open System.Text
open Fable.Core
open Util

[<Emit("String.fromCharCode($0)")>]
let private stringFromCharCode (_: int) : string =
    nativeOnly

[<Fact>]
let ``Rune.GetRuneAt works with BMP characters`` () =
    let rune = Rune.GetRuneAt("ABC", 1)

    rune.Value |> equal 0x42
    rune.ToString() |> equal "B"

[<Fact>]
let ``Rune.GetRuneAt works with supplementary characters`` () =
    let rune = Rune.GetRuneAt("𠀀", 0)

    rune.Value |> equal 0x20000
    rune.ToString() |> equal "𠀀"

[<Fact>]
let ``Rune.GetRuneAt uses UTF-16 code unit indexes`` () =
    Rune.GetRuneAt("A𠀀B", 0).ToString()
    |> equal "A"

    Rune.GetRuneAt("A𠀀B", 1).ToString()
    |> equal "𠀀"

    Rune.GetRuneAt("A𠀀B", 3).ToString()
    |> equal "B"

[<Fact>]
let ``Rune.GetRuneAt throws when index points inside a surrogate pair`` () =
    throwsAnyError <| fun () ->
        Rune.GetRuneAt("A𠀀B", 2)
        |> ignore

[<Fact>]
let ``Rune.GetRuneAt throws when index is out of range`` () =
    throwsAnyError <| fun () ->
        Rune.GetRuneAt("ABC", -1)
        |> ignore

    throwsAnyError <| fun () ->
        Rune.GetRuneAt("ABC", 3)
        |> ignore


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Rune.GetRuneAt throws for unpaired surrogate characters`` () =
    let highSurrogate = stringFromCharCode 0xD800
    let lowSurrogate = stringFromCharCode 0xDC00

    throwsAnyError <| fun () ->
        Rune.GetRuneAt(highSurrogate, 0)
        |> ignore

    throwsAnyError <| fun () ->
        Rune.GetRuneAt(lowSurrogate, 0)
        |> ignore

[<Fact>]
let ``Rune.Value returns the Unicode scalar value`` () =
    Rune.GetRuneAt("A", 0).Value
    |> equal 0x41

    Rune.GetRuneAt("𠀀", 0).Value
    |> equal 0x20000

[<Fact>]
let ``Rune.Utf16SequenceLength returns the number of UTF-16 code units`` () =
    Rune.GetRuneAt("A", 0).Utf16SequenceLength
    |> equal 1

    Rune.GetRuneAt("𠀀", 0).Utf16SequenceLength
    |> equal 2

[<Fact>]
let ``Rune.Utf16SequenceLength works at the BMP boundary`` () =
    Rune.GetRuneAt("\uFFFF", 0).Utf16SequenceLength
    |> equal 1

    Rune.GetRuneAt("\U00010000", 0).Utf16SequenceLength
    |> equal 2

[<Fact>]
let ``Rune.ToString returns the represented Unicode scalar as a string`` () =
    Rune.GetRuneAt("A", 0).ToString()
    |> equal "A"

    Rune.GetRuneAt("𠀀", 0).ToString()
    |> equal "𠀀"

[<Fact>]
let ``F_u23_ string conversion works with Rune`` () =
    Rune.GetRuneAt("A", 0)
    |> string
    |> equal "A"

    Rune.GetRuneAt("𠀀", 0)
    |> string
    |> equal "𠀀"

[<Fact>]
let ``String.EnumerateRunes enumerates Unicode scalar values`` () =
    "A𠀀B".EnumerateRunes()
    |> Seq.map (fun rune -> rune.Value)
    |> Seq.toList
    |> equal [ 0x41; 0x20000; 0x42 ]

[<Fact>]
let ``String.EnumerateRunes works with Rune string conversion`` () =
    "A𠀀B".EnumerateRunes()
    |> Seq.map string
    |> Seq.toList
    |> equal [ "A"; "𠀀"; "B" ]


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``String.EnumerateRunes replaces unpaired surrogates`` () =
    // .NET String.EnumerateRunes replaces invalid UTF-16 sequences with U+FFFD
    // https://learn.microsoft.com/en-us/dotnet/api/system.text.rune.replacementchar
    let input =
        stringFromCharCode 0xD800
        + "A"
        + stringFromCharCode 0xDC00

    input.EnumerateRunes()
    |> Seq.map _.Value
    |> Seq.toList
    |> equal [ 0xFFFD; 0x41; 0xFFFD ]

let tests () =
    ``Rune.GetRuneAt works with BMP characters`` ()
    ``Rune.GetRuneAt works with supplementary characters`` ()
    ``Rune.GetRuneAt uses UTF-16 code unit indexes`` ()
    ``Rune.GetRuneAt throws when index points inside a surrogate pair`` ()
    ``Rune.GetRuneAt throws when index is out of range`` ()
    ``Rune.GetRuneAt throws for unpaired surrogate characters`` ()
    ``Rune.Value returns the Unicode scalar value`` ()
    ``Rune.Utf16SequenceLength returns the number of UTF-16 code units`` ()
    ``Rune.Utf16SequenceLength works at the BMP boundary`` ()
    ``Rune.ToString returns the represented Unicode scalar as a string`` ()
    ``F_u23_ string conversion works with Rune`` ()
    ``String.EnumerateRunes enumerates Unicode scalar values`` ()
    ``String.EnumerateRunes works with Rune string conversion`` ()
    ``String.EnumerateRunes replaces unpaired surrogates`` ()
