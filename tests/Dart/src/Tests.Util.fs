module Fable.Tests.Dart.Util

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.Dart

[<Erase>]
type FactAttribute() =
    inherit System.Attribute()

type Assertion =
    interface end

[<ImportAll("package:test/test.dart")>]
type Test =
    static member expect(actual: obj, assertion: Assertion): unit = nativeOnly
    static member equals(value: obj): Assertion = nativeOnly
    static member isNot(assertion: Assertion): Assertion = nativeOnly
    static member throwsException: Assertion = nativeOnly
#else
open Xunit

type FactAttribute = Xunit.FactAttribute
#endif

let runAsyncTest (f: unit -> Async<unit>) =
#if FABLE_COMPILER
    f ()
#else
    Async.RunSynchronously(f ())
#endif

let equal<'T> (expected: 'T) (actual: 'T): unit =
#if FABLE_COMPILER
    Test.expect(actual, Test.equals(expected))
#else
    Assert.Equal(expected, actual)
#endif

let notEqual<'T> (expected: 'T) (actual: 'T): unit =
#if FABLE_COMPILER
    Test.expect(actual, Test.isNot(Test.equals(expected)))
#else
    Assert.NotEqual(expected, actual)
#endif

let private run (f: unit -> 'a) =
    try
        f ()
        |> Ok
    with e ->
        Error e.Message

module private RunResult =
    let wasAnyError actual =
        match actual with
        | Error _ -> ()
        | Ok value -> equal (Error ()) (Ok value)

    let wasError expected actual =
        match actual with
        | Error _ when System.String.IsNullOrEmpty expected -> ()
        | Error actual -> equal (Error expected) (Error actual)
        | Ok value -> equal (Error expected) (Ok value)

    let wasErrorContaining expected actual =
        match actual with
        | Error _ when System.String.IsNullOrEmpty expected -> ()
        | Error (actual: string) when actual.Contains expected -> ()
        | Error actual -> equal (Error expected) (Error actual)
        | Ok value -> equal (Error expected) (Ok value)

let throwsAnyError (f: unit -> 'a): unit =
#if FABLE_COMPILER
    Test.expect(f, Test.throwsException)
#else
    run f |> RunResult.wasAnyError
#endif

let throwsError (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasError expected

/// While `throwsError` tests exception message for equality,
/// this checks if error message CONTAINS passed message
let throwsErrorContaining (expected: string) (f: unit -> 'a): unit =
    run f
    |> RunResult.wasErrorContaining expected
