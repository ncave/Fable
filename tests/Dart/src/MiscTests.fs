module Fable.Tests.Dart.Misc

open System
open Fable.Core
open Util

let increase (x: int ref) =
    x.Value <- x.Value + 1

type MaybeBuilder() =
  member _.Bind(x,f) = Option.bind f x
  member _.Return v = Some v
  member _.ReturnFrom o = o
let maybe = MaybeBuilder()

let riskyOp x y =
  if x + y < 100 then Some(x+y) else None

let execMaybe a = maybe {
  let! b = riskyOp a (a+1)
  let! c = riskyOp b (b+1)
  return c
}

let applyUncurried f x y =
    f x y

let getCurrriedFunctionInTuple (tup: int * (int -> int -> int)) y =
    let x, f = tup
    applyUncurried f x y

let getCurrriedFunctionInTuple2 (tup: int * (int -> int -> int)) =
    let x, f = tup
    applyUncurried f x

let getCurrriedGenericFunctionInTuple (tup: int * (int -> 'a)) =
    let x, f = tup
    f x

[<Measure>] type km           // Define the measure units
[<Measure>] type mi           // as simple types decorated
[<Measure>] type h            // with Measure attribute

[<Measure>] type Measure1
[<Measure>] type Measure2 = Measure1

type MeasureTest() =
    member _.Method(x: float<Measure2>) = x

// Can be used in a generic way
type Vector3D<[<Measure>] 'u> =
    { x: float<'u>; y: float<'u>; z: float<'u> }
    static member (+) (v1: Vector3D<'u>, v2: Vector3D<'u>) =
        { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z }

type UncurryUnion =
    | UncurryUnion of add: (int -> int -> int)

let applyUncurryUnion x y = function
    | UncurryUnion f -> f x y

type ElmishWidget<'Model, 'View, 'Msg>(view: 'Model -> ('Msg -> unit) -> 'View) =
    let _view = view
    member _.Render(model, dispatch) = _view model dispatch

[<Fact>]
let ``ref works`` () =
    let x = ref 5
    increase x
    x.Value |> equal 6

[<Fact>]
let ``for-in descending range with -1 step works`` () =
    let mutable result = ""
    for i in 5 .. -1 .. 1 do
        result <- result + string i
    result |> equal "54321"

[<Fact>]
let ``for-in ascending range with 1 step works`` () =
    let mutable sum = 0
    for i in 1 .. 1 .. 5 do
        sum <- sum + i
    sum |> equal 15

[<Fact>]
let ``for-in const-step range with zero iterations works`` () =
    let mutable count = 0
    for _i in 0 .. -1 .. 5 do
        count <- count + 1
    count |> equal 0

[<Fact>]
let ``for-in step range other than one still works`` () =
    let mutable result = ""
    for i in 9 .. -2 .. 0 do
        result <- result + string i
    result |> equal "97531"

[<Fact>]
let ``Custom computation expressions work`` () =
    execMaybe 5 |> equal (Some 23)
    execMaybe 99 |> equal None

[<Fact>]
let ``Can pass a curried function in a tuple to another function as argument`` () =
    let f = getCurrriedFunctionInTuple (20, (/))
    f 6 |> equal 3

    getCurrriedFunctionInTuple (25, (/)) 6 |> equal 4

    let f = getCurrriedFunctionInTuple2 (4, (+))
    f 6 |> equal 10

    let f2 = getCurrriedGenericFunctionInTuple (4, fun x y z -> x + y * z)
    f2 5 7 |> equal 39

[<Fact>]
let ``Units of measure work`` () =
    3<km/h> + 2<km/h> |> equal 5<km/h>

    let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
    let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
    let v3 = v1 + v2
    equal 8.8<mi> v3.y

[<Fact>]
let ``Units of measure work with longs`` () =
    3L<km/h> + 2L<km/h> |> equal 5L<km/h>

// TODO: decimals
// [<Fact>]
// let ``Units of measure work with decimals`` () =
//     3M<km/h> + 2M<km/h> |> equal 5M<km/h>

[<Fact>]
let ``Abbreviated measures work`` () =
    // #2313
    let x = 5.<Measure1>
    let c = MeasureTest()
    c.Method(5.<Measure2>) |> equal x

[<Fact>]
let ``Can get uncurried functions from unions`` () =
    UncurryUnion (-) |> applyUncurryUnion 5 2 |> equal 3

[<Fact>]
let ``Class fields are uncurried`` () =
    let mutable count = 1.2
    let w = ElmishWidget<int, string, float>(fun model dispatch ->
        dispatch 0.5
        (model + 5).ToString())
    w.Render(7, fun f -> count <- count + f) |> equal "12"
    equal 1.7 count

[<Fact>]
let ``nullArgCheck don_u27_t throw exception if argument is not null`` () =
    let expected = "hello"
    let value = nullArgCheck "arg1" expected
    equal expected value

// [<Fact>]
// let ``nullArgCheck throws exception if argument is null`` () =
//     throwsAnyError (fun () ->
//         nullArgCheck<string> "str" null
//     )

[<Fact>]
let ``invalidArg formats the message like .NET`` () =
    throwsError "This is invalid (Parameter 'arg')" (fun () ->
        invalidArg "arg" "This is invalid"
    )

[<Fact>]
let ``Compiler target flags have correct value per target`` () =
    equal false Compiler.isJavaScript
    equal false Compiler.isTypeScript
    equal false Compiler.isPython
#if FABLE_COMPILER_DART
    equal true Compiler.isDart
#else
    equal false Compiler.isDart
#endif
    equal false Compiler.isRust
#if FABLE_COMPILER
    equal false Compiler.isDotnet
#else
    equal true Compiler.isDotnet
#endif
    equal false (Compiler.isJavaScript || Compiler.isTypeScript)

[<Fact>]
let ``Compiler target flags eliminate dead branches`` () =
    let target =
        if Compiler.isJavaScript then "javascript"
        elif Compiler.isTypeScript then "typescript"
        elif Compiler.isPython then "python"
        elif Compiler.isDart then "dart"
        elif Compiler.isRust then "rust"
        else "dotnet"
#if FABLE_COMPILER_DART
    equal "dart" target
#else
    equal "dotnet" target
#endif

let tests() =
    ``ref works`` ()
    ``for-in descending range with -1 step works`` ()
    ``for-in ascending range with 1 step works`` ()
    ``for-in const-step range with zero iterations works`` ()
    ``for-in step range other than one still works`` ()
    ``Custom computation expressions work`` ()
    ``Can pass a curried function in a tuple to another function as argument`` ()
    ``Units of measure work`` ()
    ``Units of measure work with longs`` ()
    ``Abbreviated measures work`` ()
    ``Can get uncurried functions from unions`` ()
    ``Class fields are uncurried`` ()
    ``nullArgCheck don_u27_t throw exception if argument is not null`` ()
    ``invalidArg formats the message like .NET`` ()
    ``Compiler target flags have correct value per target`` ()
    ``Compiler target flags eliminate dead branches`` ()
