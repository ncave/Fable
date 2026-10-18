module Fable.Tests.Dart.Quotation

open Util
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Linq.RuntimeHelpers

type C =
    | A of int
    | B

type Cfg =
    static member Version = 42

type MutableRec = { mutable Value: int }


// DerivedPatterns: AndAlso and OrElse recover the shape `&&` and `||` desugar
// into, and SpecificCall matches a call by the identity of a template quotation
// rather than by its compiled name. All three previously reported
// "not supported by Fable".
type private DpRec = { Country: string; Balance: float; Id: int }

let rec private dpToSql (e: Expr) : string =
    match e with
    | Lambda(_, body) -> dpToSql body
    | AndAlso(l, r) -> "(" + dpToSql l + " AND " + dpToSql r + ")"
    | OrElse(l, r) -> "(" + dpToSql l + " OR " + dpToSql r + ")"
    | SpecificCall <@ (=) @> (_, _, [ l; r ]) -> "(" + dpToSql l + " = " + dpToSql r + ")"
    | SpecificCall <@ (>) @> (_, _, [ l; r ]) -> "(" + dpToSql l + " > " + dpToSql r + ")"
    | SpecificCall <@ (<) @> (_, _, [ l; r ]) -> "(" + dpToSql l + " < " + dpToSql r + ")"
    // Bound as a wildcard: PropertyGet exposes a PropertyInfo on Rust but the
    // bare name on the other targets, and this test is about DerivedPatterns.
    | PropertyGet _ -> "col"
    | Value _ -> "?"
    | _ -> "<unsupported>"

[<Fact>]
let ``Simple integer value quotation`` () =
    let q = <@ 42 @>
    match q with
    | Value(v, _t) -> equal 42 (v :?> int)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``Boolean value quotation`` () =
    let q = <@ true @>
    match q with
    | Value(v, _t) -> equal true (v :?> bool)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``String value quotation`` () =
    let q = <@ "hello" @>
    match q with
    | Value(v, _t) -> equal "hello" (v :?> string)
    | _ -> failwith "Expected Value"

[<Fact>]
let ``Lambda quotation`` () =
    let q = <@ fun x -> x + 1 @>
    match q with
    | Lambda(v, _body) -> equal "x" v.Name
    | _ -> failwith "Expected Lambda"

[<Fact>]
let ``Let binding quotation`` () =
    // NOTE: uses a unique binder name (`letv`) instead of the canonical `x`.
    // All Dart tests share one `tests()` function, so reusing `x` across test
    // bodies makes Fable rename later occurrences (e.g. `x` -> `x_1`), which the
    // quotation faithfully reports. A unique name avoids the collision rename.
    let q = <@ let letv = 5 in letv @>
    match q with
    | Let(v, Value(value, _), _) ->
        equal "letv" v.Name
        equal 5 (value :?> int)
    | _ -> failwith "Expected Let"

[<Fact>]
let ``IfThenElse quotation`` () =
    let q = <@ if true then 1 else 2 @>
    match q with
    | IfThenElse(_, _, _) -> ()
    | _ -> failwith "Expected IfThenElse"

[<Fact>]
let ``Application quotation`` () =
    let q = <@ (fun x -> x) 42 @>
    match q with
    | Application(Lambda _, Value _) -> ()
    | _ -> failwith "Expected Application of Lambda"

[<Fact>]
let ``Evaluate simple value`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 42 @>
    equal 42 (result :?> int)

[<Fact>]
let ``Evaluate boolean value`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ true @>
    equal true (result :?> bool)

[<Fact>]
let ``Evaluate string value`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ "hello" @>
    equal "hello" (result :?> string)

[<Fact>]
let ``Evaluate addition`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 1 + 2 @>
    equal 3 (result :?> int)

[<Fact>]
let ``Evaluate let binding`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 10 in x + 5 @>
    equal 15 (result :?> int)

[<Fact>]
let ``Evaluate if then else true branch`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ if true then 1 else 2 @>
    equal 1 (result :?> int)

[<Fact>]
let ``Evaluate if then else false branch`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ if false then 1 else 2 @>
    equal 2 (result :?> int)

[<Fact>]
let ``Evaluate lambda application`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> x + 1) 5 @>
    equal 6 (result :?> int)

[<Fact>]
let ``NewTuple quotation`` () =
    let q = <@ (1, 2, 3) @>
    match q with
    | NewTuple(exprs) -> equal 3 exprs.Length
    | _ -> failwith "Expected NewTuple"

[<Fact>]
let ``Sequential quotation`` () =
    let q = <@ (); 42 @>
    match q with
    | Sequential(_, Value(v, _)) -> equal 42 (v :?> int)
    | _ -> failwith "Expected Sequential"

[<Fact>]
let ``Var quotation`` () =
    let q = <@ fun x -> x @>
    match q with
    | Lambda(v, Var(v2)) ->
        equal v.Name v2.Name
    | _ -> failwith "Expected Lambda with Var body"

[<Fact>]
let ``Evaluate subtraction`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 10 - 3 @>
    equal 7 (result :?> int)

[<Fact>]
let ``Evaluate multiplication`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 6 * 7 @>
    equal 42 (result :?> int)

[<Fact>]
let ``Evaluate comparison`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 5 > 3 @>
    equal true (result :?> bool)

[<Fact>]
let ``Evaluate nested let bindings`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ let x = 3 in let y = 4 in x * y @>
    equal 12 (result :?> int)

[<Fact>]
let ``Evaluate nested lambda`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ (fun x -> (fun y -> x + y)) 3 4 @>
    equal 7 (result :?> int)


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Evaluate tuple`` () =
    // NOTE: the canonical suite casts the result to `(int * int)`, but Dart
    // reifies generic type arguments, so `obj :?> (int * int)` fails for a
    // tuple built from dynamic values. The runtime evaluates a tuple to an
    // obj[] (mirroring the TS/JS runtime), so we inspect the elements.
    let result = LeafExpressionConverter.EvaluateQuotation <@ (1, 2) @>
    let arr = result :?> obj[]
    equal 1 (arr.[0] :?> int)
    equal 2 (arr.[1] :?> int)

[<Fact>]
let ``Expr.GetFreeVars returns empty for closed expr`` () =
    let q = <@ fun x -> x + 1 @>
    let freeVars = q.GetFreeVars() |> Seq.length
    equal 0 freeVars

[<Fact>]
let ``Option match deconstructs to IfThenElse`` () =
    let q = <@ fun (o: int option) -> match o with Some v -> v | None -> 0 @>
    match q with
    | Lambda(_, IfThenElse(_, _, _)) -> ()
    | _ -> failwith "Expected Lambda with IfThenElse body"

[<Fact>]
let ``Literal match deconstructs to IfThenElse`` () =
    let q = <@ fun (x: int) -> match x with 0 -> "zero" | _ -> "other" @>
    match q with
    | Lambda(_, IfThenElse(_, _, _)) -> ()
    | _ -> failwith "Expected Lambda with IfThenElse body"

[<Fact>]
let ``NewUnionCase quotation _u28_single-field DU_u29_ exposes fields`` () =
    let q = <@ A 7 @>
    match q with
    | NewUnionCase(_uci, args) -> equal 1 (List.length args)
    | _ -> failwith "Expected NewUnionCase"

[<Fact>]
let ``NewUnionCase quotation _u28_no-field DU_u29_ exposes empty fields`` () =
    let q = <@ B @>
    match q with
    | NewUnionCase(_uci, args) -> equal 0 (List.length args)
    | _ -> failwith "Expected NewUnionCase"

// --- Option/List property-getter accessors (ListHead/ListTail/OptionValue) deconstruct
// as PropertyGet, matching real F# quotations ---

[<Fact>]
let ``Option.Value access inside a quotation is a PropertyGet`` () =
    let q = <@ fun (o: int option) -> o.Value @>
    match q with
    | Lambda(_, PropertyGet(Some _, _, [])) -> ()
    | _ -> failwith "Expected Lambda with PropertyGet body"

[<Fact>]
let ``Static property access inside a quotation is a PropertyGet with no instance`` () =
    let q = <@ Cfg.Version @>
    match q with
    | PropertyGet(None, _, []) -> ()
    | _ -> failwith "Expected PropertyGet with no instance"

[<Fact>]
let ``Evaluate exponentiation`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 2.0 ** 3.0 @>
    equal 8.0 (result :?> float)

[<Fact>]
let ``Evaluate exponentiation with negative exponent`` () =
    // Regression test: `_pow` used to loop `b` times, wrongly returning `1.0` here.
    let result = LeafExpressionConverter.EvaluateQuotation <@ 2.0 ** -1.0 @>
    equal 0.5 (result :?> float)

[<Fact>]
let ``Evaluate exponentiation with fractional exponent`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ 4.0 ** 0.5 @>
    equal 2.0 (result :?> float)

[<Fact>]
let ``NewRecord quotation exposes field values`` () =
    let q = <@ { Value = 5 } : MutableRec @>
    match q with
    | NewRecord(_ty, values) -> equal 1 (List.length values)
    | _ -> failwith "Expected NewRecord"


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Evaluate mutable variable set _u28_VarSet_u29_`` () =
    let result = LeafExpressionConverter.EvaluateQuotation <@ let mutable v = 1 in (v <- 2); v @>
    equal 2 (result :?> int)


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Evaluate mutable record field set _u28_FieldSet_u29_`` () =
    let result =
        LeafExpressionConverter.EvaluateQuotation
            <@ let r = { Value = 1 }: MutableRec in r.Value <- 5; r.Value @>

    equal 5 (result :?> int)

[<Fact>]
let ``DerivedPatterns AndAlso_u2c_ OrElse and SpecificCall work`` () =
    dpToSql <@ fun (c: DpRec) -> c.Country = "UK" @> |> equal "(col = ?)"
    dpToSql <@ fun (c: DpRec) -> c.Country = "UK" && c.Balance > 100.0 @>
    |> equal "((col = ?) AND (col > ?))"
    dpToSql <@ fun (c: DpRec) -> c.Id < 5 || c.Balance > 1.0 @>
    |> equal "((col < ?) OR (col > ?))"

let tests() =
    ``Simple integer value quotation`` ()
    ``Boolean value quotation`` ()
    ``String value quotation`` ()
    ``Lambda quotation`` ()
    ``Let binding quotation`` ()
    ``IfThenElse quotation`` ()
    ``Application quotation`` ()
    ``Evaluate simple value`` ()
    ``Evaluate boolean value`` ()
    ``Evaluate string value`` ()
    ``Evaluate addition`` ()
    ``Evaluate let binding`` ()
    ``Evaluate if then else true branch`` ()
    ``Evaluate if then else false branch`` ()
    ``Evaluate lambda application`` ()
    ``NewTuple quotation`` ()
    ``Sequential quotation`` ()
    ``Var quotation`` ()
    ``Evaluate subtraction`` ()
    ``Evaluate multiplication`` ()
    ``Evaluate comparison`` ()
    ``Evaluate nested let bindings`` ()
    ``Evaluate nested lambda`` ()
    ``Evaluate tuple`` ()
    ``Expr.GetFreeVars returns empty for closed expr`` ()
    ``Option match deconstructs to IfThenElse`` ()
    ``Literal match deconstructs to IfThenElse`` ()
    ``NewUnionCase quotation _u28_single-field DU_u29_ exposes fields`` ()
    ``NewUnionCase quotation _u28_no-field DU_u29_ exposes empty fields`` ()
    ``Option.Value access inside a quotation is a PropertyGet`` ()
    ``Static property access inside a quotation is a PropertyGet with no instance`` ()
    ``Evaluate exponentiation`` ()
    ``Evaluate exponentiation with negative exponent`` ()
    ``Evaluate exponentiation with fractional exponent`` ()
    ``NewRecord quotation exposes field values`` ()
    ``Evaluate mutable variable set _u28_VarSet_u29_`` ()
    ``Evaluate mutable record field set _u28_FieldSet_u29_`` ()
    ``DerivedPatterns AndAlso_u2c_ OrElse and SpecificCall work`` ()
