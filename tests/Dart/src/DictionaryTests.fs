module Fable.Tests.Dart.Dictionary

open System
open System.Collections.Generic
open Util

type MyRefType(i: int) =
    member x.Value = i

type MyRefTypeComparer() =
    interface IEqualityComparer<MyRefType> with
        member _.Equals(x, y) = x.Value = y.Value
        member _.GetHashCode(x) = x.Value

type MyRecord = { a: int }

type R = { i: int; s: string }

[<Fact>]
let ``Dictionary KeyValuePattern works`` () =
    // See #509
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    let mutable i = 0.
    for KeyValue(x,y) in dic do
       i <- y + i
    equal 385. i

[<Fact>]
let ``Dictionary creation works`` () =
    let dic = Dictionary<_,_>()
    equal 0 dic.Count

[<Fact>]
let ``Interface IDictionary creation works`` () =
    let dic = dict <| seq { for i in 1. .. 10. -> i, i*i }
    equal 4. dic.[2]

[<Fact>]
let ``Dictionary creation from IDictionary works`` () =
    let idic = dict <| seq { for i in 1 .. 10 -> i, i*i }
    let dic = Dictionary<_,_>(idic)
    dic.Add(100, 100)
    equal 10 idic.Count
    equal 11 dic.Count

[<Fact>]
let ``Dictionaries with IEqualityComparer work`` () =
    let x = MyRefType(4)
    let y = MyRefType(4)
    let z = MyRefType(6)
    let dic = Dictionary<_,_>()
    dic.Add(x, "foo")
    dic.ContainsKey(x) |> equal true
    dic.ContainsKey(y) |> equal false

    let dic2 = Dictionary<_,_>(MyRefTypeComparer())
    dic2.Add(x, "bar")
    dic2.ContainsKey(x) |> equal true
    dic2.ContainsKey(y) |> equal true
    dic2.ContainsKey(z) |> equal false

// FIXME
//    [<Fact>]
//    let ``Interface IDictionary iteration works`` () =
//        let dic = dict <| seq { for i in 1. .. 10. -> i, i*i }
//        let mutable i = 0.
//        for kv in dic do
//           i <- kv.Value + i
//        equal 385. i

[<Fact>]
let ``Interface IDictionary folding works`` () =
    let dic = dict [ ("A", 1.); ("B", 2.); ("C", 3.) ]
    dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 6.

[<Fact>]
let ``Dictionary iteration works`` () =
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    let mutable i = 0.
    for kv in dic do
       i <- kv.Value + i
    i + dic.[1.]
    |> equal 386.

[<Fact>]
let ``Dictionary folding works`` () =
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 385.

[<Fact>]
let ``Dictionary.Count works`` () =
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    dic.Count
    |> equal 10

[<Fact>]
let ``Dictionary indexer works`` () =
    let dic = Dictionary<string,obj>()
    dic.["A"] <- "Hello"
    dic.["B"] <- 2
    dic.["B"].ToString()
    |> equal "2"

[<Fact>]
let ``Dictionary.TryGetValue works`` () =
    let dic1 = dict ["A", 1]
    let dic2 = dict ["B", "2"]
    let success1, val1 = dic1.TryGetValue("A")
    let success2, val2 = dic1.TryGetValue("B")
    let success3, val3 = dic2.TryGetValue("B")
    let success4, val4 = dic2.TryGetValue("C")
    equal success1 true
    equal success2 false
    equal success3 true
    equal success4 false
    equal val1 1
    equal val2 0
    equal val3 "2"
    // equal val4 null

[<Fact>]
let ``Dictionary.Keys works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    let mutable i = ""
    for value in dic.Keys do
       i <- value + i

    dic.Keys |> Seq.fold (fun acc k -> acc + dic.[k]) 0
    |> equal 3

[<Fact>]
let ``Dictionary.Keys.Count works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Keys.Count
    |> equal 2

[<Fact>]
let ``Dictionary.Values works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    let mutable i = 0
    for value in dic.Values do
       i <- value + i
    i |> equal 3

[<Fact>]
let ``Dictionary.Values.Count works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Values.Count
    |> equal 2

[<Fact>]
let ``Dictionary.Clear works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Clear()
    dic.Count |> equal 0

[<Fact>]
let ``IDictionary.Clear works`` () =
    // see #1120
    let dic: IDictionary<_,_> = upcast Dictionary()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Clear()
    dic.Count |> equal 0

[<Fact>]
let ``Dictionary.Add works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Count |> equal 2

[<Fact>]
let ``Dictionary.ContainsKey works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

[<Fact>]
let ``Dictionary.ContainsValue works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsValue("Hello") |> equal true
    dic.ContainsValue("Everybody!") |> equal false

[<Fact>]
let ``Dictionary.Remove works`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Remove("A") |> equal true
    dic.Remove("C") |> equal false
    dic.Count |> equal 1

[<Fact>]
let ``Dictionary.Remove with complex keys works`` () =
    let dic = Dictionary<_, _>()
    dic.Add((0,"A"), "Hello")
    dic.Add((1,"B"), "World!")
    dic.Remove((0,"A")) |> equal true
    dic.Remove((1,"B")) |> equal true
    dic.Count |> equal 0

[<Fact>]
let ``Dictionary.Remove with records as keys works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let x4 = { a = 15 }
    let dic = Dictionary<_,_>()
    dic.Add(x1, "Hello")
    dic.Add(x3, "World!")
    dic.Remove(x2) |> equal true
    dic.Remove(x4) |> equal false
    dic.Count |> equal 1

[<Fact>]
let ``Interface IDictionary.Count works`` () =
    let dic = dict <| seq { for i in 1. .. 10. -> i, i*i }
    dic.Count |> equal 10

[<Fact>]
let ``Interface IDictionary indexer getter works`` () =
    let dic = dict <| seq { for i in 1. .. 10. -> i, i*i }
    dic.[2] |> equal 4.

[<Fact>]
let ``Interface IDictionary.Keys works`` () =
    let dic = dict <| seq { for i in 1. .. 10. -> i, i*i }
    let mutable i = 0.
    for key in dic.Keys do
       i <- key + i
    i |> equal 55.

[<Fact>]
let ``Interface IDictionary.Values works`` () =
    let dic = dict <| seq { for i in 1. .. 10. -> i, i*i }
    let mutable i = 0.
    for value in dic.Values do
       i <- value + i
    i |> equal 385.

[<Fact>]
let ``Interface IDictionary.ContainsKey works`` () =
    let dic = dict <| [ ("A", 1.); ("B", 2.) ]
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

[<Fact>]
let ``Adding 2 items with the same key throws`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 65)
    throwsAnyError (fun () -> dic.Add("A", 95))

[<Fact>]
let ``Indexer throws when key not found`` () =
    let dic = Dictionary<_,_>()
    dic.Add("A", 65)
    throwsAnyError (fun () -> dic.["B"])

[<Fact>]
let ``conversion from array works`` () =
    let dic = [| "A",1; "B",2|] |> dict
    dic.Values.Count
    |> equal 2

[<Fact>]
let ``conversion from array works with duplicates`` () =
    let dic = [| "A",1; "A",3; "B",2|] |> dict
    dic.Values.Count
    |> equal 2

//    [<Fact>]
//    let ``Dictionary with type as key works`` () = // See #2202
//        let cache = Dictionary<Type, int>()
//        cache.Add(typeof<int>, 1)
//        cache.Add(typeof<string>, 2)
//        cache.Add(typeof<int64>, 3)
//        equal 3 cache.Count
//        equal cache.[typeof<int>] 1
//        equal cache.[typeof<string>] 2
//        equal cache.[typeof<int64>] 3

let tests() =
    ``Dictionary KeyValuePattern works`` ()
    ``Dictionary creation works`` ()
    ``Interface IDictionary creation works`` ()
    ``Dictionary creation from IDictionary works`` ()
    ``Dictionaries with IEqualityComparer work`` ()
    ``Interface IDictionary folding works`` ()
    ``Dictionary iteration works`` ()
    ``Dictionary folding works`` ()
    ``Dictionary.Count works`` ()
    ``Dictionary indexer works`` ()
    ``Dictionary.TryGetValue works`` ()
    ``Dictionary.Keys works`` ()
    ``Dictionary.Keys.Count works`` ()
    ``Dictionary.Values works`` ()
    ``Dictionary.Values.Count works`` ()
    ``Dictionary.Clear works`` ()
    ``IDictionary.Clear works`` ()
    ``Dictionary.Add works`` ()
    ``Dictionary.ContainsKey works`` ()
    ``Dictionary.ContainsValue works`` ()
    ``Dictionary.Remove works`` ()
    ``Dictionary.Remove with complex keys works`` ()
    ``Dictionary.Remove with records as keys works`` ()
    ``Interface IDictionary.Count works`` ()
    ``Interface IDictionary indexer getter works`` ()
    ``Interface IDictionary.Keys works`` ()
    ``Interface IDictionary.Values works`` ()
    ``Interface IDictionary.ContainsKey works`` ()
    ``Adding 2 items with the same key throws`` ()
    ``Indexer throws when key not found`` ()
    ``conversion from array works`` ()
    ``conversion from array works with duplicates`` ()
