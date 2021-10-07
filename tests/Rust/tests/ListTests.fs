module Fable.Tests.ListTests

open Util.Testing

[<Fact>]
let ``List empty works`` () =
    // Using mutable to suppress inlining which is not supported yet,
    // as it prevents the generic type being instantiated, and breaks.
    // WRONG: let actual: bool = List::isEmpty(&List::empty())
    // RIGHT: let actual: bool = List::isEmpty(&List::empty::<i32>()) // somehow
    let mutable xs: int list = []
    List.isEmpty xs |> equal true
    let mutable xs = List.empty<int>
    List.isEmpty xs |> equal true

[<Fact>]
let ``List cons works`` () =
    let xs = 1::2::3::[]
    List.head xs |> equal 1
    let xs = List.tail xs
    List.head xs |> equal 2
    let xs = List.tail xs
    List.head xs |> equal 3
    let xs = List.tail xs
    List.isEmpty xs |> equal true