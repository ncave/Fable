// Fable .NET HashSet implementation for non-primitive keys.
namespace Fable.Collections

open System.Collections.Generic
open Native

[<Sealed>]
[<CompiledName("HashSet")>]
type NativeSet<'T when 'T: equality>(items: 'T seq) =

    let hashSet = Fable.Core.JS.Constructors.Set.Create(items)

    // TODO: more constructors
    // new () = NativeSet<'T>(Seq.empty)

    // static member CreateSetComparer(): IEqualityComparer<HashSet<'T>> =
    //     upcast EqualityComparer.Default

    member _.Comparer = EqualityComparer<'T>.Default

    member _.Clear() = hashSet.clear ()

    member _.Count = hashSet.size

    member _.Add(k) =
        if hashSet.has (k) then
            false
        else
            hashSet.add (k) |> ignore
            true

    member _.Contains(k) = hashSet.has (k)

    member _.Remove(k) = hashSet.delete (k)

    member _.EnsureCapacity(capacity: int) = capacity

    member _.TrimExcess() = ()

    member _.CopyTo(array: 'T[]) : unit =
        hashSet.values () |> Seq.iteri (fun i e -> array.[i] <- e)

    member _.CopyTo(array: 'T[], arrayIndex: int) : unit =
        hashSet.values () |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)

    member _.CopyTo(array: 'T[], arrayIndex: int, count: int) : unit =
        hashSet.values ()
        |> Seq.take count
        |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)

    member _.ExceptWith(other: IEnumerable<'T>) : unit =
        for x in other do
            hashSet.delete (x) |> ignore

    member _.UnionWith(other: IEnumerable<'T>) : unit =
        for x in other do
            hashSet.add (x) |> ignore

    //TODO: implement
    member _.IntersectWith(other: IEnumerable<'T>) : unit = failwith "Not Implemented"
    member _.IsProperSubsetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"
    member _.IsProperSupersetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"
    member _.IsSubsetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"
    member _.IsSupersetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"
    member _.Overlaps(other: IEnumerable<'T>) : bool = failwith "Not Implemented"
    member _.SetEquals(other: IEnumerable<'T>) : bool = failwith "Not Implemented"
    member _.SymmetricExceptWith(other: IEnumerable<'T>) : unit = failwith "Not Implemented"

    interface Fable.Core.Symbol_wellknown with
        member _.``Symbol.toStringTag`` = "HashSet"

    // Native JS Set (used for primitive keys) doesn't work with `JSON.stringify` but
    // let's add `toJSON` for consistency with the types within fable-library.
    interface Fable.Core.IJsonSerializable with
        member this.toJSON() = Helpers.arrayFrom (this) |> box

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() : System.Collections.IEnumerator =
            ((this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<'T> with
        member _.GetEnumerator() : IEnumerator<'T> = hashSet.values().GetEnumerator()

    interface ICollection<'T> with
        member this.Add(item: 'T) : unit = this.Add item |> ignore
        member this.Clear() : unit = this.Clear()
        member this.Contains(item: 'T) : bool = this.Contains item
        member this.CopyTo(array: 'T[], arrayIndex: int) : unit = this.CopyTo(array, arrayIndex)
        member this.Count: int = this.Count
        member this.IsReadOnly: bool = false
        member this.Remove(item: 'T) : bool = this.Remove item

    interface ISet<'T> with
        member this.Add(item: 'T) : bool = this.Add item
        member this.ExceptWith(other: IEnumerable<'T>) : unit = this.ExceptWith other
        member this.IntersectWith(other: IEnumerable<'T>) : unit = this.IntersectWith other
        member this.IsProperSubsetOf(other: IEnumerable<'T>) : bool = this.IsProperSubsetOf other
        member this.IsProperSupersetOf(other: IEnumerable<'T>) : bool = this.IsProperSupersetOf other
        member this.IsSubsetOf(other: IEnumerable<'T>) : bool = this.IsSubsetOf other
        member this.IsSupersetOf(other: IEnumerable<'T>) : bool = this.IsSupersetOf other
        member this.Overlaps(other: IEnumerable<'T>) : bool = this.Overlaps other
        member this.SetEquals(other: IEnumerable<'T>) : bool = this.SetEquals other
        member this.SymmetricExceptWith(other: IEnumerable<'T>) : unit = this.SymmetricExceptWith other
        member this.UnionWith(other: IEnumerable<'T>) : unit = this.UnionWith other

    interface Fable.Core.JS.Set<'T> with
        member _.size = hashSet.size

        member this.add(k) =
            hashSet.add (k) |> ignore
            this :> Fable.Core.JS.Set<'T>

        member _.clear() = hashSet.clear ()
        member _.delete(k) = hashSet.delete (k)
        member _.has(k) = hashSet.has (k)
        member _.keys() = hashSet.keys ()
        member _.values() = hashSet.values ()
        member _.entries() = hashSet.entries ()
        member _.forEach(f, ?thisArg) = hashSet.forEach (f, thisArg)
