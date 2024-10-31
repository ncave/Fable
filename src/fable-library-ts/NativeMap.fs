// Fable .NET Dictionary implementation for non-primitive keys.
namespace Fable.Collections

open System.Collections.Generic
open Native

[<Sealed>]
[<CompiledName("Dictionary")>]
type NativeMap<'Key, 'Value when 'Key: equality>(pairs: KeyValuePair<'Key, 'Value> seq) as this =

    let items = pairs |> Seq.map (fun p -> p.Key, p.Value)
    let hashMap = Fable.Core.JS.Constructors.Map.Create(items)

    // TODO: more constructors
    // new () = NativeMap<'Key, 'Value>(Seq.empty)

    member _.TryFind(k: 'Key) = hashMap.get (k)

    member _.Comparer = EqualityComparer<'Key>.Default

    member _.Clear() = hashMap.clear ()

    member _.Count = hashMap.size

    member _.EnsureCapacity(capacity: int) = capacity

    member _.TrimExcess() = ()

    member _.TrimExcess(capacity: int) = ()

    member this.Item
        with get (k: 'Key) =
            match hashMap.get (k) with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The item was not found in collection"))
        and set (k: 'Key) (v: 'Value) = hashMap.set (k, v) |> ignore

    member _.Add(k: 'Key, v: 'Value) =
        if hashMap.has (k) then
            let msg = $"An item with the same key has already been added. Key: {k}"
            raise (System.ArgumentException(msg))
        else
            hashMap.set (k, v) |> ignore

    member _.ContainsKey(k: 'Key) = hashMap.has (k)

    member _.ContainsValue(v: 'Value) =
        hashMap.values () |> Seq.exists (fun v2 -> Unchecked.equals v v2)

    member _.Remove(k: 'Key) : bool = hashMap.delete (k)

    member this.Remove(k: 'Key, value: byref<'Value>) : bool =
        match hashMap.get (k) with
        | Some v ->
            value <- v
            hashMap.delete (k)
        | _ -> false

    member _.Keys: ICollection<'Key> = hashMap.keys () |> Seq.toArray :> ICollection<'Key>

    member _.Values: ICollection<'Value> =
        hashMap.values () |> Seq.toArray :> ICollection<'Value>

    member _.TryAdd(k, v) =
        if hashMap.has (k) then
            false
        else
            hashMap.set (k, v) |> ignore
            true

    member this.TryGetValue(k: 'Key, value: byref<'Value>) : bool =
        match hashMap.get (k) with
        | Some v ->
            value <- v
            true
        | _ -> false

    interface Fable.Core.Symbol_wellknown with
        member _.``Symbol.toStringTag`` = "Dictionary"

    // Native JS Map (used for primitive keys) doesn't work with `JSON.stringify` but
    // let's add `toJSON` for consistency with the types within fable-library.
    interface Fable.Core.IJsonSerializable with
        member this.toJSON() = Helpers.arrayFrom (this) |> box

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() : System.Collections.IEnumerator =
            ((this :> IEnumerable<KeyValuePair<'Key, 'Value>>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member _.GetEnumerator() : IEnumerator<KeyValuePair<'Key, 'Value>> =
            let entries = hashMap.entries () |> Seq.map (fun (k, v) -> KeyValuePair(k, v))
            entries.GetEnumerator()

    interface ICollection<KeyValuePair<'Key, 'Value>> with
        member this.Add(item: KeyValuePair<'Key, 'Value>) : unit = this.Add(item.Key, item.Value)

        member this.Clear() : unit = this.Clear()

        member _.Contains(item: KeyValuePair<'Key, 'Value>) : bool =
            match hashMap.get (item.Key) with
            | Some v -> Unchecked.equals v item.Value
            | None -> false

        member _.CopyTo(array: KeyValuePair<'Key, 'Value>[], arrayIndex: int) : unit =
            hashMap.entries ()
            |> Seq.map (fun (k, v) -> KeyValuePair(k, v))
            |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)

        member this.Count: int = this.Count
        member this.IsReadOnly: bool = false

        member _.Remove(item: KeyValuePair<'Key, 'Value>) : bool =
            match hashMap.get (item.Key) with
            | Some v ->
                if Unchecked.equals v item.Value then
                    hashMap.delete (item.Key)
                else
                    false
            | None -> false

    interface IDictionary<'Key, 'Value> with
        member this.Add(key: 'Key, value: 'Value) : unit = this.Add(key, value)
        member this.ContainsKey(key: 'Key) : bool = this.ContainsKey(key)

        member this.Item
            with get (key: 'Key): 'Value = this.[key]
            and set (key: 'Key) (v: 'Value): unit = this.[key] <- v

        member this.Keys: ICollection<'Key> = this.Keys

        member this.Remove(key: 'Key) = this.Remove(key)

        member this.TryGetValue(key: 'Key, value: byref<'Value>) = this.TryGetValue(key, &value)

        member this.Values: ICollection<'Value> = this.Values

    interface Fable.Core.JS.Map<'Key, 'Value> with
        member _.size = hashMap.size
        member _.clear() = hashMap.clear ()
        member _.delete(k) = hashMap.delete (k)
        member _.entries() = hashMap.entries ()
        member _.get(k) = hashMap.get (k)
        member _.has(k) = hashMap.has (k)
        member _.keys() = hashMap.keys ()

        member this.set(k, v) =
            hashMap.set (k, v) |> ignore
            this :> Fable.Core.JS.Map<'Key, 'Value>

        member _.values() = hashMap.values ()
        member _.forEach(f, ?thisArg) = hashMap.forEach (f, thisArg)
