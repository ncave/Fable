namespace System.Threading

open System

type CancellationTokenRegistration() =
    interface IDisposable with
        member _.Dispose() = ()

type CancellationTokenState() =
    let mutable isCanceled = false
    let listeners = ResizeArray<unit -> unit>()

    member _.IsCancellationRequested = isCanceled

    member _.Cancel() =
        if not isCanceled then
            isCanceled <- true

            for listener in listeners.ToArray() do
                listener ()

    member _.Register(callback: unit -> unit) : CancellationTokenRegistration =
        if isCanceled then
            callback ()
        else
            listeners.Add(callback)

        CancellationTokenRegistration()

type CancellationToken(state: CancellationTokenState) =
    new() = CancellationToken(CancellationTokenState())

    member _.IsCancellationRequested = state.IsCancellationRequested

    member _.Register(callback: unit -> unit) : CancellationTokenRegistration = state.Register(callback)

    member _.ThrowIfCancellationRequested() = ()

type CancellationTokenSource() =
    let state = CancellationTokenState()
    let token = CancellationToken(state)

    member _.Token = token

    member _.IsCancellationRequested = state.IsCancellationRequested

    member _.Cancel() = state.Cancel()

    member _.CancelAfter(millisecondsDelay: int) =
        let start = DateTimeOffset.UtcNow

        while (DateTimeOffset.UtcNow - start).TotalMilliseconds < float millisecondsDelay do
            ()

        state.Cancel()

    member _.Register(callback: unit -> unit) : CancellationTokenRegistration = state.Register(callback)

    member _.ThrowIfCancellationRequested() = ()
