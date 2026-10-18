module Fable.Tests.Dart.Async

open System
open Fable.Core.Dart
open Util

type DisposableAction(f) =
    interface IDisposable with
        member _.Dispose() = f ()

type MyException(value) =
    inherit Exception()
    member _.Value: int = value

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess, _, _) -> onSuccess "success")

let errorWork: Async<string> = Async.FromContinuations(fun (_, onError, _) -> onError (exn "error"))

let cancelWork: Async<string> =
    Async.FromContinuations(fun (_, _, onCancel) -> System.OperationCanceledException("cancelled") |> onCancel)

let asyncMap f a =
    async {
        let! a = a
        return f a
    }

let sleepAndAssign token (res: bool ref) =
    Async.StartImmediate(
        async {
            do! Async.Sleep 200
            res.Value <- true
        },
        token
    )

[<Fact>]
let ``Simple async translates without exception`` () =
    async { return () } |> Async.StartImmediate

[<Fact>]
let ``Async while binding works correctly`` () =
    let mutable result = 0

    async {
        while result < 10 do
            result <- result + 1
    }
    |> Async.StartImmediate

    equal 10 result

[<Fact>]
let ``Async for binding works correctly`` () =
    let inputs = [| 1; 2; 3 |]
    let mutable result = 0

    async {
        for inp in inputs do
            result <- result + inp
    }
    |> Async.StartImmediate

    equal 6 result

[<Fact>]
let ``Async exceptions are handled correctly`` () =
    let mutable result = 0

    let f shouldThrow =
        async {
            try
                if shouldThrow then
                    failwith "boom!"
                else
                    result <- 12
            with _ ->
                result <- 10
        }
        |> Async.StartImmediate

        result

    f true + f false |> equal 22

[<Fact>]
let ``Non captured exception in async is propagated when using Async.StartImmediate`` () =
    throwsAnyError (fun () -> async { failwith "boom!" } |> Async.StartImmediate)

// Behaviour of Async.Start is the same as Async.StartImmediate in Dart

#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Non captured exception in async is propagated when using Async.Start`` () =
    throwsAnyError (fun () -> async { failwith "boom!" } |> Async.Start)

[<Fact>]
let ``Simple async is executed correctly`` () =
    let mutable result = false
    let x = async { return 99 }

    async {
        let! x = x
        let y = 99
        result <- x = y
    }
    |> Async.StartImmediate

    equal true result

[<Fact>]
let ``async use statements should dispose of resources when they go out of scope`` () =
    let mutable isDisposed = false
    let mutable step1ok = false
    let mutable step2ok = false

    let resource = async { return new DisposableAction(fun () -> isDisposed <- true) }

    async {
        use! _r = resource
        step1ok <- not isDisposed
    }
    |> Async.StartImmediate

    step2ok <- isDisposed

    (step1ok && step2ok) |> equal true

[<Fact>]
let ``Try ... with ... expressions inside async expressions work the same`` () =
    let mutable result = ""

    let throw () : unit = raise (exn "Boo!")

    let append x = result <- result + x

    let innerAsync () =
        async {
            append "b"

            try
                append "c"
                throw ()
                append "1"
            with _ ->
                append "d"

            append "e"
        }

    async {
        append "a"

        try
            do! innerAsync ()
        with _ ->
            append "2"

        append "f"
    }
    |> Async.StartImmediate

    equal "abcdef" result

[<Fact>]
let ``Async cancellation works`` () =
    runAsyncTest <| fun () ->
        async {
            let res1, res2, res3 = ref false, ref false, ref false
            let tcs1 = new System.Threading.CancellationTokenSource(50)
            let tcs2 = new System.Threading.CancellationTokenSource()
            let tcs3 = new System.Threading.CancellationTokenSource()
            sleepAndAssign tcs1.Token res1
            sleepAndAssign tcs2.Token res2
            sleepAndAssign tcs3.Token res3
            tcs2.Cancel()
            tcs3.CancelAfter(1000)
            do! Async.Sleep 500
            equal false res1.Value
            equal false res2.Value
            equal true res3.Value
        }

[<Fact>]
let ``CancellationTokenSource.Register works`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable x = 0
            let res1 = ref false
            let tcs1 = new System.Threading.CancellationTokenSource(50)
            let _foo = tcs1.Token.Register(fun () -> x <- x + 1)
            sleepAndAssign tcs1.Token res1
            do! Async.Sleep 500
            equal false res1.Value
            equal 1 x
        }

[<Fact>]
let ``CancellationToken can be disposed`` () =
    runAsyncTest <| fun () -> // See #2879
        async {
            let res1 = ref false

            do
                use tcs1 = new System.Threading.CancellationTokenSource()
                sleepAndAssign tcs1.Token res1

            do! Async.Sleep 300
            // Disposing the CancellationToken doesn't cancel the operation
            equal true res1.Value
        }

[<Fact>]
let ``Async.StartWithContinuations works`` () =
    let mutable res1 = ""
    let mutable res2 = ""
    let mutable res3 = ""
    Async.StartWithContinuations(successWork, (fun x -> res1 <- x), ignore, ignore)
    Async.StartWithContinuations(errorWork, ignore, (fun x -> res2 <- x.Message), ignore)
    Async.StartWithContinuations(cancelWork, ignore, ignore, (fun x -> res3 <- x.Message))
    equal "success" res1
    equal "error" res2
    equal "cancelled" res3

[<Fact>]
let ``Async.Catch works`` () =
    let mutable res1 = ""
    let mutable res2 = ""

    let assign =
        function
        | Choice1Of2 msg -> msg
        | Choice2Of2(ex: Exception) -> "ERROR: " + ex.Message

    async {
        let! x1 = successWork |> Async.Catch
        res1 <- assign x1
        let! x2 = errorWork |> Async.Catch
        res2 <- assign x2
    }
    |> Async.StartImmediate

    equal "success" res1
    equal "ERROR: error" res2

[<Fact>]
let ``Async.Ignore works`` () =
    let mutable res = false

    async {
        do! successWork |> Async.Ignore
        res <- true
    }
    |> Async.StartImmediate

    equal true res

[<Fact>]
let ``Async.Parallel works`` () =
    runAsyncTest <| fun () ->
        async {
            let makeWork i =
                async {
                    do! Async.Sleep 200
                    return i
                }

            let mutable res: int[] = [||]
            let works = [ makeWork 1; makeWork 2; makeWork 3 ]

            async {
                let! x = Async.Parallel works
                res <- x
            }
            |> Async.StartImmediate

            do! Async.Sleep 500
            res |> Array.sum |> equal 6
        }

[<Fact>]
let ``Async.Parallel is lazy`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable x = 0

            let add i = x <- x + i

            let a = Async.Parallel [ async { add 1 }; async { add 2 } ]

            do! Async.Sleep 100

            equal 0 x

            let! _ = a

            equal 3 x
        }

[<Fact>]
let ``Async.Sequential works`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable aggregate = 0

            let makeWork i =
                async {
                    // check that the individual work items run sequentially and not interleaved
                    aggregate <- aggregate + i
                    let copyOfI = aggregate
                    do! Async.Sleep 100
                    equal copyOfI aggregate
                    do! Async.Sleep 100
                    equal copyOfI aggregate
                    return i
                }

            let works = [ for i in 1..5 -> makeWork i ]
            let! result = Async.Sequential works
            result |> equal [| 1..5 |]
            result |> Array.sum |> equal aggregate
        }

[<Fact>]
let ``Async.Sequential is lazy`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable x = 0

            let a = Async.Sequential [ async { x <- x + 1 }; async { x <- x + 2 } ]

            do! Async.Sleep 100

            equal 0 x

            let! _ = a

            equal 3 x
        }


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Interaction between Async and Future works`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable res = false

            async { res <- true }
            |> Async.StartAsFuture
            |> Async.AwaitFuture
            |> Async.StartImmediate

            equal true res
        }


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Futures can be cancelled`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable res = 0
            let tcs = new System.Threading.CancellationTokenSource(50)

            let work =
                let work =
                    async {
                        do! Async.Sleep 75
                        res <- -1
                    }

                Async.StartAsFuture(work, tcs.Token) |> Async.AwaitFuture

            Async.StartWithContinuations(work, ignore, ignore, (fun _ -> res <- 1))
            do! Async.Sleep 100
            equal 1 res
        }


#if FABLE_COMPILER
[<Fact>]
#else
[<Fact(Skip = "Dart target-specific behavior.")>]
#endif
let ``Async.AwaitFuture propagates Future errors`` () =
    runAsyncTest <| fun () ->
        async {
            let future =
                async {
                    failwith "boom"
                    return 0
                }
                |> Async.StartAsFuture

            let mutable message = ""

            try
                let! _ = Async.AwaitFuture future
                ()
            with error ->
                message <- error.Message

            equal "boom" message
        }

[<Fact>]
let ``Async try .. with returns correctly from _u27_with_u27_ branch`` () =
    let work =
        async {
            try
                failwith "testing"
                return -1
            with e ->
                return 42
        }

    let mutable result = 0
    Async.StartWithContinuations(work, (fun r -> result <- r), ignore, ignore)
    equal 42 result

[<Fact>]
let ``Deep recursion with async doesn_u27_t cause stack overflow`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable result = false

            let rec trampolineTest i =
                async {
                    if i > 100000 then
                        result <- true
                    else
                        return! trampolineTest (i + 1)
                }

            do! trampolineTest 0
            equal true result
        }

[<Fact>]
let ``Nested failure propagates in async expressions`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable data = ""

            let f1 x =
                async {
                    try
                        failwith "1"
                        return x
                    with e ->
                        return! failwith ("2 " + e.Message)
                }

            let f2 x =
                async {
                    try
                        return! f1 x
                    with e ->
                        return! failwith ("3 " + e.Message)
                }

            let f () =
                async {
                    try
                        let! _y = f2 4
                        return ()
                    with e ->
                        data <- e.Message
                }
                |> Async.StartImmediate

            f ()
            do! Async.Sleep 100
            equal "3 2 1" data
        }

[<Fact>]
let ``Try .. finally expressions inside async expressions work`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable data = ""

            async {
                try
                    data <- data + "1 "
                finally
                    data <- data + "2 "
            }
            |> Async.StartImmediate

            async {
                try
                    try
                        failwith "boom!"
                    finally
                        data <- data + "3"
                with _ ->
                    ()
            }
            |> Async.StartImmediate

            do! Async.Sleep 100
            equal "1 2 3" data
        }

[<Fact>]
let ``Final statement inside async expressions can throw`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable data = ""

            let f () =
                async {
                    try
                        data <- data + "1 "
                    finally
                        failwith "boom!"
                }

            async {
                try
                    do! f ()
                    return ()
                with e ->
                    data <- data + e.Message
            }
            |> Async.StartImmediate

            do! Async.Sleep 100
            equal "1 boom!" data
        }

[<Fact>]
let ``Async.Bind propagates exceptions`` () =
    runAsyncTest <| fun () -> // See #724
        async {
            let task1 name =
                async {
                    if name = "fail" then
                        failwith "Invalid access credentials"

                    return "Ok"
                }

            let task2 name =
                async {
                    do! Async.Sleep 100 //difference between task1 and task2

                    if name = "fail" then
                        failwith "Invalid access credentials"

                    return "Ok"
                }

            let doWork _name task =
                let catch comp =
                    async {
                        let! res = Async.Catch comp

                        return
                            match res with
                            | Choice1Of2 str -> str
                            | Choice2Of2 ex -> ex.Message
                    }

                async {
                    let! a = task "work" |> catch
                    let! b = task "fail" |> catch
                    return a, b
                }

            let! res1 = doWork "task1" task1
            let! res2 = doWork "task2" task2
            equal ("Ok", "Invalid access credentials") res1
            equal ("Ok", "Invalid access credentials") res2
        }

[<Fact>]
let ``Async.StartChild works`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable x = ""

            let taskA =
                async {
                    do! Async.Sleep 500
                    x <- x + "D"
                    return "E"
                }

            let taskB =
                async {
                    do! Async.Sleep 100
                    x <- x + "C"
                    return "F"
                }

            let! result1Async = taskA |> Async.StartChild // start first request but do not wait
            let! result2Async = taskB |> Async.StartChild // start second request in parallel
            x <- x + "AB"
            let! result1 = result1Async
            let! result2 = result2Async
            x <- x + result1 + result2
            equal "ABCDEF" x
        }

[<Fact>]
let ``Async.StartChild applies timeout`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable x = ""

            let task =
                async {
                    x <- x + "A"
                    do! Async.Sleep 1_000
                    x <- x + "X" // Never hit
                }

            try
                let! childTask = Async.StartChild(task, 200)

                do! childTask
            with :? TimeoutException ->
                x <- x + "B"

            x <- x + "C"

            equal "ABC" x
        }

[<Fact>]
let ``Async.StartChild with timeout completes when computation finishes before timeout`` () =
    runAsyncTest <| fun () -> // See #4481
        async {
            let fast = async { do! Async.Sleep 10 }

            try
                let! child = Async.StartChild(fast, 1_000)
                do! child
                equal true true // should reach here
            with :? TimeoutException ->
                failwith "should not time out"
        }

[<Fact>]
let ``Unit arguments are erased`` () =
    runAsyncTest <| fun () -> // See #1832
        let mutable token = 0

        async {
            let! res = async.Return 5 |> asyncMap (fun x -> token <- x)
            equal 5 token
            res
        }

[<Fact>]
let ``Can use custom exceptions in async workflows _u23_2396`` () =
    runAsyncTest <| fun () ->
        let workflow () : Async<unit> = async { return MyException(7) |> raise }

        let parentWorkflow () =
            async {
                try
                    do! workflow ()
                    return 100
                with :? MyException as ex ->
                    return ex.Value
            }

        async {
            let! res = parentWorkflow ()
            equal 7 res
        }

[<Fact>]
let ``Async try .. with does not run _u27_with_u27_ branch when body succeeds`` () =
    let work =
        async {
            try
                return 1
            with _ ->
                return 99
        }

    let mutable result = 0
    Async.StartWithContinuations(work, (fun r -> result <- r), ignore, ignore)
    equal 1 result

[<Fact>]
let ``Exception thrown by the continuation is not redirected to the _u27_with_u27_ branch`` () =
    let work =
        async {
            try
                return 1
            with _ ->
                return 99
        }

    let mutable calls = 0

    throwsAnyError (fun () ->
        Async.StartWithContinuations(
            work,
            (fun _ ->
                calls <- calls + 1
                failwith "boom from continuation"),
            ignore,
            ignore
        )
    )

    equal 1 calls

[<Fact>]
let ``Async.StartChild cancels the child when the parent_u27_s token is cancelled`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable finallyRan = false
            let mutable completed = false
            let cts = new System.Threading.CancellationTokenSource()

            let child =
                async {
                    try
                        do! Async.Sleep 500
                        completed <- true
                    finally
                        finallyRan <- true
                }

            let parent =
                async {
                    let! childResult = Async.StartChild child
                    do! childResult
                }

            Async.StartImmediate(parent, cts.Token)
            do! Async.Sleep 200
            cts.Cancel()
            do! Async.Sleep 600
            equal true finallyRan
            equal false completed
        }

[<Fact>]
let ``Async.Parallel children are cancelled when the parent_u27_s token is cancelled`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable completed = 0
            let cts = new System.Threading.CancellationTokenSource()

            let mkChild () =
                async {
                    do! Async.Sleep 500
                    completed <- completed + 1
                }

            let work =
                async {
                    let! _ = Async.Parallel [ mkChild (); mkChild (); mkChild () ]
                    return ()
                }

            Async.StartImmediate(work, cts.Token)
            do! Async.Sleep 200
            cts.Cancel()
            do! Async.Sleep 600
            equal 0 completed
        }

[<Fact>]
let ``Async.Sequential children are cancelled when the parent_u27_s token is cancelled`` () =
    runAsyncTest <| fun () ->
        async {
            let mutable completed = 0
            let cts = new System.Threading.CancellationTokenSource()

            let mkChild () =
                async {
                    do! Async.Sleep 500
                    completed <- completed + 1
                }

            let work =
                async {
                    let! _ = Async.Sequential [ mkChild (); mkChild (); mkChild () ]
                    return ()
                }

            Async.StartImmediate(work, cts.Token)
            do! Async.Sleep 200
            cts.Cancel()
            do! Async.Sleep 600
            equal 0 completed
        }

[<Fact>]
let ``CancellationTokenSource.Cancel invokes registrations only once`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let mutable calls = 0

    cts.Token.Register(fun () -> calls <- calls + 1) |> ignore

    cts.Cancel()
    cts.Cancel()

    equal 1 calls

[<Fact>]
let ``CancellationToken registration can be disposed`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let mutable called = false

    let registration = cts.Token.Register(fun () -> called <- true)

    registration.Dispose()

    cts.Cancel()

    equal false called

[<Fact>]
let ``CancellationToken registration after cancellation runs immediately`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let mutable called = false

    cts.Cancel()

    cts.Token.Register(fun () -> called <- true) |> ignore

    equal true called

[<Fact>]
let ``ThrowIfCancellationRequested throws after cancellation`` () =
    let cts = new System.Threading.CancellationTokenSource()

    // Does not throw before cancellation.
    cts.Token.ThrowIfCancellationRequested()

    cts.Cancel()

    throwsAnyError (fun () -> cts.Token.ThrowIfCancellationRequested())

[<Fact>]
let ``Async.CancellationToken returns current cancellation token`` () =
    let cts = new System.Threading.CancellationTokenSource()

    let work =
        async {
            let! token = Async.CancellationToken
            token = cts.Token |> equal true
        }

    Async.StartWithContinuations(work, ignore, raise, raise, cts.Token)

[<Fact>]
let ``CancellationTokenSource.CancelAfter can be reset`` () =
    runAsyncTest <| fun () ->
        async {
            let cts = new System.Threading.CancellationTokenSource()

            cts.CancelAfter 300
            do! Async.Sleep 30

            cts.CancelAfter 500
            do! Async.Sleep 350

            equal false cts.IsCancellationRequested

            do! Async.Sleep 200

            equal true cts.IsCancellationRequested
        }

let tests () =
    ``Simple async translates without exception`` ()
    ``Async while binding works correctly`` ()
    ``Async for binding works correctly`` ()
    ``Async exceptions are handled correctly`` ()
    ``Non captured exception in async is propagated when using Async.StartImmediate`` ()
    ``Non captured exception in async is propagated when using Async.Start`` ()
    ``Simple async is executed correctly`` ()
    ``async use statements should dispose of resources when they go out of scope`` ()
    ``Try ... with ... expressions inside async expressions work the same`` ()
    ``Async cancellation works`` ()
    ``CancellationTokenSource.Register works`` ()
    ``CancellationToken can be disposed`` ()
    ``Async.StartWithContinuations works`` ()
    ``Async.Catch works`` ()
    ``Async.Ignore works`` ()
    ``Async.Parallel works`` ()
    ``Async.Parallel is lazy`` ()
    ``Async.Sequential works`` ()
    ``Async.Sequential is lazy`` ()
    ``Interaction between Async and Future works`` ()
    ``Futures can be cancelled`` ()
    ``Async.AwaitFuture propagates Future errors`` ()
    ``Async try .. with returns correctly from _u27_with_u27_ branch`` ()
    ``Deep recursion with async doesn_u27_t cause stack overflow`` ()
    ``Nested failure propagates in async expressions`` ()
    ``Try .. finally expressions inside async expressions work`` ()
    ``Final statement inside async expressions can throw`` ()
    ``Async.Bind propagates exceptions`` ()
    ``Async.StartChild works`` ()
    ``Async.StartChild applies timeout`` ()
    ``Async.StartChild with timeout completes when computation finishes before timeout`` ()
    ``Unit arguments are erased`` ()
    ``Can use custom exceptions in async workflows _u23_2396`` ()
    ``Async try .. with does not run _u27_with_u27_ branch when body succeeds`` ()
    ``Exception thrown by the continuation is not redirected to the _u27_with_u27_ branch`` ()
    ``Async.StartChild cancels the child when the parent_u27_s token is cancelled`` ()
    ``Async.Parallel children are cancelled when the parent_u27_s token is cancelled`` ()
    ``Async.Sequential children are cancelled when the parent_u27_s token is cancelled`` ()
    ``CancellationTokenSource.Cancel invokes registrations only once`` ()
    ``CancellationToken registration can be disposed`` ()
    ``CancellationToken registration after cancellation runs immediately`` ()
    ``ThrowIfCancellationRequested throws after cancellation`` ()
    ``Async.CancellationToken returns current cancellation token`` ()
    ``CancellationTokenSource.CancelAfter can be reset`` ()
    ()
