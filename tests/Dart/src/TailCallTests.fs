module Fable.Tests.Dart.TailCall

open Util

module Functions =
    let rec factorial1 aux n =
        if n = 0 then aux
        else factorial1 (aux * n) (n - 1)

    let rec factorial2 n =
        match n with
        | 0 | 1 -> 1
        | _ -> n * factorial2(n-1)

    let factorial3 n =
        let rec loop i acc =
            match i with
            | 0 | 1 -> acc
            | _ -> loop (i-1) (acc * i)
        loop n 1

    let rec sum v m s =
        if v >= m
        then s
        else sum (v + 1L) m (s + v)

    let rec iife a =
        2 * (match id a with
            | []  -> 0
            | [x]  -> x
            | h::t -> iife t)

    type Tree<'Key,'T> =
        | Empty
        | Node of 'Key * 'T * Tree<'Key,'T> *  Tree<'Key,'T> * int

    let One(k,v) = Node(k,v,Empty,Empty,1)

    let rec tryFind (k: 'a) (m: Tree<'a,'b>): 'b option =
        match m with
        | Empty -> None
        | Node(k2,v2,l,r,_) ->
            let c = compare k k2
            if c < 0 then tryFind k l
            elif c = 0 then Some v2
            else tryFind k r

    let rec functionArguments x f =
        let f2 = f >> ((+) 3)
        match x with
        | [] -> failwith "empty list"
        | [x] -> f x
        | h::t -> functionArguments t f2

    let rec iterate f n x =
        match n with
        | 0 -> x
        | _ -> iterate f (n - 1) (f x)

    let recWithFinally () =
        let mutable log = ""
        let rec test n =
          try
            log <- log + "abcde".Substring(n, 1)
            if n < 4 then test (n+1)
          finally
            log <- log + "ABCDE".Substring(n, 1)
        test 0
        log

    type Disposable(f) =
        interface System.IDisposable with
              member x.Dispose() = f()
        static member Make(f): System.IDisposable = new Disposable(f)

    let recWithUse () =
        let mutable log = ""
        let disp(n) = Disposable.Make(fun () ->
            log <- log + "ABCDE".Substring(n, 1))
        let rec test n =
          use _disp = disp(n)
          log <- log + "abcde".Substring(n, 1)
          if n < 4 then test (n+1) else 0
        test 0 |> ignore
        log

open Functions

type MyTailCall() =
    member x.Sum(v, m, s) =
        if v >= m
        then s
        else x.Sum(v + 1L, m, s + v)

let rec parseNum tokens acc = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens (x::acc) xs
  | xs -> parseTokens ((List.rev acc)::tokens) xs

and parseTokens tokens = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens [x] xs
  | x::xs -> parseTokens tokens xs
  | [] -> List.rev tokens

type Element =
    | Element of action: (unit->unit) * children: Element list
    member this.Activate() =
        match this with
        | Element(action, children) ->
            action()
            for child in children do child.Activate()

module Elmish =
    let mutable private accValue = 0
    let getAccValue() = accValue

    let el action children = Element(action, children)

    let init () =
        [|1 .. 4|] |> Array.map (fun i -> i, false) |> Map.ofArray

    let update msg (model: Map<int,bool>) =
        accValue <- accValue + msg
        let b = model.[msg]
        Map.add msg (not b) model

    let view model dispatch =
        let rec looper acc x =
            match x with
            |(i1,b1)::(i2,b2)::rest ->
                let newParent =
                    el ignore [
                        el (fun _ ->
                            // Force the x argument to be captured
                            // by the closure
                            match x with
                            |(i1,_)::_ -> i1 |> dispatch
                            | _ -> ()) []
                        el (fun _ -> i2 |> dispatch) []
                    ]
                looper (newParent::acc) rest
            |(i,b)::rest ->
                let newParent =
                    el (fun _ -> i |> dispatch) []
                looper (newParent::acc) rest
            |[] -> acc

        // Use two looper references to prevent the binding
        // beta reduction and force the tail-call optimization
        model |> Map.toList |> looper [] |> ignore
        model |> Map.toList |> looper [] |> el ignore

    let main (init: unit -> 'Model) (view: 'Model -> ('msg -> unit) -> Element) (update: 'msg -> 'Model -> 'Model): unit =
        let mutable model = init()
        let dispatch m msg =
            model <- update msg m
        let el: Element = view model (dispatch model)
        el.Activate()

[<Fact>]
let ``Recursive functions can be tailcall optimized`` () =
    factorial1 1 10 |> equal 3628800

[<Fact>]
let ``Non-tailcall recursive functions work`` () =
    factorial2 10 |> equal 3628800

[<Fact>]
let ``Nested functions can be tailcall optimized`` () =
    factorial3 10 |> equal 3628800

[<Fact>]
let ``Arguments can be consumed after being _u22_passed_u22_ in tailcall optimizations`` () =
    sum 0L 100000L 0L
    |> equal 4999950000L

[<Fact>]
let ``Class methods can be tailcall optimized`` () =
    let x = MyTailCall()
    x.Sum(0L, 100000L, 0L)
    |> equal 4999950000L

[<Fact>]
let ``Mutually recursive functions can be partially optimized`` () =
   let s = "a5b6c"
   s.ToCharArray() |> Seq.toList |> parseTokens []
   |> Seq.concat |> Seq.map string |> String.concat ""
   |> equal "56"

[<Fact>]
let ``IIFEs prevent tailcall optimization`` () =
    // See #674
    iife [5; 4; 3] |> equal 24

[<Fact>]
let ``Tailcall optimization doesn_u27_t cause endless loops`` () =
    // See #675
    One("a", 42)
    |> tryFind "a"
    |> equal (Some 42)
    Tree<string, int>.Empty
    |> tryFind "a"
    |> equal None

[<Fact>]
let ``Recursive functions containing finally work`` () =
    recWithFinally () |> equal "abcdeEDCBA"

[<Fact>]
let ``Recursive functions containing use work`` () =
    recWithUse () |> equal "abcdeEDCBA"

[<Fact>]
let ``Function arguments can be optimized`` () =
    // See #681
    functionArguments [1;2;3] ((+) 2) |> equal 11

[<Fact>]
let ``Function arguments can be optimized II`` () =
    // See #681
    iterate ((*) 2) 5 10 |> equal 320

    // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713

[<Fact>]
let ``State of internally mutated tail called function parameters is preserved properly`` () =
    let rec loop i lst =
        if i <= 0
        then lst
        else loop (i - 1) ((fun () -> i) :: lst)
    loop 3 [] |> List.map (fun f -> f()) |> equal [1;2;3]

[<Fact>]
let ``State of internally mutated tail called function parameters is preserved properly II`` () =
    let rec loop lst i =
        if i <= 0
        then lst
        else loop ((fun () -> i) :: lst) (i - 1)
    loop [] 3 |> List.map (fun f -> f()) |> equal [1;2;3]

// See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713

[<Fact>]
let ``Functions returning unit can be tail called`` () =
  let cmpsts = Array.zeroCreate 16384
  let rec loop x =
    if x > 0 then
      let rec loopi i =
        if i <= 254 then
          if cmpsts.[i >>> 3] &&& (1uy <<< (i &&& 7)) <> 0uy then loopi (i + 1) else
          let p = i + i + 3
          let s0 = 2 * i * (i + 3) + 3
          let lmt = min 131072 <| s0 + (p <<< 3)
          let rec loops s =
            if s < lmt then
              let msk = 1uy <<< (s &&& 7)
              let rec loopj j =
                let cmpsts = cmpsts
                if j < 16384 then
                  cmpsts.[j] <- cmpsts.[j] ||| msk
                  loopj (j + p)
              loopj (s >>> 3); loops (s + p)
          loops s0; loopi (i + 1)
      loopi 0; loop (x - 1)
  loop 100
  let oddprms = seq {
    for i in 0 .. 131071 do
      if cmpsts.[i >>> 3] &&& (1uy <<< (i &&& 7)) = 0uy
      then yield i + i + 3 }
  seq { yield 2; yield! oddprms }
  |> Seq.length |> equal 23000

[<Fact>]
let ``Expressiones captured in a closure don_u27_t change after tail-call optimizations`` () =
    // See #1859
    Elmish.main Elmish.init Elmish.view Elmish.update
    Elmish.getAccValue() |> equal 10

let tests() =
    ``Recursive functions can be tailcall optimized`` ()
    ``Non-tailcall recursive functions work`` ()
    ``Nested functions can be tailcall optimized`` ()
    ``Arguments can be consumed after being _u22_passed_u22_ in tailcall optimizations`` ()
    ``Class methods can be tailcall optimized`` ()
    ``Mutually recursive functions can be partially optimized`` ()
    ``IIFEs prevent tailcall optimization`` ()
    ``Tailcall optimization doesn_u27_t cause endless loops`` ()
    ``Recursive functions containing finally work`` ()
    ``Recursive functions containing use work`` ()
    ``Function arguments can be optimized`` ()
    ``Function arguments can be optimized II`` ()
    ``State of internally mutated tail called function parameters is preserved properly`` ()
    ``State of internally mutated tail called function parameters is preserved properly II`` ()
    ``Functions returning unit can be tail called`` ()
    ``Expressiones captured in a closure don_u27_t change after tail-call optimizations`` ()
