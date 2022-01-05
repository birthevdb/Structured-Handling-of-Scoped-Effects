## Getting Started Guide

Make sure you have [Docker](https://docs.docker.com/get-docker/) installed for your device.

If you want to build the image yourself, clone the [repository](https://github.com/birthevdb/Structured-Handling-of-Scoped-Effects)
and build using the following commands.
(If you use the prebuilt image, you can skip this step.)
```
git clone https://github.com/birthevdb/Structured-Handling-of-Scoped-Effects.git
cd Structured-Handling-of-Scoped-Effects/
docker build -t scoped .
```

Now, construct the container using
```
docker run -it scoped /bin/bash
```

Check if you can compile the Haskell file
```
ghc Examples.hs
```
and the OCaml file
```
ocaml examples.ml
```

## Step-by-step Instructions

We propose to evaluate the artifact as follows.

### Haskell implementation

The Haskell implementation contains the general datatype `Prog` with its instances, together with the `fold` function for indexed algebras
and that for functorial algebras (paper Figure 1).

Start the Haskell code using the following command:
```
ghci Examples.hs
```

###### Nondeterminism with `Once` (paper Example 2, 4)

We test the following nondeterminism example program
```
prog1 = once(or(1,5)) >>= λx. or(x,x+1)
```
Both implementations with (1) indexed algebras (`Hybrid`) and (2) functorial algebras (`Endo`) should yield `[1,2]` as a result.
```
Examples> exampleHybrid1 -- indexed algebra
[1,2]
Examples> exampleEndo1 -- functorial algebra
[1,2]
```

###### State with Local Variables (paper Example 6)

We test the following local variable example program
```
prog2 = put "x" 1 ;
        put "y" 1 ;
        local "x" 100 (incr "x" 100 ;
                       v <- get "x" ;
                       incr "y" v) ;
        incr "x" 2 ;
        incr "y" 2 ;
        vx <- get "x" ;
        vy <- get "y" ;
        return (vx, vy) ;
```

Both implementations with (1) indexed algebras (`Hybrid`) and (2) functorial algebras (`Endo`) should yield `(3,203)` as a result.
```
Examples> exampleHybrid2 -- indexed algebra
(3,203)
Examples> exampleEndo2 -- functorial algebra
(3,203)
```

###### Parallel Composition of Processes (Concurrency)

We test the following concurrency example programs
```
prog3 = spawn (say "hello " >> say "world ")
        (say "goodbye " >> say "cruel " >> say "world ")
prog4 = spawn (atomic (spawn (mapM say ["a", "b", "c"])
                      (mapM say ["A", "B", "C"])))
       (atomic (spawn (mapM say ["1", "2", "3"])
                      (mapM say ["-", "-", "-"])))
```

Both implementations with (1) indexed algebras (`Hybrid`) and (2) functorial algebras (`Endo`) should yield the same results.
```
Examples> exampleHybrid3 -- indexed algebra
hello goodbye world cruel world
Examples> exampleEndo3 -- functorial algebra
hello goodbye world cruel world

Examples> exampleHybrid4 -- indexed algebra
aAbBcC1-2-3-
Examples> exampleEndo4 -- functorial algebra
aAbBcC1-2-3-
```

###### Depth-bound Search (Example 5)

We test the following search strategy example programs:
- `queens n`, which generates `n` queens.
- `prog5` with a breadth-first search strategry (`bfs`).
```
prog5 = bfs (or (or (return 1)
                    (or (return 3)
                        (return 4)))
                (or (return 2)
                    (or (return 5)
                        (return 6))))
```
- `prog6` with a depth-bound search strategry (`dbs n`).
```
prog6 = dbs 2 (or (or (return 1)
                      (or (return 3)
                          (return 4)))
                  (or (return 2)
                      (or (return 5)
                          (return 6))))
```
- `prog7`, which extends `prog5`.
```
prog7 = do i <- prog5
           or (return i)
              (or (return (-i))
                  (return i))
```
- `prog8` with a breadth-first search strategry (`bfs`)
```
prog8 = bfs (or (return 1)
                (or (dfs (or (or (or (or (return 3)
                                         fail)
                                  fail)
                             fail)
                         fail))
                    (return 2)))
```
We wrote some test for these examples, which should all be true.
```
Examples> exampleQueens -- generate 92 solutions to 8-queens problem
[[4,2,7,3,6,8,5,1],[5,2,4,7,3,8,6,1],[3,5,2,8,6,4,7,1],[3,6,4,2,8,5,7,1],[5,7,1,3,8,6,4,2],[4,6,8,3,1,7,5,2],[3,6,8,1,4,7,5,2],[5,3,8,4,7,1,6,2],[5,7,4,1,3,8,6,2],[4,1,5,8,6,3,7,2],[3,6,4,1,8,5,7,2],[4,7,5,3,1,6,8,2],[6,4,2,8,5,7,1,3],[6,4,7,1,8,2,5,3],[1,7,4,6,8,2,5,3],[6,8,2,4,1,7,5,3],[6,2,7,1,4,8,5,3],[4,7,1,8,5,2,6,3],[5,8,4,1,7,2,6,3],[4,8,1,5,7,2,6,3],[2,7,5,8,1,4,6,3],[1,7,5,8,2,4,6,3],[2,5,7,4,1,8,6,3],[4,2,7,5,1,8,6,3],[5,7,1,4,2,8,6,3],[6,4,1,5,8,2,7,3],[5,1,4,6,8,2,7,3],[5,2,6,1,7,4,8,3],[6,3,7,2,8,5,1,4],[2,7,3,6,8,5,1,4],[7,3,1,6,8,5,2,4],[5,1,8,6,3,7,2,4],[1,5,8,6,3,7,2,4],[3,6,8,1,5,7,2,4],[6,3,1,7,5,8,2,4],[7,5,3,1,6,8,2,4],[7,3,8,2,5,1,6,4],[5,3,1,7,2,8,6,4],[2,5,7,1,3,8,6,4],[3,6,2,5,8,1,7,4],[6,1,5,2,8,3,7,4],[8,3,1,6,2,5,7,4],[2,8,6,1,3,5,7,4],[5,7,2,6,3,1,8,4],[3,6,2,7,5,1,8,4],[6,2,7,1,3,5,8,4],[3,7,2,8,6,4,1,5],[6,3,7,2,4,8,1,5],[4,2,7,3,6,8,1,5],[7,1,3,8,6,4,2,5],[1,6,8,3,7,4,2,5],[3,8,4,7,1,6,2,5],[6,3,7,4,1,8,2,5],[7,4,2,8,6,1,3,5],[4,6,8,2,7,1,3,5],[2,6,1,7,4,8,3,5],[2,4,6,8,3,1,7,5],[3,6,8,2,4,1,7,5],[6,3,1,8,4,2,7,5],[8,4,1,3,6,2,7,5],[4,8,1,3,6,2,7,5],[2,6,8,3,1,4,7,5],[7,2,6,3,1,4,8,5],[3,6,2,7,1,4,8,5],[4,7,3,8,2,5,1,6],[4,8,5,3,1,7,2,6],[3,5,8,4,1,7,2,6],[4,2,8,5,7,1,3,6],[5,7,2,4,8,1,3,6],[7,4,2,5,8,1,3,6],[8,2,4,1,7,5,3,6],[7,2,4,1,8,5,3,6],[5,1,8,4,2,7,3,6],[4,1,5,8,2,7,3,6],[5,2,8,1,4,7,3,6],[3,7,2,8,5,1,4,6],[3,1,7,5,8,2,4,6],[8,2,5,3,1,7,4,6],[3,5,2,8,1,7,4,6],[3,5,7,1,4,2,8,6],[5,2,4,6,8,3,1,7],[6,3,5,8,1,4,2,7],[5,8,4,1,3,6,2,7],[4,2,5,8,6,1,3,7],[4,6,1,5,2,8,3,7],[6,3,1,8,5,2,4,7],[5,3,1,6,8,2,4,7],[4,2,8,6,1,3,5,7],[6,3,5,7,1,4,2,8],[6,4,7,1,3,5,2,8],[4,7,5,2,6,1,3,8],[5,7,2,6,3,1,4,8]]
```
```
Examples> list' prog5
[1,2,3,4,5,6]
Examples> list' prog6
[1,2]
Examples> list' prog7
[1,-1,1,2,-2,2,3,-3,3,4,-4,4,5,-5,5,6,-6,6]
Examples> list' prog8
[1,3,2]
```

### OCaml implementation

The Ocaml implementation contains the general modules for functors and monads.
The `Prog` module is the equivalent of the paper's `Prog` datatype, and the `Alg`
module implements the `fold` function for functorial algebras (equivalent to paper Figure 1).

Start the Ocaml code using the following commands:
```
ocaml
# #use "examples.ml";;
```

###### Concurrency

We test the following concurrency example program
```
let prog : unit -> unit prog
         = fun () -> spawn (say "hello " >> say "world ")
                           (say "goodbye " >> say "cruel " >> say "world ")
```

Interpreting this program gives us the following:
```
# Concurrent.example (Concurrent.prog ());;
hello
goodbye
world
cruel
world
- : (unit, string) result = Error "main process killed"
```


###### Nondeterminism with Once

We test the following nondeterminism example program
```
let prog = let x1 = once (cor (return 1) (return 5))
    in let x2 = fun x -> cor (return x) (return (x + 1))
    in x1 >>= x2
```

Interpreting this program gives us the following:
```
# NonDet.example;;
- : int list = [1; 2]
```

###### Local State

We test the following local state example program
```
let prog = put "x" 1 >>
       put "y" 1 >>
       local "x" 100 (incr "x" 100 >>
               ((get "x") >>=
                (fun v -> incr "y" v))) >>
       incr "x" 2 >>
       incr "y" 2 >>
       (get "x") >>= (fun vx ->
          ((get "y")>>= (fun vy ->
            return (vx, vy))))
```

Interpreting this program gives us the following:
```
# LocalState.example;;
- : int * int = (3, 203)
```
