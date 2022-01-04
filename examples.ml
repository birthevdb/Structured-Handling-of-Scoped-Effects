module type FUNCTOR =
  sig
    type 'a f
    val fmap : ('a -> 'b) -> 'a f -> 'b f
  end

module type MONAD =
  sig
    include FUNCTOR
    type 'a m = 'a f
    val (>>=)  : 'a m -> ('a -> 'b m) -> 'b m
    val return : 'a -> 'a m
  end

module type PROG =
  functor (F : FUNCTOR) ->
  functor (G : FUNCTOR) ->
	sig
	  include MONAD
	  type 'a prog
	end

module Prog (F : FUNCTOR) (G : FUNCTOR) =
    struct
      type 'a prog = Return of 'a
                   | Call   of ('a prog) F.f
                   | Enter of (('a prog) prog) G.f
      type 'a f = 'a prog
      type 'a m = 'a prog
      let rec fmap : 'a 'b. ('a -> 'b) -> 'a prog -> 'b prog
      	= fun f m -> match m with
        | Return x  -> Return (f x)
        | Call op   -> Call (F.fmap (fmap f) op)
        | Enter scp -> Enter (G.fmap (fmap (fmap f)) scp)
      let return x = Return x
      let rec (>>=) m f = match m with
        | Return x  -> f x
        | Call op   -> Call (F.fmap (fun n -> n >>= f) op)
        | Enter scp -> Enter (G.fmap (fmap (fun n -> n >>= f)) scp)
      let (>>) f g = f >>= (fun _ -> g)
    end

module type ALG =
  functor (F : FUNCTOR) ->
  functor (G : FUNCTOR) ->
  functor (C : FUNCTOR) ->
    sig
      type endoalg = { callE  : 'x. ('x C.f) F.f -> 'x C.f ;
                       enterE : 'x. (('x C.f) C.f) G.f -> 'x C.f ;
                       ret     : 'x. 'x -> 'x C.f }
      type 'b basealg = { callB  : 'b F.f -> 'b ;
                          enterB : ('b C.f) G.f -> 'b }
      type 'a prog
      val hcata : endoalg -> 'a prog -> 'a C.f
      val fold  : endoalg -> 'd basealg -> ('a -> 'd) -> 'a prog -> 'd
    end

(* operator for function composition *)
let (<<) f g x = f(g(x));;

module Alg (F : FUNCTOR) (G : FUNCTOR) (C : FUNCTOR) =
  struct
    include Prog (F) (G)
    type endoalg = { callE  : 'x. ('x C.f) F.f -> 'x C.f ;
                     enterE : 'x. (('x C.f) C.f) G.f -> 'x C.f ;
                     ret     : 'x. 'x -> 'x C.f }
    type 'b basealg = { callB  : 'b F.f -> 'b ;
                        enterB : ('b C.f) G.f -> 'b }
    (* Catamorphism *)
    let rec hcata : 'a. endoalg -> 'a prog -> 'a C.f
      = fun alg m -> match m with
        | Return x  -> alg.ret x
        | Call op   -> (alg.callE << F.fmap (hcata alg)) op
        | Enter scp ->
            (alg.enterE << G.fmap (hcata alg << fmap (hcata alg))) scp
    let rec fold ea ba gen m = match m with
        | Return x  -> gen x
        | Call op   -> (ba.callB << F.fmap (fold ea ba gen)) op
        | Enter scp ->
            (ba.enterB << G.fmap (hcata ea << fmap (fold ea ba gen))) scp
  end

(* Example Concurrency *)

module IO =
  struct
    open Result
    type 'a io = unit -> ('a, string) result
    let fmap : 'a 'b . ('a -> 'b) -> 'a io -> 'b io
      = fun f m -> fun () -> match m () with
        | Ok x    -> Ok (f x)
        | Error e -> Error e
    let return x = fun () -> Ok x
    let (>>=) m f = fun () -> match m () with
      | Ok x    -> f x ()
      | Error e -> Error e
    let putStr : string -> unit io
      = fun s () -> Ok (Printf.printf "%s\n" s)
  end

module Act =
  struct
    open IO
    type 'a act = Kill | Act of 'a io
    type 'a f = 'a act
	let fmap : 'a 'b . ('a -> 'b) -> 'a act -> 'b act
     = fun f m -> match m with
  		| Kill  -> Kill
  		| Act g -> Act (fun () -> match g () with
        | Ok x    -> Ok (f x)
        | Error e -> Error e
      )
  end

module Con =
  struct
    type 'a con = Atomic of 'a
                | Spawn of 'a * 'a
    type 'a f = 'a con
    let fmap : 'a 'b . ('a -> 'b) -> 'a con -> 'b con
      = fun f m -> match m with
        | Atomic x -> Atomic (f x)
        | Spawn (x, y) -> Spawn (f x, f y)
  end

module Resumption =
  struct
    open IO
    open Result
    type 'a resumption = Done of ('a, string) result
                       | More of ('a resumption) io
    type 'a f = 'a resumption
    let rec fmap : 'a 'b 'm . ('a -> 'b) -> 'a resumption -> 'b resumption
      = fun f m -> match m with
        | Done (Ok x)    -> Done (Ok (f x))
        | Done (Error e) -> Done (Error e)
        | More xs        -> More (IO.fmap (fmap f) xs)
    let rec interleave : 'a resumption -> 'b resumption -> 'a resumption
      = fun r1 r2 -> match r1 with
        | Done (Ok x)    -> fmap (fun _ -> x) r2
        | Done (Error e) -> Done (Error e)
        | More m1        -> match r2 with
          | Done _  -> r1
          | More m2 -> More (IO.fmap (fun x1 ->
                       More (IO.fmap (fun x2 -> interleave x1 x2) m2)) m1)
    let rec retraction : 'a resumption -> 'a io
      = fun m -> match m with
        | Done x -> fun () -> x
        | More r -> IO.(>>=) r retraction
    let rec ijoin : ('a resumption) resumption -> 'a resumption
      = fun m -> match m with
        | Done (Ok x) -> x
        | Done (Error e) -> Done (Error e)
        | More r -> More (IO.fmap ijoin r)
  end

module Concurrent =
  struct
    open Act
    open Con
    open Resumption
    open IO
    include Prog (Act) (Con)
    include Alg (Act) (Con) (Resumption)
    (* Implementations *)
  	let call : ('a resumption) act -> 'a resumption
  		= fun m -> match m with
          | Kill -> Done (Error "main process killed")
          | Act x -> More x
    let enter : (('a resumption) resumption) con -> 'a resumption
        = fun m -> match m with
          | Atomic r -> More (retraction r)
          | Spawn (r1, r2) -> ijoin (interleave r1 r2)
    let ret : 'a -> 'a resumption
              = fun x -> Done (Ok x)
    (* Algebras *)
    let ea = { callE = call ; enterE = enter ; ret = ret }
    let ba = { callB = call ; enterB = enter }
    (* Example *)
    let act    : 'a io -> 'a prog
               = fun m -> Call (Act (IO.fmap return m))
    let kill   : 'a prog
               = Call Kill
    let spawn  : 'a prog -> 'b prog -> 'a prog
               = fun p q -> Enter (Con.fmap (fmap return) (Spawn (p, q >> kill)))
    let atomic : 'a prog -> 'a prog
               = fun p -> Enter (Con.fmap (fmap return) (Atomic p))
    let say : string -> 'a prog
            = act << putStr
    let rec mapM f lst : (string -> 'a prog) -> string list -> 'a prog
                       = mapM (fun x -> x) (fmap f lst)
    let prog : unit -> unit prog
             = fun () -> spawn (say "hello " >> say "world ")
                               (say "goodbye " >> say "cruel " >> say "world ")
    let example x = (retraction (fold ea ba ret x)) ()
  end

(* Example Non-determinism *)

module Choice =
  struct
    type 'a choice = Fail | Or of 'a * 'a
    type 'a f = 'a choice
  let fmap : 'a 'b . ('a -> 'b) -> 'a choice -> 'b choice
     = fun f m -> match m with
      | Fail -> Fail
      | Or (x, y) -> Or (f x, f y)
  end

module Once =
  struct
    type 'a once = Once of 'a
    type 'a f = 'a once
    let fmap : 'a 'b. ('a -> 'b) -> 'a once -> 'b once
    = fun f (Once x) -> Once (f x)
  end

module List =
  struct
    include List
    type 'a f = 'a list
    let fmap = List.map
  end

module NonDet =
  struct
    open Choice
    open Once
    include Prog (Choice) (Once)
    include Alg (Choice) (Once) (List)
    (* Implementations *)
    let call : ('a list) choice -> 'a list
      = fun m -> match m with
          | Fail -> []
          | Or (x, y) -> List.append x y
    let enter : (('a list) list) once -> 'a list
        = fun (Once lst) -> match lst with
          | [] -> []
          | (x :: _) -> x
    let ret : 'a -> 'a list
              = fun x -> x :: []
    (* Algebras *)
    let ea = { callE = call ; enterE = enter ; ret = ret }
    let ba = { callB = call ; enterB = enter }
    (* Example *)
    let fail = Call Fail
    let cor x y = Call (Or (x, y))
    let once x = Enter (Once (fmap return x))
    let prog = let x1 = once (cor (return 1) (return 5))
        in let x2 = fun x -> cor (return x) (return (x + 1))
        in x1 >>= x2
    let example = fold ea ba ret prog
  end

(* Example Local State *)

module State =
  struct
    type 'a state = Get of string * (int -> 'a)
                  | Put of string * int * 'a
    type 'a f = 'a state
    let fmap : 'a 'b . ('a -> 'b) -> 'a state -> 'b state
      = fun f st -> match st with
        | Get (ng, sg) -> Get (ng, f << sg)
        | Put (np, sp, xp) -> Put (np, sp, f xp)
  end

module Local =
  struct
    type 'a local = Local of string * int * 'a
    type 'a f = 'a local
    let fmap : 'a 'b . ('a -> 'b) -> 'a local -> 'b local
      = fun f (Local (n, s, x)) -> Local (n, s, f x)
  end

module C =
  struct
    (* Memory *)
    type mem = string -> int option
    let retrieve : string -> mem -> int
      = fun x m -> match (m x) with
        | None   -> 0
        | Some s -> s
    let update : string -> int -> mem -> mem
      = fun x s m y -> if x=y then Some s else m y
    (* c datatype *)
    type 'a c = mem -> ('a * mem)
    type 'a f = 'a c
    let mapfst : 'a 'b 'c . ('a -> 'b) -> 'a * 'c -> 'b * 'c
      = fun f (x, c) -> (f x, c)
    let fmap : 'a 'b . ('a -> 'b) -> 'a c -> 'b c
      = fun f g -> mapfst f << g
  end

module LocalState =
  struct
    open State
    open Local
    open C
    include Prog (State) (Local)
    include Alg (State) (Local) (C)
    (* Implementations *)
    let call : ('a c) state -> 'a c
      = fun m -> match m with
          | Get (ng, sg) -> fun m -> sg (retrieve ng m) m
          | Put (np, sp, xp) -> xp << update np sp
    let enter : (('a c) c) local -> 'a c
        = fun (Local (x, s, f)) m -> let (g, n) = f (update x s m)
                                        in g (update x (retrieve x m) n)
    let ret : 'a -> 'a c
              = fun x m -> (x, m)
    (* Algebras *)
    let ea = { callE = call ; enterE = enter ; ret = ret }
    let ba = { callB = call ; enterB = enter }
    (* Smart constructors *)
    let get x = Call (Get (x, return))
    let put x s = Call (Put (x, s, return ()))
    let local x s p = Enter (Local.fmap (fmap return) (Local (x, s, p)))
    let incr x i = (get x) >>= (fun v -> put x (v + i))
    (* Example *)
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
    let fst (x, _) = x
    let example = fst ((fold ea ba ret prog) (fun _ -> None))
  end

(* Example Depth Bound Search *)

module Depth =
  struct
    type 'a depth = Depth of int * 'a
    type 'a f = 'a depth
    let fmap : 'a 'b . ('a -> 'b) -> 'a depth -> 'b depth
     = fun f (Depth (n, x)) -> Depth (n, f x)
  end

module Carrier =
  struct
    type 'a carrier = int -> 'a list
    type 'a f = 'a carrier
    let fmap : 'a 'b . ('a -> 'b) -> 'a carrier -> 'b carrier
      = fun f g -> List.map f << g
  end

module Dbs =
  struct
    open Choice
    open Depth
    open Carrier
    include Prog (Choice) (Depth)
    include Alg (Choice) (Depth) (Carrier)
	(* Implementations *)
  	let call : ('a carrier) choice -> 'a carrier
  		= fun m -> match m with
          | Fail -> fun _ -> []
          | Or (f, g) -> fun d ->
                  if d=0 then [] else List.append (f (d-1)) (g (d-1))
    let enter : (('a carrier) carrier) depth -> 'a carrier
        = fun (Depth (n, f)) -> fun d -> List.concat (List.map (fun g -> g d) (f n))
    let ret : 'a -> 'a carrier
              = fun x _ -> [x]
  	let reply : ('a list) choice -> 'a list
  		= fun m -> match m with
          | Fail -> []
          | Or (x, y) -> List.append x y
    let exit : (('a list) carrier) depth -> 'a list
        = fun (Depth (n, x)) -> List.concat (x n)
    (* Algebras *)
    let ea = { callE = call ; enterE = enter ; ret = ret }
    let ba = { callB = reply ; enterB = exit }
    (* Example *)
    let depth d p = Enter (Depth (d, fmap return p))
  end
