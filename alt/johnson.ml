(** Trying out ideas from Johnson 1995:
  
   This paper shows how to do memoisation with parser combinators in Lisp, using
   mutable variables. My O'Caml translation uses references to acheive the same effect.
   However, because of O'Caml's immutable let bindings, I had to implement my own
   fix-point combinator. This is quite good in a way, because it means that all
   I need to implement for each memoising approach is a [memoinc] function, an
   imcomplete recursive function as described by Bruce McAdam:

   @author Samer Abdallah
 *)


let id x = x
let ( ** ) f g x = f (g x)

module Dynamic : sig 
  exception Dynamic
  type dyn
  val newdyn : unit -> ('a -> dyn) * (dyn -> 'a)
end= struct
  (* This modules exploits impure computations (mutable references) to implement
   * a sort of universal type. I copied it from Filinski. The trick is to use a
   * mutable reference as a communication channel between a thunk of fixed type 
   * unit -> unit and a polymorphic function to read the value. 
   *)
  exception Dynamic
  type dyn = Dyn of (unit->unit)

  let newdyn () = 
    let r = ref None in
    ( (fun a -> Dyn (fun () -> r:=Some a)),
      (fun (Dyn d) ->
        r := None; d (); 
        match !r with
        | Some a -> a
        | None -> raise Dynamic))
end

module Store : sig

  type t
  type 'a loc

  val empty : t
  val new_loc : t -> 'a loc * t
  val get : 'a loc -> t -> 'a 
  val put : 'a loc -> 'a -> t -> t
  val upd : 'a loc -> ('a -> 'a) -> t -> t

end = struct

  (* module M = Map.Make(struct type t=int let compare = (-) end) *)
  module M = BatMap.IntMap

  open Utils.Dynamic

  (* the map is a standard map from integers to dyn values *)
  type map = dyn M.t

  (* the store is a map and the integer value of the next unused key *)
  type t =  int * map
  type 'a loc = int * ('a -> dyn) * (dyn -> 'a)

  let empty = (0,M.empty)

  let get (j,_,outd) (_,m) = outd (M.find j m) 
  let put (j,ind,_) x (i,m) = (i,M.add j (ind x) m)
  let upd (j,ind,outd) f (i,m) = (i,M.modify j (fun x -> ind (f (outd x))) m)

  (* new_loc without initialisation *)
  let new_loc (i,m) =
    let (ind,outd) = newdyn () in ((i,ind,outd),(i+1,m))
end

(** Fixed point combinator *)
let fix f = let rec p x = f p x in p
let fix2 (f,g) =
  let rec fp x = f (fp,gp) x
  and     gp x = g (fp,gp) x
  in (fp,gp)

module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end

module type MONADPLUS = sig
  include MONAD

  val mzero : unit -> 'a m
  val mplus : 'a m -> 'a m -> 'a m
end

module MonadOps (M : MONAD) = struct
  open M

  let (>>=) = bind
  let (=<<) f m = bind  m f
  let (>>) m1 m2 = bind m1 (fun _ -> m2)
  let fmap f m = m >>= return ** f

  let rec sequence = function 
    | [] -> return []
    | m::ms -> m >>= (fun x -> sequence ms >>= (fun xs -> return (x::xs)))

  let sequence_ ms = sequence ms >> return ()

  let mapM f = sequence ** (List.map f)
  let mapM_ f = sequence_ ** (List.map f)
  let forM s f = mapM f s
  let forM_ s f = mapM_ f s

  let liftM op m1 = m1 >>= (fun x -> return (op x))
  let liftM2 op m1 m2 = m1 >>= (fun x -> m2 >>= (fun y -> return (op x y)))
end


module type TYPE = sig
  type t
end

module type MONADSTATE = sig
  include MONAD
  type state

  val get : state m
  val put : state -> unit m
  val upd : (state -> state) -> unit m
end

module StateM (State : TYPE) = struct
  (* Functional representation of state monad. *)
  type 'a m = State.t -> 'a * State.t 
  type state = State.t

  let return x s = (x,s)
  let bind m f s = let (x,s')=m s in f x s'

  let get s = (s,s)
  let put s _ = ((),s)
  let upd f s = ((),f s)
end

(* State monad transformer *)
module StateT (State : TYPE) (M : MONAD) = struct
  type 'a m = State.t -> ('a * State.t) M.m
  type state = State.t

  let return x s = M.return (x,s)
  let bind m f s = M.bind (m s) (uncurry f)

  let get s = M.return (s,s)
  let put s _ = M.return ((),s)
  let upd f s = M.return ((),f s)

  let lift m s = M.bind m (fun x -> M.return (x,s))
end

module ListT (M : MONAD) : sig
  type 'a m = 'a list M.m

  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val mplus : 'a m -> 'a m -> 'a m
  val mzero : unit -> 'a m
  val lift : 'a M.m -> 'a m

end = struct
  type 'a m = 'a list M.m
  module MO = MonadOps (M)
  open MO
  open List

  let return x = M.return [x]
  let bind m f = m >>= mapM f >>= (M.return ** concat)
  let mplus f g = f >>= fun xs -> g >>= fun ys -> M.return (xs @ ys)
  let mzero () = M.return []
  let lift m = M.bind m return
end

(**** MEMO utilities *****)

(** A module type for monads that provide memoisation as a monadic effect. *)
module type MEMOMONAD = sig
  include MONAD
  val memoinc : ('a -> 'b -> 'c m) -> ('a -> 'b -> 'c m) m
end

(** Given a monad that provides memoisation, this
 * module provides some monadic combinators that produce a memoising fixed-point
 * from an incomplete recursive monadic computation. *)
module MemoMonadOps (M : MEMOMONAD) = struct
  include MonadOps(M)
  let memo f = M.memoinc (fun () x -> f x) >>= fun finc -> M.return (finc ())
  let memorec f = liftM fix (M.memoinc f)
  let memorec2 (f,g)  = liftM2 (curry fix2) (M.memoinc f) (M.memoinc g)
end

(** A signature for monads that provided polypmorphic updatable references.
 * Modelled on Haskell's [Control.Monad.ST]. The underlying implementation,
 * whatever it is, is hidden. *)
module type STMONAD = sig
  include MONAD
  type 'a ref 

  val new_ref : 'a -> 'a ref m
  val put_ref : 'a ref -> 'a -> unit m
  val upd_ref : 'a ref -> ('a -> 'a) -> unit m 
  val get_ref : 'a ref -> 'a m 
end

module StoreST (S : functor(T:TYPE) -> MONADSTATE with type state = T.t) : 
    STMONAD with type 'a m = 'a S(Store).m = struct
  module SS = S(Store)      (* state monad over stores *)
  module SO = MonadOps(SS)
  open SO
  
  type 'a m = 'a SS.m
  type 'a ref = 'a Store.loc

  let return = SS.return
  let bind = SS.bind

  let put_ref loc x  = SS.upd (Store.put loc x)
  let upd_ref loc f  = SS.upd (Store.upd loc f)
  let get_ref loc    = SS.get >>= SS.return ** Store.get loc 
  let new_ref x = 
    let loc = Store.new_loc () in 
    bind (put_ref loc x) (fun _ -> return loc)
end

module StateMemoM (ST : STMONAD) : MEMOMONAD with type 'a m = 'a ST.m = struct
  module SO = MonadOps (ST)
  open SO
  open ST

  type 'a m = 'a ST.m
  let return = ST.return
  let bind = ST.bind

  (* memoinc :  ('a -> 'b -> 'c m) -> ('a -> 'b -> 'c m) m *)
  let memoinc f_inc =
    new_ref BatMap.empty >>= fun loc ->
    return (fun comp x ->
      get_ref loc >>= fun tab ->
          try return (BatMap.find x tab) 
          with Not_found -> 
            f_inc comp x >>= fun v ->
            upd_ref loc (BatMap.add x v) >> return v)
end

(* monadic implementation of memoising nondet monad in CPS *)
module ContMemoM : sig

  type 'a m
  type ('a,'b) table = ('a,'b list) BatMap.t

  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val mzero : unit -> 'a m
  val mplus : 'a m -> 'a m -> 'a m
  val memoinc : ('a -> 'b -> 'c m) -> ('a -> 'b -> 'c m) m
  val choose : 'a list -> 'a m
  val nubify : ('a -> 'b -> 'c m) -> ('a -> 'b -> 'c m)

  val run : 'a m -> 'a list

end = struct
  module SST = StoreST (StateM) 
  module LST = ListT (SST)
  open Dynamic 

  type ans = dyn LST.m
  type 'a m = ('a -> ans) -> ans
  type ('a,'b) table = ('a,'b list) BatMap.t

  let run m =
    let (ind,outd) = newdyn () in
    fst (LST.bind (m (LST.return ** ind)) (LST.return ** outd) Store.empty)
    (* could also remove duplicates here *)


  (* MONAD implementation *)
  let return x k = k x  
  let bind m f k = m (fun x -> f x k) 
  let lift m = LST.bind m 
        
  (* MONADPLUS implementation *)
  let mzero () _ s = LST.mzero () s 
  let mplus f g k s = LST.mplus (f k) (g k) s
  let msum = List.fold_left mplus (mzero ()) 
  let choose xs k = List.fold_left LST.mplus (LST.mzero ()) (List.map k xs)
  let nubify f p x = choose @@ nub @@ run @@ f p x

  let new_ref x k s   = let (l,s') = SST.new_ref x s in k l s'
  let get_ref loc k s = let (x,s') = SST.get_ref loc s in k x s'
  let put_ref loc x k s = let (y,s') = SST.put_ref loc x s in k y s'

  type ('a,'b) entry = Entry of ('a list * ('a->'b) list)

  (* let memoinc finc = let open MO in *) 
  (*   bind (new_ref BatMap.empty) (fun loc -> *)
  (*   let upd_entry x entry table = LST.lift (SST.put_ref loc (BatMap.add x entry table)) in *)
  (*   return (fun p x -> *) 
  (*     bind (get_ref loc) (fun table -> *)
  (*     try let Entry (res,conts) = BatMap.find x table in *) 
  (*       fun k -> upd_entry x (Entry (res,k::conts)) table >> msum (List.map k res) *) 
  (*     with Not_found -> (1* first call *1) *) 
  (*       fun k -> *) 
  (*         upd_entry x (Entry ([],[k])) table >> *)
  (*         finc p x (fun y -> LST.lift (SST.get_ref loc) >>= fun table' -> *)
  (*                            let Entry (res,conts) = BatMap.find x table' in *)
  (*                            if List.mem y res then LST.mzero () *)
  (*                            else upd_entry x (Entry (y::res,conts)) table' >> *)
  (*                                 msum (List.map (fun k -> k y) conts))))) *)

  (* let get_table = get_ref loc >>= return ** (BatMap.map (fun (Entry (res,_)) -> res)) *) 

  let shift e k = e (fun x k' -> k' (k x)) id
  let memoinc finc = 
    let ( >>= ) = bind in
    let ( >> ) m n = m >>= fun _ -> n in 
    new_ref BatMap.empty >>= (fun loc ->
    let upd_entry x entry table = put_ref loc (BatMap.add x entry table) in
    return (fun p x -> 
      get_ref loc >>= fun table ->
      try let Entry (res,conts) = BatMap.find x table in 
        shift (fun k -> upd_entry x (Entry (res,k::conts)) table >> 
                        msum (List.map k res))
      with Not_found -> 
        shift (fun k -> upd_entry x (Entry ([],[k])) table >> 
                        finc p x >>= fun y ->
                        get_ref loc >>= fun table' ->
                        let Entry (res,conts) = BatMap.find x table' in
                        if List.mem y res then mzero ()
                        else upd_entry x (Entry (y::res,conts)) table' >>
                             msum (List.map (fun k -> k y) conts))))
end

(***** PARSING *****)

(* state T on ContMemoM for nondet and memoing *)
module Parse (Token : TYPE) = struct
  type t = Token.t
  type s = t list (* type of parser state *)

  module SLS = StateT (struct type t = Token.t list end) (ContMemoM)
  module MO = MonadOps (SLS)
  module MMO = MemoMonadOps (ContMemoM)
  include MMO
  open SLS
  open MO

  let return = ContMemoM.return
  let mzero () = SLS.lift (ContMemoM.mzero ())
  let mplus f g s = ContMemoM.mplus (f s) (g s)

  let term x = get >>= (function
                        | y::ys when x=y -> put ys >> SLS.return ()
                        | _ -> mzero ())

  let epsilon = SLS.return ()

  let ( || ) = mplus
  let ( >> ) = MO.( >> )
  let opt f  = epsilon || f
  let rec star f    = epsilon || f >> star f

  let parse cat x = ContMemoM.run (ContMemoM.bind cat (fun s -> s x))
end

module STRING = struct
  type t = string
  let show s = s
end

(******** TESTING *******)

(* from Hafiz and Frost
 
 1. s    = term `x` *> s *> s <+> empty
 2. sm   = memoize SM $ term `x` *> sm *> sm <+> empty
 3. sml  = memoize SML $ sml *> sml *> term `x` <+> empty
 4. smml = memoize SMML $ smml *> (memoize SMML` $ smml *> term `x`) <+> empty

 Input length No. of parses    s     sm     sml     smml
 -------------------------------------------------------
   6                 132      1.22    -      -       -
   12            208,012       *      -      -      0.02
   24          1.289e+12       *     0.08   0.13    0.06
   48          1.313e+26       *     0.83   0.97    0.80
*)

(** ordinary incomplete Fibonacci function *)
let fib_inc fib' = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib' (n-1) + fib' (n-2)


module RecTest = struct
  module ND = ContMemoM
  include MonadicFib (ContMemoM)
  include MonadOps(ND)
  include MemoMonadOps(ND)
  include ND

  let ( || ) = mplus

  (* fib_inc_m : (int -> int m) -> (int -> int m) *)
  let fib_inc_m fib' = function
    | 0 -> M.return 0
    | 1 -> M.return 1
    | n -> liftM2 (+) (fib' (n-1)) (fib' (n-2))

  (* link : 'a -> 'a m *)
  let link = function
             | 'a' -> return 'b'
             | 'b' -> return 'c' || return 'd'
             | 'c' -> return 'd'
             | 'e' -> return 'c'
             | _   -> mzero () 

  (* recursive functions : 'a -> 'a m *)
  let rec path x = link x || (path x >>= path) 
  let rec path' x = (return x || path' x) >>= link 
  let rec path'' x = link x >>= (fun y -> return y || path'' y)

  (* incomplete functions : ('a -> 'a m) -> 'a -> 'a m *)
  let path_inc p x = link x || (p x >>= p) 
  let flaz_inc p x = p x || (p x || if x=5 then return x else mzero ())

  let test f x = run (memorec f >>= (|>) x)
  let fib = test fib_inc_m

  let step i = return 1 || return (i+1)
  let rec stepN p = function | 0 -> return 0
                             | n -> p (n-1) >>= step
  let dups d = function | 0 -> return 1 
                        | n -> (return 1 || return 1) >> d (n-1)

end


module TestParse5 = struct
  module PString = Parse5 (STRING)
  open PString

  let tr = trace (String.concat " ")

  let test x = parse ( 
    let v  = term "likes" || term "knows" in
    let det = term "every" || term "a" || term "the" || term "no" in
    let pn = term "Kim" || term "Sandy" in
    memorec (fun n -> term "fat" >> n  
                   || term "student" 
                   || term "professor" 
                   || term "fish") >>= fun n ->
    memorec (fun np -> pn 
                    || term "fat" >> np 
                    || np >> term "with fries" 
                    || det >> n 
                    || term "fish") >>= fun np ->
    memorec2 ( (fun (vp,s) -> v >> (np || s)),
               (fun (vp,s) -> np >> vp)) >>= fun (vp,s) ->
    return s
    ) x
end
