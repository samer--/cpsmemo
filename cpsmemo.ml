(* cpsmemo - Nondeterministic, left recursive memoisations using delimited continuations
 * (c) Samer Abdallah, 2017
 *)

(* -- utilities -- *)
let id x = x               (* identity function *)
let ( ** ) f g x = f (g x)  (* function composition *)
let cons x xs = x :: xs
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y
let flip f x y = f y x
let rec iota = function | 0 -> [] | n -> iota (n-1) @ [n-1]

module type TYPE = sig type t end

(* -- fixed point combinators -- *)

let fix f = (let rec fp x = f fp x in fp)
let fix2 ( (f : ('a -> 'b) * ('c -> 'd) -> 'a -> 'b),
           (g : ('a -> 'b) * ('c -> 'd) -> 'c -> 'd) ) =
  let rec fp x = f (fp,gp) x
  and     gp x = g (fp,gp) x
  in (fp,gp)

(* variadic fixed point combinator *)
let rec fixlist l = List.map (fun li x -> li (fixlist l) x) l

(* -- Basic monads ------------------------------------------------ *)

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

module type MONADREF = sig
  include MONAD
  type 'a ref
  val new_ref : 'a -> 'a ref m
  val get_ref : 'a ref -> 'a m
  val put_ref : 'a ref -> 'a -> unit m
end

module MonadOps (M : MONAD) = struct
  open M

  let (>>=) = bind
  let (>>) m1 m2 = bind m1 (fun _ -> m2)

  let fmap f = return ** f
  let liftM op m = m >>= return ** op
  let liftM2 op m n = m >>= fun x -> n >>= (return ** op x)

  let rec mapM f = function 
    | [] -> return []
    | x :: xs -> liftM2 cons (f x) (mapM f xs)
end

module ListT (M : MONAD) = struct
  type 'a m = 'a list M.m
  
  module MO = MonadOps (M)
  open MO

  let return x = M.return [x]
  let bind m f = m >>= mapM f >>= (M.return ** List.concat)
  let lift m   = M.bind m return
  let mzero () = M.return []
  let mplus f g = liftM2 List.rev_append f g
end

(* -- Monad of mutable references using state monad ---- *)

(* -- universal type, taken from Filinski -- *)
module Dynamic = struct 
  exception Dynamic
  type t = Dyn of (unit->unit)

  let newdyn () : ('a -> t) * (t -> 'a) = 
    let r = ref None in
    ( (fun a -> Dyn (fun () -> r := Some a)),
      (fun (Dyn d) -> r := None; d (); 
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
  module M = BatMap.Make(BatInt)

  type t =  int * Dynamic.t M.t
  type 'a loc = int * ('a -> Dynamic.t) * (Dynamic.t -> 'a)

  let empty = (0,M.empty)
  let get (j,_,outd) (_,m)     = outd (M.find j m) 
  let put (j,ind,_) x (i,m)    = (i,M.add j (ind x) m)
  let upd (j,ind,outd) f (i,m) = (i,M.modify j (ind ** f ** outd) m)
  let new_loc (i,m) = let (ind,outd) = Dynamic.newdyn () in 
                      ((i,ind,outd),(i+1,m))
end

module StateM (State : TYPE) = struct
  type 'a m = State.t -> 'a * State.t 
  type state = State.t

  let return x s = (x,s)
  let bind m f s = let (x,s')=m s in f x s'
  let get s   = (s,s)
  let put s _ = ((),s)
  let upd f s = ((),f s)
end

module Ref = struct
  include StateM(Store)
  type 'a ref = 'a Store.loc

  let put_ref loc x  = upd (Store.put loc x)
  let upd_ref loc f  = upd (Store.upd loc f)
  let get_ref loc    = bind get (return ** Store.get loc)
  let new_ref x = bind Store.new_loc (fun loc ->
                  bind (put_ref loc x) (fun _ -> return loc))
  let run m = fst (m Store.empty)
end


(* -- Memoisation monad without access to memo tables ------- *)

module type MONADMEMO = sig
  include MONAD

  val memo : ('c -> 'a -> 'b m) -> ('c -> 'a -> 'b m) m
end

module MemoOps (M : MONADMEMO) = struct
  include MonadOps(M)

  let mem f = M.memo (fun () x -> f x) >>= fun mf -> M.return (mf ())
  let memrec f = liftM fix (M.memo f)
  let memrec2 (f,g) = liftM2 (curry fix2) (M.memo f) (M.memo g)
end

module ContT (W : TYPE) (M : MONAD) = struct
  type 'a m = {run: ('a -> W.t M.m) -> W.t M.m}

  let return x = {run = fun k -> k x}
  let bind m f = {run = fun k -> m.run (fun x -> (f x).run k)}
  let shift f  = {run = fun k -> (f (return ** k)).run id}
  let lift m   = {run = fun k -> M.bind m k}
end

module MemoT (Ref : MONADREF) : sig
  type 'a m

  val memo  : ('a -> 'b -> 'c m) -> ('a -> 'b -> 'c m) m
  val return : 'a -> 'a m
  val bind   : 'a m -> ('a -> 'b m) -> 'b m
  val mzero  : unit -> 'a m
  val mplus  : 'a m -> 'a m -> 'a m
  val run    : 'a m -> 'a list Ref.m

end = struct
  module ND = ListT (Ref)
  module CC = ContT (Dynamic) (ND)
  module CO = MonadOps (CC)
  include CC

  let run m =
    let (ind,outd) = Dynamic.newdyn () in
    ND.bind (m.run (ND.return ** ind)) (ND.return ** outd)

  let liftRef m  = lift (ND.lift m)
  let mzero ()  = {run = fun k -> ND.mzero ()}
  let mplus f g = {run = fun k -> ND.mplus (f.run k) (g.run k)}
  let msum      = List.fold_left mplus (mzero ())

  let memo finc = let open CO in
    liftRef (Ref.new_ref BatMap.empty) >>= (fun loc ->
    let upd_entry x e t = liftRef (Ref.put_ref loc (BatMap.add x e t)) in
    return (fun p x ->
      liftRef (Ref.get_ref loc) >>= fun table ->
      try let (res,conts) = BatMap.find x table in
        shift (fun k -> upd_entry x (res,k::conts) table >>
                        msum (List.map k res))
      with Not_found ->
        shift (fun k -> upd_entry x ([],[k]) table >>
                        finc p x >>= fun y ->
                        liftRef (Ref.get_ref loc) >>= fun table' ->
                        let (res,conts) = BatMap.find x table' in
                        if List.mem y res then mzero ()
                        else upd_entry x (y::res,conts) table' >>
                             msum (List.map (fun k -> k y) conts))))
end

module Fibonacci (M : MONAD) = struct
  include MonadOps (M)
  let fib f = function | 0 -> M.return 0 | 1 -> M.return 1
                       | n -> liftM2 (+) (f (n-1)) (f (n-2))
end

module TransClose (M : MONADPLUS) = struct
  open M
  let edge = function | "a" -> return "b"
                      | "b" -> return "c"
                      |  _  -> mzero ()
  let path p x = mplus (edge x) (bind (p x) p) 
end

module Test = struct
  module MM = MemoT (Ref)
  module FF = Fibonacci (MM)
  module TC = TransClose (MM)
  module MO = MemoOps (MM)
  open MO

  let test_fib n = Ref.run (MM.run (memrec FF.fib >>= fun fib -> fib n))
  let test_path x = Ref.run (MM.run (memrec TC.path>>= fun path -> path x))
end


(* -- Memoisation monad with access to memo tables ------- *)

module type MONADMEMOTABLE = sig
  include MONAD
  module Nondet : MONADPLUS

  type ('a,'b) table = ('a * 'b list) list
  val run : 'a Nondet.m -> 'a list m
  val memo : ('c -> 'a -> 'b Nondet.m) -> 
             (('a,'b) table m * ('c -> 'a -> 'b Nondet.m) ) m
end

module MemoTabOps (M : MONADMEMOTABLE) = struct
  module MO = MonadOps (M)
  open M
  open MO

  let mem f = memo (fun () x -> f x) >>= fun (gt,mf) -> return (gt,mf ())
  let memrec f = memo f >>= fun (g,mf) -> return (g,fix mf)
  let memrec2 (f,g) = memo f >>= fun (get_f,mf) ->
                      memo g >>= fun (get_g,mg) ->
                      let (fp,gp) = fix2 (mf,mg) in
                      return ((get_f,get_g),(fp,gp))

end

module type COLLECTION = sig
  type 'a t
  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module ListC = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add x xs = x :: xs
  let fold = List.fold_right
end

module MemoTabT (Ref : MONADREF) (Col : COLLECTION) = struct
  module ND = ListT (Ref)
  include Ref

  module Nondet = struct
    include ContT (Dynamic) (ND)
    let mzero ()  = {run = fun k -> ND.mzero ()}
    let mplus f g = {run = fun k -> ND.mplus (f.run k) (g.run k)}
  end

  module RefO = MonadOps (Ref)
  module CCO = MonadOps (Nondet)

  type ('a,'b) table = ('a * 'b list) list

  let run (m : 'a Nondet.m) : 'a list Ref.m =
    let (ind,outd) = Dynamic.newdyn () in
    ND.bind (m.run (ND.return ** ind)) (ND.return ** outd)

  let memo finc = let open RefO in 
    new_ref BatMap.empty >>= (fun loc ->

    let liftRef m  = Nondet.lift (ND.lift m) in
    let sanitize (x,(s,_)) = (x, Col.fold cons s []) in
    let upd_entry x e t = liftRef (put_ref loc (BatMap.add x e t)) in

    return (
      get_ref loc >>= return ** List.map sanitize ** BatMap.bindings, 
      ( fun p x -> let open CCO in let open Nondet in
          liftRef (get_ref loc) >>= fun table ->
          try let (res,conts) = BatMap.find x table in
            shift (fun k -> upd_entry x (res,k::conts) table >>
                            Col.fold (mplus ** k) res (mzero ()))
          with Not_found ->
            shift (fun k -> upd_entry x (Col.empty, [k]) table >>
                            finc p x >>= fun y ->
                            liftRef (get_ref loc) >>= fun table' ->
                            let (res,conts) = BatMap.find x table' in
                            if Col.mem y res then mzero ()
                            else upd_entry x (Col.add y res,conts) table' >>
                                 List.fold_right (fun k -> mplus (k y)) conts (mzero ())))))
end


(* -- Parser interface build on any MONADPLUS --------*)

module Parser (M : MONADPLUS) = struct
  let ( *> ) f g xs = M.bind (f xs) g
  let ( <|> ) f g xs = M.mplus (f xs) (g xs)
  let epsilon xs = M.return xs 
  let term x = function | y::ys when x=y -> M.return ys
                        | _ -> M.mzero ()
end

module type GRAMMAR = functor (MM : MONADMEMOTABLE) -> sig 
  val grammar : ((bytes * (bytes list, bytes list) MM.table MM.m) list *
                 (bytes -> bytes list -> bytes list MM.Nondet.m)) MM.m
  val sentence : int -> bytes list
end

(* --- timing and testing ------------------------ *)
let timeit thunk =
  let time_start = Sys.time () in
  let result = thunk () in
  Printf.printf "Time: %g s" (Sys.time () -. time_start);
  print_endline "";
  result

let words s = BatString.nsplit s ~by:" "

module MM = MemoTabT (Ref) (BatSet)

module TestG (G: GRAMMAR) = struct
  (* module MM = MemoTabT (Ref) (ListC) *)
  module GM = G (MM)
  include Parser (MM.Nondet)
  include MonadOps (MM)
  include MemoTabOps (MM)
  open MM

  let success = function | [] -> true | _ -> false


  let get_table (label, getter) = 
    getter >>= fun table ->
    return (label, table)

  let sent i = GM.sentence i
  let parse start input = 
    Ref.run ( GM.grammar >>= fun (getters, get_nonterm) ->
              run ((get_nonterm start) input) >>= fun results ->
              mapM get_table getters >>= fun tables ->
              return (results, tables) )

  let profile start input = timeit (fun () -> fst (parse start input))
end

module GrammarOps (MM: MONADMEMOTABLE) = struct
  include Parser (MM.Nondet)
  include MonadOps (MM)
  include MemoTabOps (MM)
  include MM
end

module Johnson (MM : MONADMEMOTABLE) = struct
	include GrammarOps (MM)

  let success = function | [] -> true | _ -> false
  let grammar = 
    let v   = term "likes" <|> term "knows" in
    let pn  = term "Kim" <|> term "Sandy" in 
    let det = term "every" <|> term "no" in
    let n   = term "student" <|> term "professor" in
    memrec (fun np -> det *> n <|> pn <|> np *> term "'s" *> n) >>= fun (get_np,np) ->
    memrec2 ((fun (vp,s) -> v *> np <|> v *> s),
             (fun (vp,s) -> np *> vp))                          >>= fun ((get_vp,get_s),(vp,s)) -> 
    return ([("s",get_s); ("np",get_np); ("vp",get_vp)], 
            function | "s" -> s
                     | "vp" -> vp)

  let sentence = List.nth []
end

module Frost1 (MM: MONADMEMOTABLE) = struct
	include GrammarOps (MM)

  let grammar = 
    let t = term in
    mem (t "boy" <|> t "girl" <|> t "man" <|> t "woman") >>= fun (gn, n) ->
    mem (t "knows" <|> t "respects" <|> t "loves") >>= fun (gv,v) ->
    mem (t "helen" <|> t "john" <|> t "pat"      ) >>= fun (gpn, pn) ->
    mem (t "and" <|> t "or"                      ) >>= fun (gconj, conj) ->
    mem (t "every" <|> t "some"                  ) >>= fun (gdet, det) ->
    mem (det *> n                                ) >>= fun (gdp, dp) ->
    mem (pn <|> dp                               ) >>= fun (gnp', np') ->
    memrec (fun np -> np' <|> np *> conj *> np   ) >>= fun (gnp, np) ->
    memrec (fun vp -> v <|> vp *> conj *> vp     ) >>= fun (gvp, vp) ->
    mem (np *> vp *> np) >>= fun (gs,s) ->
    return (["np",gnp; "s",gs], (function "np" -> np | "s" -> s))

  let s1 = "every boy or some girl and helen and john or pat " 
  let s2 = s1^"knows and respects or loves "
             ^"every boy or some girl and pat or john and helen"
  let sentence = List.nth [words s1; words s2]
end

let replicate n x = let rec r = function 0 -> [] | n -> x::r (n-1) in r n

module FrostAmbig (MM: MONADMEMOTABLE) = struct
	include GrammarOps (MM)

  let grammar = 
    let a = term "a" in 
    let s = fix (fun s -> a *> s *> s <|> epsilon) in
    memrec (fun sm -> a *> sm *> sm <|> epsilon) >>= fun (gsm, sm) ->
    memrec (fun sml -> sml *> sml *> a <|> epsilon) >>= fun (gsml, sml) ->
    memrec2 ((fun (smml, smml') -> smml *> smml' <|> epsilon),
             (fun (smml, smml') -> smml *> a)) >>= fun ((gsmml,_), (smml,_)) ->
    return (["s", return []; "sm",gsm; "sml",gsml; "smml",gsmml],
            function "s" -> s | "sm" -> sm | "sml" -> sml | "smml" -> smml)

  let sentence i = replicate i "a"
end

module T1 (MM: MONADMEMOTABLE) = struct
	include GrammarOps (MM)
	
	let sentences i = (words "n v d n") @ List.concat (replicate i (words "p d n"))
	let grammar = 
	  memrec2 ((fun (np,pp) -> term "n" <|> term "d" *> term "n" <|> np *> pp),
             (fun (np,pp) -> term "p" *> np)) >>= fun ((gnp,gpp), (np,pp)) ->
		memrec (fun vp -> term "v" *> np <|> vp *> pp) >>= fun (gvp, vp) ->
		mem (np *> vp) >>= fun (gs, s) ->
		return (["s",gs], s)
end

module Tomita2 (MM: MONADMEMOTABLE) = struct
	include GrammarOps (MM)
	
  (* polyadic memoising fixed point *)
  let memrec_list fops =
     mapM memo fops >>= fun memos_getters ->
     let (getters, mfops) = List.split memos_getters in
     let mfuns = fixlist mfops in
     return (List.combine getters (fixlist mfops))

	let sentence i = (words "n v d n") @ List.concat (replicate i (words "p d n"))

  (* open recursive grammar - a lot of mutual recursion here *)
  let [advm; adjm; nm; vc; np0; np1; np; pp; s; vp; dir; start] = List.map (fun i l -> List.nth l i) (iota 12)

  let rules = 
		let t = term in 
		[ (* advm *) (fun g -> t "a" *> advm g <|> t "a" <|> advm g *> t "c" *> advm g)
    ; (* adjm *) (fun g -> t "j" <|> t "j" *> adjm g <|> advm g *> t "j" <|> adjm g *> t "c" *> adjm g)
    ; (* nm *)   (fun g -> t "n" <|> t "n" *> nm g)
    ; (* vc *)   (fun g -> t "x" *> t "v" <|> t "v")
    ; (* np0 *)  (fun g -> nm g <|> adjm g *> nm g <|> t "d" *> nm g <|> t "t" *> adjm g *> nm g)
    ; (* np1 *)  (fun g -> adjm g *>  np0 g *> pp g *> pp g
                       <|> adjm g *> np0 g *> pp g
                       <|> adjm g *> np0 g
                       <|> np0 g *> pp g
                       <|> np0 g
                       <|> np0 g *> pp g *> pp g)
    ; (* np *)   (fun g -> np g *> t "c" *> np g
                       <|> np1 g *> t "t" *> s g
                       <|> np1 g *> s g
                       <|> np1 g)
    ; (* pp *)   (fun g -> pp g *> t "c" *> pp g <|> t "p" *> np g)
    ; (* s *)    (fun g -> np g *> vp g *> pp g *> pp g
                       <|> np g *> vp g *> pp g
                       <|> np g *> vp g
                       <|> s g *> t "c" *> s g)
    ; (* vp *)   (fun g -> vc g *> np g <|> vp g *> t "c" *> vp g <|> vc g)
    ; (* dir *)  (fun g -> dir g *> t "c" *> dir g
                       <|> pp g *> vp g
                       <|> vp g
                       <|> vp g *> pp g)
    ; (* start *)(fun g -> dir g <|> np g <|> s g)
    ]

  let grammar = memrec_list rules >>= fmap (flip List.nth 11) >>= fun (gs,s) ->
                return (["s",gs], fun "s" -> s)
end

