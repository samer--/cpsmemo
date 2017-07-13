let id x = x               (* identity function *)
let ( ** ) f g x = f (g x)  (* function composition *)
let cons x xs = x :: xs
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

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
  let mplus f g = liftM2 (@) f g
end

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

module type TYPE = sig type t end

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

let fix f = (let rec fp x = f fp x in fp)
let fix2 ( (f : ('a -> 'b) * ('c -> 'd) -> 'a -> 'b),
           (g : ('a -> 'b) * ('c -> 'd) -> 'c -> 'd) ) =
  let rec fp x = f (fp,gp) x
  and     gp x = g (fp,gp) x
  in (fp,gp)

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

module MemoTabT (Ref : MONADREF) = struct
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
    let sanitize (x,(s,_)) = (x,s) in
    let upd_entry x e t = liftRef (put_ref loc (BatMap.add x e t)) in
    let msum = List.fold_left Nondet.mplus (Nondet.mzero ()) in

    return (
      get_ref loc >>= return ** List.map sanitize ** BatMap.bindings, 
      ( fun p x -> let open CCO in let open Nondet in
          liftRef (get_ref loc) >>= fun table ->
          try let (res,conts) = BatMap.find x table in
            shift (fun k -> upd_entry x (res,k::conts) table >>
                            msum (List.map k res))
          with Not_found ->
            shift (fun k -> upd_entry x ([],[k]) table >>
                            finc p x >>= fun y ->
                            liftRef (get_ref loc) >>= fun table' ->
                            let (res,conts) = BatMap.find x table' in
                            if List.mem y res then mzero ()
                            else upd_entry x (y::res,conts) table' >>
                                 msum (List.map (fun k -> k y) conts)))))
end

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

(* time execution *)
let timeit thunk =
  let time_start = Sys.time () in
  let result = thunk () in
  Printf.printf "Time: %g s" (Sys.time () -. time_start);
  print_endline "";
  result

let words s = BatString.nsplit s ~by:" "

module TestG (G: GRAMMAR) = struct
  module MM = MemoTabT (Ref)
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

module G1 (MM : MONADMEMOTABLE) = struct
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

module G2 (MM: MONADMEMOTABLE) = struct
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

module G3 (MM: MONADMEMOTABLE) = struct
	include GrammarOps (MM)

  let grammar = 
    let x = term "x" in 
    let s = fix (fun s -> x *> s *> s <|> epsilon) in
    memrec (fun sm -> x *> sm *> sm <|> epsilon) >>= fun (gsm, sm) ->
    memrec (fun sml -> sml *> sml *> x <|> epsilon) >>= fun (gsml, sml) ->
    memrec2 ((fun (smml, smml') -> smml *> smml' <|> epsilon),
             (fun (smml, smml') -> smml *> x)) >>= fun ((gsmml,_), (smml,_)) ->
    return (["s", return []; "sm",gsm; "sml",gsml; "smml",gsmml],
            function "s" -> s | "sm" -> sm | "sml" -> sml | "smml" -> smml)

  let sentence i = replicate i "x"
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

(* needs tetradic or polyadic fixed point operator!
module T2 (MM: MONADMEMOTABLE) = struct
	include GrammarOps (MM)
	
	let sentences i = map (:[]) $ "nvdn" ++ concat (replicate i "pdn")
	let grammar = 
		let t = term in 
		memrec (fun advm -> t "a" *> advm <|> t "a" <|> advm *> t "c" *> advm) >>= fun advm ->
		memrec (fun adjm ->= t "j" <|> t "j" *> adjm <|> advm *> t "j" <|> adjm *> t "c" *> adjm) >>= fun adjm ->
		memrec (fun nm -> t "n" <|> t "n" *> nm) >>= fun nm ->
		mem (t "x" *> t "v" <|> t "v") >>= fun vc ->
		mem (nm <|> adjm *> nm <|> t "d" *> nm <|> t "t" *> adjm *> nm) >>= fun np0 ->
		mem (adjm *>  np0 *> pp *> pp
						 <|> adjm *> np0 *> pp
						 <|> adjm *> np0
						 <|> np0 *> pp
						 <|> np0
						 <|> np0 *> pp *> pp) >>= fun np1 ->
		np     = np *> t "c" *> np
							<|> np1 *> t "t" *> s
							<|> np1 *> s
							<|> np1
		pp     = pp *> t "c" *> pp <|> t "p" *> np
		s      = np *> vp *> pp *> pp
						 <|> np *> vp *> pp
						 <|> np *> vp
						 <|> s *> t "c" *> s
		vp     = vc *> np <|> vp *> t "c" *> vp <|> vc

		memrec (fun dir -> dir *> t "c" *> dir
 									 <|> pp *> vp
									 <|> vp
									 <|> vp *> pp) >>= fun dir ->

		return (dir <|> np <|> s)
end
*)
module Test1 = TestG (G1)
module Test2 = TestG (G2)
