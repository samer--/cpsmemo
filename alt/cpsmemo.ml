(* open Utils *)
(* open Monads *)
module PSet = BatSet
module PMap = BatMap

module type MONADMEMO = sig
  include MONAD

  val memo : ('c -> 'a -> 'b m) -> ('c -> 'a -> 'b m) m
end

module MemoOps (M : MONADMEMO) = struct
  include MonadOps(M)

  (** Memoise non-recursive function. *)
  let mem f = M.memo (fun () x -> f x) >>= fun mf -> M.return (mf ())

  (** Memoise open recursive function on two arguments *) 
  let memo2 f = M.memo (fun p (x,y) -> f p x y) >>= fun g ->
                M.return (fun p x y -> g (x,y))

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
  module M = BatMap.IntMap

  type t =  int * Dynamic.t M.t
  type 'a loc = int * ('a -> Dynamic.t) * (Dynamic.t -> 'a)

  let empty = (0,M.empty)
  let get (j,_,outd) (_,m)     = outd (M.find j m) 
  let put (j,ind,_) x (i,m)    = (i,M.add j (ind x) m)
  let upd (j,ind,outd) f (i,m) = (i,M.modify j (ind ** f ** outd) m)
  let new_loc (i,m) = let (ind,outd) = Dynamic.newdyn () in 
                      ((i,ind,outd),(i+1,m))
end

module ST = struct
  include StateM(Store)
  type 'a ref = 'a Store.loc

  let put_ref loc x  = upd (Store.put loc x)
  let upd_ref loc f  = upd (Store.upd loc f)
  let get_ref loc    = bind get (return ** Store.get loc)
  let new_ref x = bind Store.new_loc (fun loc ->
                  bind (put_ref loc x) (fun _ -> return loc))
  let run m = fst (m Store.empty)
end

module MemoT (ST : MONADST) : sig
  type 'a m

  val memo  : ('a -> 'b -> 'c m) -> ('a -> 'b -> 'c m) m
  val return : 'a -> 'a m
  val bind   : 'a m -> ('a -> 'b m) -> 'b m
  val mzero  : unit -> 'a m
  val mplus  : 'a m -> 'a m -> 'a m
  val run    : 'a m -> 'a list ST.m

end = struct
  module ND = ListT (ST)
  module CC = ContT (Dynamic) (ND)
  module CO = MonadOps (CC)
  include CC

  let run m =
    let (ind,outd) = Dynamic.newdyn () in
    ND.bind (m.run (ND.return ** ind)) (ND.return ** outd)

  let liftST m  = lift (ND.lift m)
  let mzero ()  = {run = fun k -> ND.mzero ()}
  let mplus f g = {run = fun k -> ND.mplus (f.run k) (g.run k)}
  let msum      = List.fold_left mplus (mzero ())

  let memo finc = let open CO in
    liftST (ST.new_ref PMap.empty) >>= (fun loc ->
    let upd_entry x e t = liftST (ST.put_ref loc (PMap.add x e t)) in
    return (fun p x ->
      liftST (ST.get_ref loc) >>= fun table ->
      try let (res,conts) = PMap.find x table in
        shift (fun k -> upd_entry x (res,k::conts) table >>
                        msum (List.map k res))
      with Not_found ->
        shift (fun k -> upd_entry x ([],[k]) table >>
                        finc p x >>= fun y ->
                        liftST (ST.get_ref loc) >>= fun table' ->
                        let (res,conts) = PMap.find x table' in
                        if List.mem y res then mzero ()
                        else upd_entry x (y::res,conts) table' >>
                             msum (List.map (fun k -> k y) conts))))
end


module type MONADMEMOTABLE = sig
  include MONAD
  module Nondet : MONADPLUS

  type ('a,'b) table
  val reify : 'a Nondet.m -> 'a list m
  val memo  : ('c -> 'a -> 'b Nondet.m) -> 
              (('c -> 'a -> 'b Nondet.m) * ('a,'b) table m) m
end

module MemoTabOps (M : MONADMEMOTABLE) = struct
  module MO = MonadOps (M)
  open M
  open MO

  (** Memoise open recursive function on two arguments *) 
  let memo2 f = memo (fun p (x,y) -> f p x y) >>= fun (g,gettab) ->
                return ((fun p x y -> g (x,y)),gettab)

  let memtab f = memo (fun () x -> f x) >>= fun (mf,gt) -> return (mf (),gt)
  let memtabrec f = memo f >>= fun (mf,g) -> return (fix mf,g)
  let memtabrec2 (f,g) = memo f >>= fun (mf,gf) ->
                         memo g >>= fun (mg,gg) ->
                         let (fp,gp) = fix2 (mf,mg) in
                         return ((fp,gp),(gf,gg))

  let mem f = liftM fst (memtab f)
  let memrec f = liftM fst (memtabrec f)
  let memrec2 (f,g) = liftM fst (memtabrec2 (f,g))
end

module MemoTabT (ST : MONADST) = struct
  module ND = ListT (ST)
  include ST

  type ('a,'b) table = ('a * 'b list) list

  module Nondet = struct
    include ContT (Dynamic) (ND)
    let mzero ()  = {run = fun k -> ND.mzero ()}
    let mplus f g = {run = fun k -> ND.mplus (f.run k) (g.run k)}
    let reflect l = {run = fun k -> ND.bind l k}
  end

  module STO = MonadOps (ST)
  module CCO = MonadOps (Nondet)

  let reify (m : 'a Nondet.m) : 'a list ST.m =
    let (ind,outd) = Dynamic.newdyn () in
    ND.bind (m.Nondet.run (ND.return ** ind)) (ND.return ** outd)

  (* multiple results stored in a set *)
  let memo finc = let open STO in 
    let sanitize (x,(res,_)) = (x,PSet.fold cons res []) in
    let getter loc = get_ref loc >>= return ** List.map sanitize ** PMap.bindings in
    let mempty = Nondet.mzero () in
    let memcall loc p x = let open CCO in let open Nondet in  
      let liftST m  = Nondet.lift (ND.lift m) in
      let upd_entry x e t = liftST (put_ref loc (PMap.add x e t)) in

      liftST (get_ref loc) >>= fun table ->
      try let (res,conts) = PMap.find x table in
        shift (fun k -> upd_entry x (res,k::conts) table >> 
                        PSet.fold (mplus ** k) res mempty)
      with Not_found ->
        shift (fun k -> upd_entry x (PSet.empty,[k]) table >>
                        finc p x >>= fun y ->
                        liftST (get_ref loc) >>= fun table' ->
                        let (res,conts) = PMap.find x table' in
                        if PSet.mem y res then mempty 
                        else upd_entry x (PSet.add y res,conts) table' >>
                             List.fold_left (fun s k -> mplus (k y) s) 
                                            mempty conts) in

    new_ref PMap.empty >>= fun loc -> return (memcall loc, getter loc)

  let memo' (label,finc) = let open STO in let open Pair in
    let delay x y = (label,y,x) in

    let sanitize = List.map (($>) snd) ** PMap.bindings ** fst in
    let getter loc = get_ref loc >>= return ** List.map (($>) sanitize) ** PMap.bindings in 
    let mempty = Nondet.mzero () in
    let memcall loc p x = let open CCO in let open Nondet in  
      let liftST m  = Nondet.lift (ND.lift m) in
      let update x e t = liftST (put_ref loc (PMap.add x e t)) in

      liftST (get_ref loc) >>= fun table ->
      try let (results, conts) = PMap.find x table in
        shift (fun k -> 
          update x (results, k::conts) table >>
          PMap.foldi (fun y (ret,_) s -> mplus (k (ret,y)) s) results mempty)
      with Not_found ->
        shift (fun k -> 
          update x (PMap.empty,[k]) table >>
          finc p x >>= fun (ylazy,ystrict) ->
          liftST (get_ref loc) >>= fun table' ->
          let (results, conts) = PMap.find x table' in
          try let (ret, vals) = PMap.find ystrict results in
              if List.mem ylazy vals then mempty
              else update x (PMap.add ystrict (ret, ylazy::vals) results, conts) table' >>
                   mempty 
          with Not_found ->
            let ret = (label,x,ystrict) in
            update x (PMap.add ystrict (ret, [ylazy]) results, conts) table' >>
            List.fold_left (fun s k -> mplus (k (ret,ystrict)) s) mempty conts
        )

      in new_ref PMap.empty >>= fun loc -> return (memcall loc, getter loc)
end

module type MEMORESULTS = sig
  module In : ORD
  module Out : TYPE
  module Ret : TYPE

  type t
  val empty : t
  val fold : (Ret.t -> 'a -> 'a) -> t -> 'a -> 'a
  val add : In.t -> Out.t -> t -> Ret.t option * t option
end

module ResultSet (In : ORD) (Out : ORD) = struct
  module In = In
  module Out = Out
  module Ret = Out
  include Set.Make (Out)

  let add _ y rs = if mem y rs then (None, None)
                   else (Some y, Some (add y rs))
end

module MemoTabTF (R : MEMORESULTS) (ST : MONADST) : sig
  module Nondet : MONADPLUS

  val memo : ('a -> R.In.t -> R.Out.t Nondet.m) ->
             ( ('a -> R.In.t -> R.Ret.t Nondet.m)
               * (R.In.t * R.t) list ST.m) ST.m

  val reify : 'a Nondet.m -> 'a list ST.m
  val reflect : 'a list ST.m -> 'a Nondet.m
end= struct
  module ND = ListT (ST)
  module Map = Map.Make (R.In)
  include ST

  module Nondet = struct
    include ContT (Dynamic) (ND)
    let mzero ()  = {run = fun k -> ND.mzero ()}
    let mplus f g = {run = fun k -> ND.mplus (f.run k) (g.run k)}
    let reflect l = {run = fun k -> ND.bind l k}
  end

  module STO = MonadOps (ST)
  module CCO = MonadOps (Nondet)

  let reflect = Nondet.reflect
  let reify (m : 'a Nondet.m) : 'a list ST.m =
    let (ind,outd) = Dynamic.newdyn () in
    ND.bind (m.Nondet.run (ND.return ** ind)) (ND.return ** outd)

  let memo finc = let open STO in 
    let open Pair in
    let getter loc = get_ref loc >>= return ** List.map (($>) fst) ** Map.bindings in 
    let mempty = Nondet.mzero () in
    let memcall loc p x = let open CCO in let open Nondet in  
      let liftST m  = Nondet.lift (ND.lift m) in
      let upd_entry x e t = liftST (put_ref loc (Map.add x e t)) in

      liftST (get_ref loc) >>= fun table ->
      try let (results,conts) = Map.find x table in
        shift (fun k -> upd_entry x (results,k::conts) table >>
                        R.fold (mplus **  k) results mempty)
      with Not_found ->
        shift (fun k -> upd_entry x (R.empty,[k]) table >>
                        finc p x >>= fun y ->
                        liftST (get_ref loc) >>= fun table' ->
                        let (results,conts) = Map.find x table' in
                        let (optval,optres) = R.add x y results in
                        (match optres with 
                         | Some results' -> upd_entry x (results',conts) table'
                         | None -> return ()) >> 
                        (match optval with 
                         | Some y' -> List.fold_left (fun s k -> mplus (k y') s) mempty conts
                         | None -> mempty))

      in new_ref Map.empty >>= fun loc -> return (memcall loc, getter loc)
end
