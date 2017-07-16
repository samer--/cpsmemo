(* cpsmemo - Nondeterministic, left recursive memoisations using delimited continuations
 * (c) Samer Abdallah, 2017
 *)

open Delimcc

(* -- utilities -- *)
let id x = x               (* identity function *)
let ( ** ) f g x = f (g x)  (* function composition *)
let cons x xs = x :: xs
let flip f x y = f y x
let rec iota = function | 0 -> [] | n -> iota (n-1) @ [n-1]

type 'a thunk = unit -> 'a

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

(* -- Memoisation with access to memo tables ------- *)

module type ALT = sig
  val alt : 'a -> 'a -> 'a
  val fail : unit -> 'a
end

module type MEMOTABLE = sig
  module Nondet : ALT

  type ('a,'b) table = ('a * 'b list) list
  val run : ('a thunk) -> 'a list
  val memo : ('c -> 'a -> 'b) -> ((('a,'b) table thunk) * ('c -> 'a -> 'b))
end

module MemoTabOps (M : MEMOTABLE) = struct
  open M

  let mem f = let gt, mf = memo (fun () -> f) in (gt,mf ())
  let memrec f = let g, mf = memo f in (g,fix mf)
  let memrec2 (f,g) = let get_f, mf = memo f in
                      let get_g, mg = memo g in
                      let (fp,gp) = fix2 (mf,mg) in
                      ((get_f,get_g),(fp,gp))
end

module MemoTabT = struct
  let pr = new_prompt ()

  module Nondet = struct
    let fail () = abort pr []
    let alt x y = shift0 pr (fun k -> k x @ k y)
  end

  type ('a,'b) table = ('a * 'b list) list


  let run f =
    let (ind,outd) = Dynamic.newdyn () in
    List.map outd (push_prompt pr (fun () -> [ind (f ())]))

  let memo fop = 
    let loc = ref BatMap.empty in
    let sanitize (x,(s,_)) = (x, BatSet.fold cons s []) in
    let upd_entry x e t = loc := BatMap.add x e t in

    ( (fun () -> List.map sanitize (BatMap.bindings (!loc))),
      (fun p x -> let open Nondet in
        let table = !loc in 
        try let (res,conts) = BatMap.find x table in
          shift0 pr (fun k -> upd_entry x (res,k::conts) table; 
                              BatSet.fold (List.append ** k) res [])
        with Not_found ->
          shift pr (fun k -> upd_entry x (BatSet.empty, [k]) table;
                             let y = fop p x in
                             let table' = !loc in
                             let (res,conts) = BatMap.find x table' in
                             if BatSet.mem y res then fail ()
                             else upd_entry x (BatSet.add y res,conts) table';
                                  List.fold_right (fun k -> List.append (k y)) conts [])))
end


(* -- Parser interface build on any MONADPLUS --------*)

module Parser (M : ALT) = struct
  let ( *> ) f g xs = g (f xs)
  let ( <|> ) f g xs = (M.alt f g) xs (* or M.alt (f xs) (g xs) *)
  let epsilon = id 
  let term x = function | y::ys when x=y -> ys
                        | _ -> M.fail ()
end

module type GRAMMAR = functor (MM : MEMOTABLE) -> sig 
  val grammar : ((bytes * (bytes list, bytes list) MM.table thunk) list *
                 (bytes -> bytes list -> bytes list)) thunk 
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

module MM = MemoTabT

module TestG (G: GRAMMAR) = struct
  module GM = G (MM)
  include Parser (MM.Nondet)
  include MemoTabOps (MM)
  open MM

  let success = function | [] -> true | _ -> false


  let get_table (label, getter) = (label, getter ())

  let sent i = GM.sentence i
  let parse start input = 
    let getters, get_nonterm  = GM.grammar () in
    let results = run (fun () -> ((get_nonterm start) input)) in
    (results, List.map get_table getters)

  let profile start input = timeit (fun () -> fst (parse start input))
end

module GrammarOps (MM: MEMOTABLE) = struct
  include Parser (MM.Nondet)
  include MemoTabOps (MM)
  include MM
end

module Johnson (MM : MEMOTABLE) = struct
	include GrammarOps (MM)

  let success = function | [] -> true | _ -> false
  let grammar () = 
    let v   = term "likes" <|> term "knows" in
    let pn  = term "Kim" <|> term "Sandy" in 
    let det = term "every" <|> term "no" in
    let n   = term "student" <|> term "professor" in
    let get_np, np = memrec (fun np -> det *> n <|> pn <|> np *> term "'s" *> n) in 
    let (get_vp,get_s), (vp,s) = memrec2 ((fun (vp,s) -> v *> np <|> v *> s),
                                          (fun (vp,s) -> np *> vp)) in
    ([("s",get_s); ("np",get_np); ("vp",get_vp)], 
      function | "s" -> s
               | "vp" -> vp)

  let sentence = List.nth [words "every student likes Kim 's professor"]
end

(*
module Frost1 (MM: MEMOTABLE) = struct
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
*)

let replicate n x = let rec r = function 0 -> [] | n -> x::r (n-1) in r n

module FrostAmbig (MM: MEMOTABLE) = struct
	include GrammarOps (MM)

  let grammar () = 
    let a = term "a" in 
    let s = fix (fun s -> a *> s *> s <|> epsilon) in
    let gsm, sm   = memrec (fun sm -> a *> sm *> sm <|> epsilon) in
    let gsml, sml = memrec (fun sml -> sml *> sml *> a <|> epsilon) in
    let (gsmml,_), (smml,_) = memrec2 ((fun (smml, smml') -> smml *> smml' <|> epsilon),
                                       (fun (smml, smml') -> smml *> a)) in
    (["s", (fun () -> []); "sm",gsm; "sml",gsml; "smml",gsmml],
     function "s" -> s | "sm" -> sm | "sml" -> sml | "smml" -> smml)

  let sentence i = replicate i "a"
end

(*
module T1 (MM: MEMOTABLE) = struct
	include GrammarOps (MM)
	
	let sentences i = (words "n v d n") @ List.concat (replicate i (words "p d n"))
	let grammar = 
	  memrec2 ((fun (np,pp) -> term "n" <|> term "d" *> term "n" <|> np *> pp),
             (fun (np,pp) -> term "p" *> np)) >>= fun ((gnp,gpp), (np,pp)) ->
		memrec (fun vp -> term "v" *> np <|> vp *> pp) >>= fun (gvp, vp) ->
		mem (np *> vp) >>= fun (gs, s) ->
		return (["s",gs], s)
end
*)
module Tomita2 (MM: MEMOTABLE) = struct
	include GrammarOps (MM)
	
  (* polyadic memoising fixed point *)
  let memrec_list fops =
    let (getters, mfops) = List.split (List.map memo fops) in
    (List.combine getters (fixlist mfops))

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

  let grammar () = 
    let gs, s = List.nth (memrec_list rules) 11 in
    (["s",gs], fun "s" -> s)
end

let main args = 
  let module Test = TestG (FrostAmbig) in
  let n = int_of_string args.(2) in
  let gr = args.(1) in
  let _ = Test.(profile gr (sent n)) in
  ()

let _ = if not !Sys.interactive then main Sys.argv else ()
