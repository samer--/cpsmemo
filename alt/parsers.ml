module Fibonacci (M : MONAD) = struct
  include MonadOps (M)
  let fib f = function | 0 -> M.return 0 | 1 -> M.return 2
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
  module MM = MemoT (ST)
  module FF = Fibonacci (MM)
  module TC = TransClose (MM)
  module MO = MemoOps (MM)
  open MO

  let test_fib n = ST.run (MM.run (memrec FF.fib >>= fun fib -> fib n))
  let test_path x = ST.run (MM.run (memrec TC.path>>= fun path -> path x))
end


module Parser (T : TYPE) (M : MONADPLUS) = struct
  type 's t = 's -> 's M.m

  let ( *> ) (f: 's t) (g:'s t) : 's t = fun xs -> M.bind (f xs) g
  let ( <|> ) (f: 's t) (g:'s t) : 's t = fun xs -> M.mplus (f xs) (g xs)
  let fail    : 's t = fun _ -> M.mzero ()
  let epsilon : 's t = M.return

  let term_of_list x  : T.t list t = 
    function | y::ys when x=y -> M.return ys
             | _ -> M.mzero ()

  let term_of_pos input x  : int t = 
    let arr = Array.of_list input in 
    let max = Array.length arr in 
    fun i -> if i<max && arr.(i)=x then M.return (succ i) 
             else M.mzero ()
end

module ParserM (T : TYPE) (M : MONADPLUS) = struct
  type state = int
  type 'a m = state -> ('a * state) M.m

  let bind f k = fun s -> M.bind (f s) (fun (x,s') -> k x s')
  let return x = fun s -> M.return (x,s)
  let mplus f g = fun s -> M.mplus (f s) (g s)
  let mzero ()  = fun _ -> M.mzero ()

  let transition (f : 'a m) = 
    fun s -> M.bind (f s) (fun (x,s') -> M.return (((s,s'),x),s'))

  let eos input : unit m =
    let len = List.length input in 
    fun i -> if i<len then M.return ((),i) else M.mzero ()

  let term input : T.t -> unit m = 
    let arr = Array.of_list input in 
    let len = Array.length arr in 
    fun x i -> if i<len && arr.(i)=x then M.return ((),succ i) 
               else M.mzero ()
end

module MemoGrammarOps (M : MONAD) = struct
  include MonadOps (M)

  (* let mkmemo lab t = Memo (lab, List.map (fun (x,ys) -> (x,List.map Pair.swap ys)) t) *)
  let get_tables gts = mapM (fun (lab,gt) -> liftM (Pair.pair lab) gt) gts

  let fixpoly_with memo rules =
     let (labels,funs) = List.split rules in 
     mapM memo funs >>= fun memos_getters ->
     let (memos,getters) = List.split memos_getters in
     let (start::_)= fixlist memos in
     return (start, get_tables (List.combine labels getters)) 

  let fixpoly_with_labels memo rules =
     let (labels,funs) = List.split rules in 
     mapM memo rules >>= fun memos_getters ->
     let (memos,getters) = List.split memos_getters in
     let (start::_)= fixlist memos in
     return (start, get_tables (List.combine labels getters)) 

  let run_with_tables prog run = 
    prog      >>= fun (start, getter) ->
    run start >>= fun results ->
    getter    >>= fun tables ->
    return (List.length results, tables)
end

module MonadUtils (M : MONAD) = struct
  let traceM fmtin fmtout name f comp x =
    let module MO = MonadOps (M) in 
    let open MO in
    Printf.printf "Calling   %s %s\n" name (fmtin x);
    f comp x >>= fun y ->
    Printf.printf "Returning         %s :  %s --> %s\n" name (fmtin x) (fmtout y); 
    return y
end
type word = string

module Test2 = struct
  module MM = MemoTabT (ST)
  module PP = Parser (struct type t=word end) (MM.Nondet)
  include MemoTabOps (MM)
  include MemoGrammarOps (MM)
  include PP
  open MM

  let ret s = return (s, return ())

  module AU72 = struct
    let s t = ret (fix (fun s -> t "a" *> s *> s <|> epsilon))
    let sm t = memtabrec (fun s -> t "a" *> s *> s <|> epsilon)
    let sml t = memtabrec (fun s -> s *> s *> t "a" <|> epsilon)
    let smml t = fixpoly_with memo 
      [ "s",  (fun [s;sa] -> s *> sa <|> epsilon )
      ; "sa", (fun [s;_]  -> s *> t "a")
      ]
  end

  let tomita1985 t = 
    let det  = t "a" <|> t "the" in
    let n    = t "I" <|> t "man" <|> t "park" <|> t "bat" in
    let prep = t "in" <|> t "with" in
    let v    = t "saw" in
    memtabrec2 ( 
      (fun (np,pp) -> n <|> det *> n <|> np *> pp),
      (fun (np,pp) -> prep *> np)           ) >>= fun ((np,pp),(gnp,gpp)) ->
    mem (v *> np) >>= fun vp ->
    memtabrec (fun s -> np *> vp <|> s *> pp) >>= fun (s,gs) ->
    ret s 

  let tomita1985' t = fixpoly_with memo 
    [ "s",    (fun [s;np;pp;vp;det;n;v;prep] -> np *> vp <|> s *> pp       )
    ; "np",   (fun [s;np;pp;vp;det;n;v;prep] -> n <|> det *> n <|> np *> pp)
    ; "pp",   (fun [s;np;pp;vp;det;n;v;prep] -> prep *> np                 )           
    ; "vp",   (fun [s;np;pp;vp;det;n;v;prep] -> v *> np                    )
    ; "det",  (fun [s;np;pp;vp;det;n;v;prep] -> t "a" <|> t "the" )
    ; "n",    (fun [s;np;pp;vp;det;n;v;prep] -> t "I" <|> t "man" <|> t "park" <|> t "bat" )
    ; "v",    (fun [s;np;pp;vp;det;n;v;prep] -> t "saw" )
    ; "prep", (fun [s;np;pp;vp;det;n;v;prep] -> t "in" <|> t "with"         )
    ]

  let johnson1995 t = fixpoly_with memo 
    [ "s",   (fun [s;v;pn;det;n;np;vp] -> np *> vp                     )
    ; "v",   (fun [s;v;pn;det;n;np;vp] -> t "likes" <|> t "knows"      )
    ; "pn",  (fun [s;v;pn;det;n;np;vp] -> t "Kim" <|> t "Sandy"        )
    ; "det", (fun [s;v;pn;det;n;np;vp] -> t "every" <|> t "no"         )
    ; "n",   (fun [s;v;pn;det;n;np;vp] -> t "student" <|> t "professor")
    ; "np",  (fun [s;v;pn;det;n;np;vp] -> pn <|> np *> n <|> det *> n  )
    ; "vp",  (fun [s;v;pn;det;n;np;vp] -> v *> np <|> v *> s           )
    ] 

  let frostHafiz2006 t = 
    mem (t "boy" <|> t "girl" <|> t "man" <|> t "woman") >>= fun n ->
    mem (t "knows" <|> t "respects" <|> t "loves") >>= fun v ->
    mem (t "helen" <|> t "john" <|> t "pat"      ) >>= fun pn ->
    mem (t "and" <|> t "or"                      ) >>= fun conj ->
    mem (t "every" <|> t "some"                  ) >>= fun det ->
    mem (det *> n                                ) >>= fun dp ->
    mem (pn <|> dp                               ) >>= fun np' ->
    memrec (fun np -> np' <|> np *> conj *> np   ) >>= fun np ->
    memrec (fun vp -> v <|> vp *> conj *> vp     ) >>= fun vp ->
    mem (np *> vp *> np) >>= fun s ->
    ret s

  let frostHafiz2006' t = fixpoly_with memo
    [ "s",   (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> np *> vp *> np) 
    ; "vp",  (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> v <|> vp *> conj *> vp   )
    ; "np",  (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> np' <|> np *> conj *> np )
    ; "np'", (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> pn <|> dp                )
    ; "dp",  (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> det *> n                 )
    ; "det", (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> t "every" <|> t "some"   )
    ; "conj",(fun [s;vp;np;np';dp;det;conj;pn;v;n] -> t "and" <|> t "or"       )
    ; "pn",  (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> t "helen" <|> t "john" <|> t "pat")
    ; "v",   (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> t "knows" <|> t "respects" <|> t "loves")
    ; "n",   (fun [s;vp;np;np';dp;det;conj;pn;v;n] -> t "boy" <|> t "girl" <|> t "man" <|> t "woman")
    ]

  let many_a n = BatList.make n "a" 
  let s1 = "every boy or some girl and helen and john or pat " 
  let s2 = s1^"knows and respects or loves every boy or some girl and pat or john and helen"

  let parse grammar input = ST.run (run_with_tables (grammar (term_of_pos input)) (reify ** (|>) 0))
end

module type APPLICATIVE = sig
  type 'a t 

  val ( *: ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> )  : 'a t -> 'b t -> 'b t
  val ( *< )  : 'a t -> 'b t -> 'a t
  val ( %: )  : ('a -> 'b) -> 'a t -> 'b t
  val pure    : 'a -> 'a t
end

module ParserA (T : TYPE) (M : MONADPLUS) : sig
  type 'a t = 'a ParserM (T) (M).m

  val ( *: ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> )  : 'a t -> 'b t -> 'b t
  val ( *< )  : 'a t -> 'b t -> 'a t
  val ( %: )  : ('a -> 'b) -> 'a t -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val fail    : unit -> 'a t
  val epsilon : unit t
  val pure    : 'a -> 'a t
  val term    : T.t list -> T.t -> unit t
  val eos     : T.t list -> unit t
  val trans   : 'a t -> ((int * int) * 'a) t

end = struct
  module P = ParserM (T) (M)
  open P

  type 'a t = 'a P.m

  let ( *: ) f g = P.bind f (fun ff -> P.bind g (fun gg -> return (ff gg)))
  let ( *> ) f g  = P.bind f (fun _ -> g) 
  let ( *< ) f g  = P.bind f (fun ff -> P.bind g (fun _ -> return ff)) 
  let ( %: ) ff g = P.bind g (fun gg -> return (ff gg))
  let ( <|> ) = P.mplus
  let fail = mzero 
  let term = P.term 
  let epsilon = P.return ()
  let pure x = P.return x
  let eos = P.eos
  let trans p = P.transition p
end

module Test3 = struct
  module MM = MemoTabT (ST)
  module PP = ParserA (struct type t=word end) (MM.Nondet)
  include MemoGrammarOps (MM)
  include MonadUtils (MM.Nondet)
  open PP
  open MM
  open Printf 

  type 'l span = 'l * int * int 
  type 'l tree = Leaf of word 
               | Branch of 'l span list 

  let termT t x = t x *> pure (Leaf x) 
  let consT h t = cons %: h *: t
  let branch x = Branch x
  let nilT () = pure []
  let tagT (l,f) = (l, fun g -> branch %: f g)

  let trace_rule fmtlab (l,f) = 
    let lstr = fmtlab l in
    (l, traceM string_of_int (fun (_,p) -> sprintf "(%s, ...)" (string_of_int p)) lstr f)

  let parse rules input = ST.run (run_with_tables 
    (input |> term |> termT |> rules |> fixpoly_with_labels memo') (reify ** (|>) 0))

  type tag = S | NP | VP | PP | Det | N | V | Prep | Term of word | Empty
  let fmttag = function S -> "Sent"   | NP -> "NP  " | VP -> "VP  " | PP -> "PP  " 
                      | Det -> "Det " | N -> "Noun"  | V -> "Verb"  | Prep -> "Prep" 
                      | Term w -> "Term:"^w | Empty -> "Empty"

  let make_grammar = List.map (tagT) 

  let tomita1985 t = 
    let (^) = consT in
    let e = nilT () in
    let t w = (fun ((i,j),x) -> (Term w,i,j)) %: trans (t w) in

    make_grammar 
      [ S,    (fun [s;np;pp;vp;det;n;prep;v] -> np^vp^e <|> s^pp^e 
     ); NP,   (fun [s;np;pp;vp;det;n;prep;v] -> n^e <|> det^n^e <|> np^pp^e
     ); PP,   (fun [s;np;pp;vp;det;n;prep;v] -> prep^np^e
     ); VP,   (fun [s;np;pp;vp;det;n;prep;v] -> vp^pp^e <|> v^np^e
     ); Det,  (fun [s;np;pp;vp;det;n;prep;v] -> (t "a" <|> t "the")^e
     ); N,    (fun [s;np;pp;vp;det;n;prep;v] -> (t "I" <|> t "man" <|> t "park" <|> t "bat")^e
     ); Prep, (fun [s;np;pp;vp;det;n;prep;v] -> (t "in" <|> t "with")^e
     ); V,    (fun [s;np;pp;vp;det;n;prep;v] -> t "saw" ^ e
     )]


  let sml t =
    let (^) = consT in
    let e = nilT () in
    let t w = (fun ((i,j),x) -> (Term w,i,j)) %: trans (t w) in
    let epsilonT = (fun ((i,j),_) -> (Empty,i,j)) %: trans epsilon in

    make_grammar [ S,  (fun [s] -> s^s^t "a"^e <|> epsilonT^e ) ]
end


module Int = struct type t = int let compare = (-) end

module Test4 = struct
  type tag = S | NP | VP | PP | Det | N | V | Prep 

  let fmttag = function S -> "Sent"   | NP -> "NP  " | VP -> "VP  " | PP -> "PP  " 
                      | Det -> "Det " | N -> "Noun"  | V -> "Verb"  | Prep -> "Prep" 

  (* results collection with same type playing role of parse tree
   * and link to stored parse trees *)
  module ParseResults2 = struct
    type 'l tree = Leaf of word 
                 | Branch of 'l * 'l tree list 
                 | Link of ('l * int * int)

    module In = Int
    module Out = struct type t = tag tree * int end
    module Ret = Out
    module Map = Map.Make (Int)
    
    type t = (tag tree * tag tree list) Map.t
    let fold f = Map.fold (fun y (ret,_) a -> f (ret,y) a)
    let empty = Map.empty
    let add x (tree,y) results =
      try let (ret,vals) = Map.find y results in
          if List.mem tree vals then (None,None)
          else (None, Some (Map.add y (ret,tree::vals) results))
      with Not_found ->
        let ret = match tree with
                  | Leaf w -> Leaf w
                  | Branch (l,ts) -> Link (l,x,y) 
                  | _ -> tree in
        (Some (ret,y), Some (Map.add y (ret,[tree]) results))
    let sanitize = List.map (Pair.($>) snd) ** Map.bindings
  end

  module ParseResults = struct
    type tree = Leaf of word 
              | Branch of tree list 
              | Link of (tag * int * int)
              | Empty

    let leaf x = Leaf x
    let branch = Pair.pair

    module In = Int
    module Out = struct type t = (tag * tree list) * int end
    module Ret = struct type t = tree * int end
    module IMap = Map.Make (Int)
    
    type t = (tree * tree list) IMap.t
    let fold f = IMap.fold (fun y (ret,_) a -> f (ret,y) a)
    let empty = IMap.empty

    let add x ((l,ts),y) results =
      let v = match ts with [] -> Empty | [t] -> t | _ -> Branch ts in
      try let (ret,vals) = IMap.find y results in
          (* duplicates seem not to happen here, I don't know why *)
          (* if List.mem v vals then (None,None) else *) 
          (None, Some (IMap.add y (ret,v::vals) results))
      with Not_found ->
        let ret = Link (l,x,y) 
        in (Some (ret,y), Some (IMap.add y (ret,[v]) results))

    let sanitize mm = List.map (Pair.($>) snd) (IMap.bindings mm)
  end

  open ParseResults
  module MM = MemoTabTF (ParseResults) (ST)
  module PP = ParserA (struct type t=word end) (MM.Nondet)
  open MM
  open PP

  include MonadUtils (Nondet)
  include MemoGrammarOps (ST)

  let tagT (l,f) = let b = branch l in (l, fun g -> b %: f g)
  let termT t x = t x *> pure (leaf x) 

  let trace_rule (l,f) = 
    let lstr = fmttag l in
    (l, traceM string_of_int (fun (_,p) -> Printf.sprintf "(%s, ...)" (string_of_int p)) lstr f)

  let parse rules input = 
    let open Pair in
    let memo' = liftM (($>) (liftM (List.map (($>) sanitize)))) ** memo in
    ST.run (run_with_tables (input |> term |> termT |> rules |> List.map tagT 
                                   |> fixpoly_with memo') (reify ** (|>) 0))

  (* let tomita1985 (t : word -> (word,tag) tparser) : (word,tag) grammar = *) 
  (* let tomita1985 t = *) 
  (*   let t x = t x *> pure (Leaf x) in *)
  (*   let tag l ps = branch l %: (List.fold_right consT ps (nilT ())) in *)
  (*   let pass p = let pp x = Pass x in pp %: p in *)
  (*   let tt x = t x *> pure (L x) in *)

  (*   List.map trace_rule *)  
  (*   [ S,    (fun [s;np;pp;vp;det;n;prep;v;cn] -> tag S [np;vp] <|> tag S [s;pp] *) 
  (*   ); NP,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> pass n <|> tag NP [det;n] <|> tag NP [np;pp] *)
  (*   ); PP,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> tag PP [prep;np] *)
  (*   ); VP,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> tag VP [vp;pp] <|> tag VP [v;np] *)
  (*   ); Det,  (fun [s;np;pp;vp;det;n;prep;v;cn] -> t "a" <|> t "the" *)
  (*   ); N,    (fun [s;np;pp;vp;det;n;prep;v;cn] -> pass cn <|> t "I" <|> t "man" <|> t "park" <|> t "bat" *)
  (*   ); Prep, (fun [s;np;pp;vp;det;n;prep;v;cn] -> t "in" <|> t "with" *)
  (*   ); V,    (fun [s;np;pp;vp;det;n;prep;v;cn] -> t "saw" *)
  (*   ); CN,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> tag CN [tt "chaise";tt "long"] *)
  (*                                             <|> tag CN [tt "cricket";tt "bat"] *) 
  (*    )] *)

  let tomita1985 t = 
    let (^) h t = cons %: h *: t in
    let e = pure [] in

     (* [ S,    (fun [s;np;pp;vp;det;n;prep;v;cn] -> np^vp^e <|> s^pp^e *) 
    (* ); NP,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> n^e <|> det^n^e <|> np^pp^e *)
    (* ); PP,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> prep^np^e *)
    (* ); VP,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> vp^pp^e <|> v^np^e *)
    (* ); Det,  (fun [s;np;pp;vp;det;n;prep;v;cn] -> (t "a" <|> t "the")^e *)
    (* ); N,    (fun [s;np;pp;vp;det;n;prep;v;cn] -> (cn <|> t "I" <|> t "man" <|> t "park" <|> t "bat")^e *)
    (* ); Prep, (fun [s;np;pp;vp;det;n;prep;v;cn] -> (t "in" <|> t "with")^e *)
    (* ); V,    (fun [s;np;pp;vp;det;n;prep;v;cn] -> t "saw"^e *)
    (* ); CN,   (fun [s;np;pp;vp;det;n;prep;v;cn] -> t "chaise"^t "long"^e <|> t "cricket"^t "bat"^e *)
    (* )] *)
     [ S,    (fun [s;np;pp;vp;det;n;prep;v] -> np^vp^e <|> s^pp^e 
    ); NP,   (fun [s;np;pp;vp;det;n;prep;v] -> n^e <|> det^n^e <|> np^pp^e
    ); PP,   (fun [s;np;pp;vp;det;n;prep;v] -> prep^np^e
    ); VP,   (fun [s;np;pp;vp;det;n;prep;v] -> vp^pp^e <|> v^np^e
    ); Det,  (fun [s;np;pp;vp;det;n;prep;v] -> (t "a" <|> t "the")^e
    ); N,    (fun [s;np;pp;vp;det;n;prep;v] -> (t "I" <|> t "man" <|> t "park" <|> t "bat")^e
    ); Prep, (fun [s;np;pp;vp;det;n;prep;v] -> (t "in" <|> t "with")^e
    ); V,    (fun [s;np;pp;vp;det;n;prep;v] -> t "saw"^e
    )]

  let sm t = 
    let (^) h t = cons %: h *: t in
    let e = pure [] in
    [ S, (fun [s] -> t "a"^s^s^e <|> e )]

  let sml t = 
    let (^) h t = cons %: h *: t in
    let e = pure [] in
    [ S, (fun [s] -> s^s^t "a"^e <|> e )]

  let many_a n = BatList.make n "a" 
end

open Test4
