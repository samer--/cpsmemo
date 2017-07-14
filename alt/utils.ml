let id x = x               (* identity function *)
let ( ** ) f g x = f (g x)  (* function composition *)
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y
let cons x y = x::y

let time1 thunk =
  let time_start = Sys.time () in
  let r = thunk () in
  let time_stop = Sys.time () in
  (time_stop -. time_start, r)

let rec timen n thunk =
  if n=0 then 0.0
  else let (t,_) = time1 thunk in
       t +. timen (pred n) thunk

let timeit n thunk =
  Printf.printf "Doing: %d iterations...\n" n;
  let tn = timen n thunk in
  let t = tn /. float_of_int n in
  Printf.printf "Time per iterations = %g s\n" t;
  t
 
let fix f = (let rec fp x = f fp x in fp)
let fix2 ( (f : ('a -> 'b) * ('c -> 'd) -> 'a -> 'b),
           (g : ('a -> 'b) * ('c -> 'd) -> 'c -> 'd) ) =
  let rec fp x = f (fp,gp) x
  and     gp x = g (fp,gp) x
  in (fp,gp)
 
let fixlist l = fix (fun self l -> List.map (fun li x -> li (self l) x) l) l

let words s = BatString.nsplit s " "
 
module Pair = struct
  let ( $> ) f (x,y) = (x, f y)
  let ( <$ ) f (x,y) = (f x, y)
  let ( <$> ) f (x,y) = (f x, f y)
  let ( $$ ) (f,g) (x,y) = (f x, g y)
  let pmap f (x,y) = (f x, f y)
  let pzip f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)
  let pair x y = (x,y)
  let swap (x,y) = (y,x)
end

module type TYPE = sig 
  type t 
end

module type ORD = sig
  include TYPE
  val compare : t -> t -> int
end

