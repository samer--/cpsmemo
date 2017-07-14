(* open Utils *)

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

module type MONADCHOICE = sig
  include MONAD
  val choose : 'a list -> 'a m
end

module type MONADST = sig
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
  let return = return

  let liftM op m = m >>= return ** op
  let liftM2 op m n = m >>= fun x -> n >>= (return ** op x)

  let rec mapM f = function 
    | [] -> return []
    | x :: xs -> liftM2 cons (f x) (mapM f xs)
end

module IdM = struct
   type 'a m = 'a
   let bind f g = g f
   let return x = x
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
  let choose l = M.return l
  let choose' a = M.return (Array.to_list a)
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
