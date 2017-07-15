(* Test the continuation based parser on some ambiguous left
 * recursive grammars. Command line usage is:
 *    $ ./test <grammar> <input length>
 * <grammar> is one of sm, sml, or smml.
 *)

open Cpsmemo

let main args = 
  let n = int_of_string args.(2) in
  let gr = args.(1) in
  let _ = TestT2.(profile gr (sent n)) in
  ()

let _ = main Sys.argv
