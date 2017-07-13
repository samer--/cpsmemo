
open Cpsmemo

module Test3 = TestG (G3)

let main args = 
  let n = int_of_string args.(1) in
  let gr = args.(2) in
  let _ = Test3.(profile gr (sent n)) in
  ()

let _ = main Sys.argv
