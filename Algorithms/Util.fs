module Util
(* This module contains some utility functions to be used anywhere is the FS code. *)
    
    (* max function *)
    let max(a,b) =  if a < b then b else a

    (* string exception definition *)
    exception Fail of string

    (* raises an exception Fail with string s *)
    let fail(s) = raise(Fail(s))
