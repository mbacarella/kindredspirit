open Core.Std
open Async.Std

type t = { beat_magnitude : float } [@@deriving sexp]
    
(* The magnitude of the currently detected beat. *)
val beat : float ref

(* Start beat detection subprocess.  Uses Async behind the scenes. *)
val start : unit -> unit Deferred.t
