open Core
open Async

(* The magnitude of the currently detected beat. *)
val beat : float ref

(* Start beat detection subprocess.  Uses Async behind the scenes. *)
val start : sound_dev:string -> unit Deferred.t
