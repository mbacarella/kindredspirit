(** The interface to the Pixel Pusher subsystem.

  Pixel Pushers are devices that sit on IP networks that can be sent
  commands over UDP to control LEDs and other kinds of lighting hardware.

  Each Pixel Pusher can have up to eight "strips" connected, and each strip
  can have up to N pixels.

  This module listens for Pixel Pushers to announce themselves, remembers
  them, and provides an interface for sending color commands to pixels on
    strips. *)

open Core.Std
open Async.Std
  
module Pixel : sig
  type t = { red: int; green: int; blue: int }
end
  
module Strip : sig
  type t =
      { strip_number: int
      ; strip_length : int
      ; controller_id : int
      ; group_id : int
      ; matrix : Pixel.t Array.t }
  val set_pixel : t -> color:Pixel.t -> index:int -> unit
end

(* Returns all strips seen by the subsystem. *)
val get_strips : unit -> Strip.t list

(* Begins watching for Pixel Pusher presence UDP broadcasts.
   Wrap this in a don't_wait_for because it never becomes
   determined. *)
val start_discovery_listener : unit -> unit Deferred.t
