(** This is the interface to the Pixel Pusher subsystem.

  Pixel Pushers are devices that sit on IP networks that can be sent
  commands over UDP to control LEDs and other kinds of lighting hardware.

  Each Pixel Pusher can have up to eight LED "strips" connected, and each strip
  can have up to N "pixels".

  This module listens for Pixel Pushers to announce themselves, remembers
  them, and provides an interface for telling a pixel on a strip
  to turn to a color.

  This library depends on the Async library, but it can be linked into non-Async
  applications.  The library uses Async to live and handle background tasks, such
  as listening for beacons from Pixel Pushers and sending packets.

  Since Async is a cooperative multi-tasking system, make sure in your main thread
  you frequently yield the CPU, such as by calling UNIX sleep at least as often
  as your expected FPS rate.
*)

open Core
open Async

module Controller_report : sig
  type t =
      { controller_id : int
      ; group_id : int
      ; update_period : Time.Span.t
      ; last_beacon : Time.t }
end

module Strip : sig
  type t =
      { strip_number: int
      ; strip_length : int
      ; controller_id : int
      ; group_id : int
      ; matrix : Color.t Array.t }
  val set_pixel : t -> color:Color.t -> index:int -> unit
end

type send_updates_t

(* Begins watching for Pixel Pusher presence UDP broadcasts.  *)
val start : unit -> send_updates_t Deferred.t

(* List of all currently seen controllers. *)
val get_controllers : unit -> Controller_report.t list

(* Returns all strips seen by the subsystem. *)
val get_strips : unit -> Strip.t list

(* Like get_strips, but strips are indexed by (controller_id, strip_id) *)
val get_strips_as_map : unit -> (int * int, Strip.t) Map.Poly.t

(* Instructs subsystem to release any pending updates.
   Do this every time you've finished creating your "frame".
   Only call this if you use Async in the rest of your program. *)
val send_updates : send_updates_t -> unit Deferred.t

(* Same as above, but for calling from non-Async contexts. *)
val send_updates_from_non_async_thread : send_updates_t -> unit
