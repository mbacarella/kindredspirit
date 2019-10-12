open! Core
open! Async

module Virtual_strip : sig
  type t =
    { controller_id : int
    ; strip_id : int
    ; way_points : Coordinate.t list }
  [@@deriving sexp, fields]
end

type t =
    { virtual_strips : Virtual_strip.t list
    ; virtual_pixels : Virtual_pixel.t list
    ; controller_ids : Int.Set.t
    ; x_limits : float * float
    ; y_limits : float * float
    ; z_limits : float * float
    ; y_map : (int, Virtual_pixel.t array) Core.Map.Poly.t
    ; y_range : int array
    ; max_y : int }
[@@deriving sexp, fields]

val load : string -> t Deferred.t
val dup : t -> t
val dump_sexp : t -> unit
val dump_csv : t -> unit
