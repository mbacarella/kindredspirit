open Core.Std
open Async.Std

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
    ; controller_ids : Int.Set.t }
[@@deriving sexp, fields]
    
val load : string -> t Deferred.t
val dup : t -> t
