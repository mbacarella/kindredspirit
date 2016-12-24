open Core.Std
open Async.Std

module Coordinate : sig
  type t = { x : float; y : float; z : float } with sexp
  val dist : t -> t -> float
end

module Virtual_pixel : sig
  type t =
    { controller_id : int
    ; strip_id : int
    ; pixel_id : int
    ; coord : Coordinate.t
    ; mutable color : Color.t }
  with sexp
end
  
module Virtual_strip : sig
  type t =
    { controller_id : int
    ; strip_id : int
    ; way_points : Coordinate.t list }
  with sexp
end

type t =
    { virtual_strips : Virtual_strip.t list
    ; virtual_pixels : Virtual_pixel.t list }
with sexp
    
val load : string -> t Deferred.t
val dup : t -> t
