open! Core.Std
  
type t =
  { controller_id : int
  ; strip_id : int
  ; pixel_id : int
  ; coord : Coordinate.t
  ; mutable color : Color.t }
[@@deriving sexp, fields]
