open Core

val pcm_data : int Array.t
val index : int ref

val start : sound_dev:string -> unit
