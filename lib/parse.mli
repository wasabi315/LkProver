open Lk

val from_channel : in_channel -> (Sequent.t, string) result
val from_string : string -> (Sequent.t, string) result
