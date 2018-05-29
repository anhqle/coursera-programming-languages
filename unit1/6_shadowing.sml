(* Programming Languages, Dan Grossman *)
(* Section 1: Examples to Demonstrate Shadowing *)

val a = 10

val b = a * 2

val a = 5 (* this is not an assignment statement. It's a binding statement, binding a to 5. *)
(* if it were a assignment statement, it means that there is a container, called a, and its content is now changed)
(* but a here is just a label *)
(* a -> 5, b -> 20 *)
            
val c = b

(* a -> 5, b -> 20, c -> 20 *)

val d = a

val a = a + 1

(* next line does not type-check, f not in environment *)
(* val g = f - 3  *)

val f = a * 2

