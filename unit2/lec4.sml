(* Programming Languages, Dan Grossman, CSE341 *)

(* Lecture 4: records, datatypes, case expressions *)

(* records *)

val x = {bar = (1+2,true andalso true), foo = 3+4, baz = (false,9) }

val my_first_kid = {name = "Matai", id = 1 }

val my_second_kid = {name = "Natan", id = 2 }

val brain_part = {id = true, ego = false, superego = false}

fun is_foo_positive (x : {bar : int * bool, foo : int, baz : bool * int }) =
    #foo x > 0

(* records are like tuples with user-defined field names
   conversely, tuples are just records with names 1, 2, ..., n
   the only real difference is "by position" vs. "by name"
*)
val a_pair = (3+1,4+2)
val a_record = {second=4+2, first=3+1}

(* actually, tuples *are* just records with names 1, 2, ..., n and
special "by position" syntax -- our first example of "syntactic sugar" *)
val another_pair = {2=5, 1=6}
val sum = (#1 a_pair + #1 another_pair, #2 a_pair + #2 another_pair)

(* datatypes *)

datatype mytype = TwoInts of int * int 
                | Str of string 
                | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2,3+4)
val e = a

(* Do _not_ redo datatype bindings (e.g., via use "lec4.sml") 
datatype mytype = TwoInts of int * int | Str of string | Pizza *)

fun f x = 
    case x of 
        Pizza => 3 
      | Str s => 8
      | TwoInts(i1,i2) => i1 + i2

(*    | Pizza => 4; (* redundant case: error *) *)

(*fun g x = case x of Pizza => 3 (* missing cases: warning *) *)

