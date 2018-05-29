(* CSE 341, Winter 2013 *)

(* Lecture 5: More Datatypes and Pattern-Matching *)

datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int 
            | Name of string * (string option) * string

datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
      | Negate e2  => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1,e2) => (eval e1) * (eval e2)

fun number_of_adds e =
    case e of
        Constant i      => 0
      | Negate e2       => number_of_adds e2
      | Add(e1,e2)      => 1 + number_of_adds e1 + number_of_adds e2
      | Multiply(e1,e2) => number_of_adds e1 + number_of_adds e2

val example_exp = Add (Constant 19, Negate (Constant 4))

val example_ans = eval example_exp

val example_addcount = number_of_adds (Multiply(example_exp,example_exp))

fun max_constant e =
    let fun max_of_two (e1,e2) =
	    let val m1 = max_constant e1
		val m2 = max_constant e2
	    in 
		if m1 > m2 then m1 else m2 
	    end
    in
	case e of
	    Constant i      => i
	  | Negate e2       => max_constant e2
	  | Add(e1,e2)      => max_of_two(e1,e2)
	  | Multiply(e1,e2) => max_of_two(e1,e2)
    end

val nineteen = max_constant example_exp

(* Note: Using Int.max, we don't need a local helper function. 
This second version is fewer lines, but there is a bit more
code copying. *)

fun max_constant2 e =
    case e of
	Constant i      => i
      | Negate e2       => max_constant2 e2
      | Add(e1,e2)      => Int.max(max_constant2 e1, max_constant2 e2)
      | Multiply(e1,e2) => Int.max(max_constant2 e1, max_constant2 e2)

(* lists and options are just (polymorphic) datatypes *)

datatype my_int_list = Empty 
                     | Cons of int * my_int_list
					 
val one_two_three = Cons(1,Cons(2,Cons(3,Empty)))

fun append_mylist (xs,ys) = 
    case xs of
        Empty => ys
      | Cons(x,xs') => Cons(x, append_mylist(xs',ys))

fun inc_or_zero intoption =
    case intoption of
        NONE => 0
      | SOME i => i+1

fun sum_list xs =
    case xs of
        [] => 0
      | x::xs' => x + sum_list xs'

fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append(xs',ys)

(* pattern-matching for each-of types *)

(* one-arm case expressions are bad style, but explain what's happening *)
fun sum_triple1 (triple : int * int * int) =
    case triple of
      (x,y,z) => z + y + x

fun full_name1 (r : {first:string,middle:string,last:string}) =
    case r of 
        {first=x,middle=y,last=z} => x ^ " " ^ y ^ " " ^z

(* okay style -- use a val-binding instead of one-arm case *)
fun full_name2 (r : {first:string,middle:string,last:string}) =
    let val {first=x,middle=y,last=z} = r
    in
	x ^ " " ^ y ^ " " ^z
    end

fun sum_triple2 triple =
    let val (x,y,z) = triple 
    in 
        x + y + z 
    end

(* but this is better style -- can just put pattern in function binding *)
(* -- in fact, that is what we have been doing since lecture 2!!! *)
fun full_name3 {first=x,middle=y,last=z} =
    x ^ " " ^ y ^ " " ^z

fun sum_triple3 (x,y,z) =
    x + y + z

(* makes it convenient to pass results of one function to another *)

fun rotate_left (x,y,z) = (y,z,x)

fun rotate_right triple = rotate_left(rotate_left triple)
