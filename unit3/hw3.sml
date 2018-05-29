(* Dan Grossman, CSE341 Winter 2013, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu


(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals (sl) =
    List.filter (Char.isUpper o (fn x => String.sub(x, 0))) sl

val test1 = only_capitals(["Haha", "hihi", "AbD", "abD"])

(* 2 *)
fun longest_string1 (sl) =
    foldl (fn (x, y) => if String.size(x) < String.size(y)
                        then y
                        else x) "" sl

val test2_1 = longest_string1(["abc", "ab", "abdc", "a"])
val test2_2 = longest_string1([])

(* 3 *)
fun longest_string2 (sl) =
    foldl (fn (x, y) => if String.size(x) > String.size(y)
                        then x
                        else y) "" sl

(* 4 *)
fun longest_string_helper f sl =
    foldl (fn (x, y) => if f(x, y) then x else y) "" sl

val longest_string3 = longest_string_helper (fn(xsize, ysize) => xsize >= ysize)
val longest_string4 = longest_string_helper (fn(xsize, ysize) => xsize > ysize)

val test4_1 = longest_string3(["abc", "ab", "abdc", "a"])

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals
val test5 = longest_capitalized(["abcasdfa", "Ad", "A"])

(* 6 *)
val rev_string = implode o rev o explode
val test6 = rev_string("abcd")

(* 7 *)
fun first_answer f l =
    case l of
        [] => raise NoAnswer
      | item::l' => case f(item) of
                        SOME result => result
                      | NONE => first_answer f l'

(* 8 *)

fun all_answers f l =
    let fun aux l acc =
            case l of
                [] => SOME acc
              | i::l' => case f i of
                             NONE => NONE
                           | SOME res => aux l' (res @ acc)
    in
        aux l []
    end
val test8_1 = all_answers (fn x => if x = 0 then NONE else SOME [2 div x]) [1, 2, 3]
val test8_2 = all_answers (fn x => if x = 0 then NONE else SOME [2 div x]) [1, 0, 3]

(* 9 *)
(* If pattern match Wildcard return result of f1()
If match with Variable x, return f2 x
If match with a list of pattern, (r p) produces an int (do it recursively into each pattern in ps),
produce an int overall
If match with Constructor, produces (r p)                          *)
fun g f1 f2 p =
    let
	      val r = g f1 f2
    in
	      case p of
	          Wildcard          => f1 ()
	        | Variable x        => f2 x
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | ConstructorP(_,p) => r p
	        | _                 => 0
    end

val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat p =
    let fun get_strings p =
            case p of
                Wildcard => []
              | Variable x => [x]
              | TupleP ps => List.foldl (fn (p, l) => (get_strings p) @ l ) [] ps
              | ConstructorP(_, p) => get_strings p
              | _ => []
        fun check_repeats sl =
            case sl of
                [] => false
              | s::sl' => if List.exists (fn x => if x = s then true else false) sl'
                          then true
                          else check_repeats sl'
    in
        check_repeats (get_strings p)
    end

val test10_1 = check_pat (TupleP [Variable "x", Variable "y",
                                 TupleP [Variable "x", Variable "z"]]) (* expect true *)
val test10_2 = check_pat (TupleP [Variable "x", Variable "y"]) (* expect false *)

(* 11 *)
fun match (value, pattern) =
    case (value, pattern) of
        (_, Wildcard) => SOME []
      | (anyv, Variable s) => SOME [(s, anyv)]
      | (Unit, UnitP) => SOME []
      | (Const v, ConstP p) => if p = v then (SOME []) else NONE
      | (Tuple vl, TupleP pl) => (case all_answers match (ListPair.zip(vl, pl)) of
                                      NONE => NONE
                                    | SOME bindingl => SOME bindingl)
      | (Constructor (s2, v), ConstructorP (s1, p)) => if s1 = s2
                                                    then (match (v, p))
                                                    else NONE
      | (_, _) => NONE

(* 12 *)
fun first_match v pl =
    SOME (first_answer (fn p => match(v, p)) pl)
    handle NoAnswer => NONE
