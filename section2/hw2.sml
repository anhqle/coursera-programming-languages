(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s1, sl) =
    case sl of
        [] => NONE
      | s::sl' => if same_string(s1, s)
                  then SOME sl'
                  else case all_except_option(s1, sl') of
                           NONE => NONE
                         | SOME x => SOME (s::x)

val test1_1 = all_except_option("ha", ["hi", "ha", "ho", "hm"])
val test1_2 = all_except_option("ha", ["hi", "ho"])

fun get_substitutions1 (substitutions, s) =
    case substitutions of
        [] => []
      | sl::substitutions' => case all_except_option(s, sl) of
                                  NONE => [] @ get_substitutions1(substitutions', s)
                                | SOME x => x @ get_substitutions1(substitutions', s)

val test1_3 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                                  ["Freddie","Fred","F"]], "Fred")
val test1_4 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],
                                  ["Geoff","Jeff","Jeffrey"]], "Jeff")

fun get_substitutions2 (substitutions, s) =
    let fun aux (substitutions, acc) =
            case substitutions of
                [] => acc
              | sl::substitutions' => case all_except_option(s, sl) of
                                          NONE => aux(substitutions', [] @ acc)
                                        | SOME x => aux(substitutions', x @ acc)
    in
        aux(substitutions, [])
    end

val test1_5 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],
                                  ["Freddie","Fred","F"]], "Fred")
val test1_6 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],
                                  ["Geoff","Jeff","Jeffrey"]], "Jeff")

fun similar_names (substitutions, {first=x, middle=y, last=z}) =
    let fun replace_first_name (other_firsts, {first=x, middle=y, last=z}) =
            let val full_name = {first=x, middle=y, last=z}
            in
                case other_firsts of
                    [] => [full_name]
                  | other_first::other_firsts' => {first=other_first, middle=y, last=z} :: replace_first_name(other_firsts', full_name)
            end
    in
        replace_first_name(get_substitutions2(substitutions, x), {first=x, middle=y, last=z})
    end

val test1_7 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
                            {first="Fred", middle="W", last="Smith"})

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (card) =
    case card of
        (Clubs, _) => Black
     | (Spades, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red

val test2_1 = card_color(Diamonds, 10)

fun card_value (card) =
    case card of
        (_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

val test2_2 = card_value(Hearts, Queen)

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | card::cs' => if card = c
                     then cs'
                     else card::remove_card(cs', c, e)

val test2_3 = remove_card([(Diamonds, Num 10), (Hearts, Queen), (Spades, Num 5)],
                          (Hearts, Queen), IllegalMove)
(* val test2_4 = remove_card([(Diamonds, Num 10), (Hearts, Queen), (Spades, Num 5)], *)
(*                           (Hearts, Num 2), IllegalMove) *)

fun all_same_color(cs) =
    case cs of
        [] => true
      | _::[] => true
      | head::(neck::rest) => if card_color(head) = card_color(neck)
                              then all_same_color(neck::rest)
                              else false

val test2_4 = all_same_color([(Diamonds, Num 1), (Hearts, Num 2)])
val test2_5 = all_same_color([(Diamonds, Num 4), (Diamonds, Num 2), (Hearts, Num 3),
                             (Spades, Num 2)])

fun sum_cards (cs) =
    let fun aux (cs, acc) =
        case cs of
            [] => acc
          | c::cs' => aux(cs', card_value(c) + acc)
    in
        aux(cs, 0)
    end

val test2_6 = sum_cards([(Clubs, Ace), (Hearts, Queen), (Spades, Num 1)])

fun score (held_cards, goal) =
    let val sum = sum_cards(held_cards)
        val prelim_score = if sum > goal then 3 * (sum - goal) else 3 * (goal - sum)
        val final_score = if all_same_color(held_cards) then prelim_score div 2 else prelim_score
    in
        final_score
    end

val test2_7 = score([(Clubs, Num 10), (Hearts, Num 4)], 10)
val test2_7 = score([(Clubs, Num 10), (Clubs, Num 4)], 10)

fun officiate (card_list, move_list, goal) =
    let val held_cards = []
        fun next (card_list, held_cards, move_list) =
            case move_list of
                [] => score(held_cards, goal)
              | move::move_list' => case move of
                                        Discard c => next(card_list, remove_card(held_cards, c, IllegalMove), move_list')
                                      | Draw => case card_list of
                                                    [] => score(held_cards, goal)
                                                  | card::card_list' => let val new_hand = card::held_cards
                                                                        in
                                                                            if sum_cards(new_hand) > goal
                                                                            then score(new_hand, goal)
                                                                            else next(card_list', new_hand, move_list')
                                                                        end
    in
        next(card_list, held_cards, move_list)
    end

fun officiate2 (card_list, move_list, goal) =
    let val held_cards = []
        fun next (card_list, held_cards, move_list) =
            case (card_list, move_list) of
                (_, []) => score(held_cards, goal)
              | (card_list, (Discard c)::move_list') => next(card_list,
                                                             remove_card(held_cards, c, IllegalMove), move_list')
              | ([], Draw::move_list') => score(held_cards, goal)
              | (card::card_list', Draw::move_list') => let val new_hand = card::held_cards
                                                          in
                                                              if sum_cards(new_hand) > goal
                                                              then score(new_hand, goal)
                                                              else next(card_list', new_hand, move_list')
                                                          end
    in
        next(card_list, held_cards, move_list)
    end
