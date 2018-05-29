fun sumlist xs =
    case xs of
        [] => 0
      | x::xs' => x + sumlist xs'

val test1 = sumlist [1, 2, 3]; 

fun appendlist (xs, ys) =
    case xs of
        [] => ys
      | x::xs' => x :: appendlist(xs', ys)

val test2 = appendlist([1, 2], [3, 4, 5]);

fun zip3 list_triple =
    case list_triple of
        ([], [], []) => []
     | (x::xs', y::ys', z::zs') => (x, y, z)::zip3(xs', ys', zs')

val test3 = zip3([1, 4, 7], [2, 4, 8], [3, 6, 9]);

datatype poker = Color of string
       | int

val a = (Color "red", 10)
val b = Color "blue"
