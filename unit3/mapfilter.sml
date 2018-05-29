fun map (f, xs) =
    case xs of
        [] => []
      | x::xs' => f(x)::map(f, xs')

fun filter (f, xs) =
    case xs of
        [] => []
     | x::xs' => if f(x) then x::filter(f, xs') else filter(f, xs')

fun fold (f, acc, xs) =
    case xs of
        [] => acc
     | x::xs' => fold(f, f(acc, x), xs'))
