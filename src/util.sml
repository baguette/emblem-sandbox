
infixr <|
fun (f <| x) = f x

infixr |>
fun (x |> f) = f x

fun contains x xs =
  case xs of
    [] => false
  | y::ys => if x = y then true else contains x ys

