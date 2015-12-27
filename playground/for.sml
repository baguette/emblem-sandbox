
fun for iter f =
  let val i = iter () in
    case i of
      NONE => ()
    | SOME(x) => (f x; for iter f)
  end

infix to by

fun a to b =
  if a < b then
    let
      val i = ref a
      fun f () =
        if !i <= b then
          let val curr = !i in
            i := !i + 1;
            SOME(curr)
          end
        else
          NONE
    in
      f
    end
  else
    let
      val i = ref a
      fun f () =
        if !i >= b then
          let val curr = !i in
            i := !i - 1;
            SOME(curr)
          end
        else
          NONE
    in
      f
    end

fun iter by n =
  if n > 0 then
    fn () =>
      let val x = iter () in
        for (1 to (n - 1)) (fn x => iter ());
        x
      end
  else
    raise Fail "argument to by must be positive"


fun members xs =
  let val ms = ref xs in
    fn () =>
      case !ms of
        [] => NONE
      | x::xs' => (ms := xs'; SOME(x))
  end
    

infix when
fun iter when pred =
  fn () =>
    let val x = ref (iter ())
        val loop = ref true
    in
      while !loop do
        case !x of
          NONE => loop := false
        | SOME(y) => if pred y then loop := false else x := iter ();
      !x
    end

