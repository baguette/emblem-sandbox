
structure Lexer = struct
  fun isalpha x =
    ((ord x) >= (ord #"a") andalso (ord x) <= (ord #"z"))
    orelse
    ((ord x) >= (ord #"A") andalso (ord x) <= (ord #"Z"))
    orelse x = #"_" orelse x = #"'"

  fun isdigit x =
    ((ord x) >= (ord #"0") andalso (ord x) <= (ord #"9"))

  fun isoneof x str =
    contains x (explode str)

  fun isalphanumeric x =
    (isalpha x) orelse (isdigit x) orelse x = #"_" orelse x = #"'"

  fun issymbol x =
    isoneof x "!%&$#+-/:<=>?@\\~'^|*"

  fun iswhitespace x =
    isoneof x " \t\n"

  fun isnewline x = x = #"\n"

  fun lex_while pred fb =
    let
      fun lex_while_rev xs =
        case FileBuf.getch fb of
          NONE => xs
        | SOME(c) => if pred c then
                       (FileBuf.bump fb; lex_while_rev (c::xs))
                     else xs
    in
      implode (rev (lex_while_rev []))
    end

  fun lex_until pred fb =
    let fun p x = not (pred x) in
      lex_while p fb
    end

  (* TODO: Lex numbers *)

  (* TODO: Other escapes *)
  fun lex_string fb =
    let
      val last = ref (FileBuf.getch fb);
      fun bump () = (last := FileBuf.getch fb; FileBuf.bump fb);
      fun lex_string_rev xs =
        case FileBuf.getch fb of
          NONE => raise Fail("Unclosed string")
        | SOME(#"\"") => (case !last of
                            SOME(#"\\") => (bump(); lex_string_rev (#"\""::xs))
                          | _ => (bump(); xs))
        | SOME(c) => (bump(); lex_string_rev (c::xs))
    in
      implode (rev (lex_string_rev []))
    end

  fun lex_block_comment fb =
    let val () = FileBuf.bump fb
        val last = ref (FileBuf.getch fb)
        val loop = ref true
    in
      while !loop do
        (FileBuf.bump fb;
         case !last of
           SOME(#"(") =>
             (case FileBuf.getch fb of
                SOME(#"*") =>
                  (lex_block_comment fb;
                   last := FileBuf.getch fb)
              | SOME(c) =>
                  last := FileBuf.getch fb
              | NONE =>
                  loop := false)
         | SOME(#"*") =>
             (case FileBuf.getch fb of
                SOME(#")") =>
                  loop := false
              | SOME(c) =>
                  last := FileBuf.getch fb
              | NONE =>
                  loop := false)
         | SOME(c) =>
             last := FileBuf.getch fb
         | NONE =>
             loop := false);
       FileBuf.bump fb
    end

  fun lex_line_comment fb =
    lex_until isnewline fb


  fun lex fb =
    let fun return t = (FileBuf.bump fb; t) in
      case FileBuf.getch fb of
        NONE => Token.EOF
      | SOME(c) =>

        if iswhitespace c then
          (lex_while iswhitespace fb; lex fb)

        else if c = #"(" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"*") => (lex_block_comment fb; lex fb)
           | _ => Token.LParen)

        else if c = #"-" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"-") => (lex_line_comment fb; lex fb)
           | _ => Token.reserved (lex_while issymbol fb))

        else if isalpha c then
          Token.reserved (lex_while isalphanumeric fb)

        else if issymbol c then
          Token.reserved (lex_while issymbol fb)

        else
          case c of
            #")" => return Token.RParen
          | #"[" => return Token.LSquare
          | #"]" => return Token.RSquare
          | #"{" => return Token.LCurly
          | #"}" => return Token.RCurly
          | #"," => return Token.Comma
          | #";" => return Token.Semicolon
          | #"\"" => return Token.String(lex_string fb)
          | _ => raise Fail("Lexical error")
    end
end

