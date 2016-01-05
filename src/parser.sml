structure Parser = struct
  exception Expected of string * string * int * int

  val tok = ref Token.EOF

  fun bump fb =
    tok := Lexer.lex fb

  fun peek () =
    let Token.Token { typ, ... } = !tok in
      typ
    end

  fun expected t =
    let Token.Token { typ, line, start_col, ... } = !tok in
      raise Expected(t, Token.toString typ, line, start_col)
    end

  fun eat fb t =
    let Token.Token { typ, ... } = !tok in
      if typ = t then
        bump fb
      else
        expected (Token.toString t)
    end

  fun optional fb t =
    let Token.Token { typ, ... } = !tok in
      if typ = t then
        (bump fb; true)
      else
        false
    end

  val decfirst = [Token.Val, Token.Fun, Token.Datatype, Token.Type,
                  Token.Exception, Token.Local, Token.Open, Token.Infix,
                  Token.Infixr, Token.Nonfix]

  fun mem x xs = List.exists (fn x' => x = x') xs

  fun int_or_0 fb =
    case peek() of
      Token.Int(x) => (bump fb; x)
    | _ => 0

  fun identifier fb =
    case peek() of
      Token.Identifier(x) => (bump fb; x)
    | _ => expected "identifier"

  fun program fb =
    topdec fb;
    progsuffix fb

  and progsuffix fb =
    optional fb Token.Semicolon;
    program fb

  and topdec fb =
    case peek() of
      Token.Structure => (eat fb Token.Structure; strbind fb)
    | Token.Signature => (eat fb Token.Signature; sigbind fb)
    | Token.Functor   => (eat fb Token.Functor;   funbind fb)
    | other =>
      if mem other decfirst then
        dec fb
      else
        exp fb

  and decone fb =
    case peek() of
      Token.Val => (eat fb Token.Val; valbind fb)
    | Token.Fun => (eat fb Token.Fun; fvalbind fb)
    | Token.Type => (eat fb Token.Type; typbind fb)
    | Token.Datatype => (eat fb Token.Datatype; datbind fb)
    | Token.Exception => (eat fb Token.Exception; exbind fb)
    | Token.Local => (eat fb Token.Local;
                      let val locals = dec fb in
                      eat fb Token.In;
                      let val body = dec fb in
                      eat fb Token.End;
                      Absyn.Local {locals = locals, body = body}
                      end
                      end)
    | Token.Open => (eat Token.Open;
                     Absyn.Open {ids = identifier fb :: identifierseq fb})
    | Token.Infix => (eat Token.Infix;
                      Absyn.Fixity {dir = Left (int_or_0 fb),
                                    ids = (identifier fb :: identifierseq fb)})
    | Token.Infixr => (eat Token.Infixr;
                       Absyn.Fixity {dir = Right (int_or_0 fb),
                                     ids = (identifier fb :: identifierseq fb)})
    | Token.Nonfix => (eat Token.Nonfix;
                       Absyn.Fixity {dir = Non,
                                     ids = (identifier fb :: identifierseq fb)})

  and dec fb =
    (decone fb) :: (decsequence fb)

  and decsequence fb =
    optional fb Token.semicolon;
    if mem (peek()) decfirst then
      (decone fb) :: (decsequence fb)
    else
      []

  and valbind fb =
    let val recr = optional fb Token.Rec
        val patt = pat fb
        val _ = eat fb Token.Equals
        val expr = exp fb
        val andvalbind = optional fb Token.And
    in
      Absyn.Valdec {
        recr = recr,
        patt = patt,
        expr = expr,
        typ = NONE,
        anddec = if andvalbind then
                   SOME (valbind fb)
                 else
                   NONE
      }
    end

  and fvalbind fb =
    let val op = optional fb Token.Op
        val id = identifier fb
        val args = funargs fb
        val _ = eat fb Token.Equals
        val body = exp fb
        val andvalbind = optional fb Token.And
    in
      Absyn.Fvaldec {
        id = id,
        args = args,
        body = body,
        typ = NONE,
        anddec = if andvalbind then
                   SOME (fvalbind fb)
                 else
                   NONE
      }
end

