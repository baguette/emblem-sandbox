type lexresult = Token.token

fun eof () = Tokens.EOF(0, 0)

fun reserved "abstype"   = Token.Abstype
  | reserved "and"       = Token.And
  | reserved "andalso"   = Token.Andalso
  | reserved "as"        = Token.As
  | reserved "case"      = Token.Case
  | reserved "datatype"  = Token.Datatype
  | reserved "do"        = Token.Do
  | reserved "else"      = Token.Else
  | reserved "end"       = Token.End
  | reserved "exception" = Token.Exception
  | reserved "fn"        = Token.Fn
  | reserved "fun"       = Token.Fun
  | reserved "handle"    = Token.Handle
  | reserved "if"        = Token.If
  | reserved "in"        = Token.In
  | reserved "infix"     = Token.Infix
  | reserved "infixr"    = Token.Infixr
  | reserved "let"       = Token.Let
  | reserved "local"     = Token.Local
  | reserved "nonfix"    = Token.Nonfix
  | reserved "of"        = Token.Of
  | reserved "op"        = Token.Op
  | reserved "open"      = Token.Open
  | reserved "orelse"    = Token.Orelse
  | reserved "raise"     = Token.Raise
  | reserved "rec"       = Token.Rec
  | reserved "then"      = Token.Then
  | reserved "type"      = Token.Type
  | reserved "val"       = Token.Val
  | reserved "with"      = Token.With
  | reserved "withtype"  = Token.Withtype
  | reserved "while"     = Token.While
  | reserved ":"         = Token.Colon
  | reserved "_"         = Token.Underscore
  | reserved "|"         = Token.Pipe
  | reserved "="         = Token.Equals
  | reserved "=>"        = Token.FatArrow
  | reserved "->"        = Token.Arrow
  | reserved "#"         = Token.Pound
  | reserved id          = Token.Identifier(id)

%%

digits=[0-9]+
alphanum=[_a-zA-Z'][_a-zA-Z0-9']*
symbolic=[!%&$#+-/:<=>?@\~'^|*][!%&$#+-/:<=>?@\~'^|*]*


