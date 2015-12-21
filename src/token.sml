
structure Token = struct
  datatype token = 
    EOF
  | Abstype
  | And
  | Andalso
  | As
  | Case
  | Datatype
  | Do
  | Else
  | End
  | Exception
  | Fn
  | Fun
  | Handle
  | If
  | In
  | Infix
  | Infixr
  | Let
  | Local
  | Nonfix
  | Of
  | Op
  | Open
  | Orelse
  | Raise
  | Rec
  | Then
  | Type
  | Val
  | With
  | Withtype
  | While
  | Colon
  | Underscore
  | Pipe
  | FatArrow
  | Arrow
  | Pound
  | Identifier of string
  | Int of string
  | Word of string
  | Real of string
  | String of string
  | Char of char
  | LParen
  | RParen
  | LCurly
  | RCurly
  | LSquare
  | RSquare
  | Comma
  | Dot
  | Semicolon
  | Dotdotdot

  fun reserved "abstype"   = Abstype
    | reserved "and"       = And
    | reserved "andalso"   = Andalso
    | reserved "as"        = As
    | reserved "case"      = Case
    | reserved "datatype"  = Datatype
    | reserved "do"        = Do
    | reserved "else"      = Else
    | reserved "end"       = End
    | reserved "exception" = Exception
    | reserved "fn"        = Fn
    | reserved "fun"       = Fun
    | reserved "handle"    = Handle
    | reserved "if"        = If
    | reserved "in"        = In
    | reserved "infix"     = Infix
    | reserved "infixr"    = Infixr
    | reserved "let"       = Let
    | reserved "local"     = Local
    | reserved "nonfix"    = Nonfix
    | reserved "of"        = Of
    | reserved "op"        = Op
    | reserved "open"      = Open
    | reserved "orelse"    = Orelse
    | reserved "raise"     = Raise
    | reserved "rec"       = Rec
    | reserved "then"      = Then
    | reserved "type"      = Type
    | reserved "val"       = Val
    | reserved "with"      = With
    | reserved "withtype"  = Withtype
    | reserved "while"     = While
    | reserved "."         = Dot
    | reserved ":"         = Colon
    | reserved "_"         = Underscore
    | reserved "|"         = Pipe
    | reserved "=>"        = FatArrow
    | reserved "->"        = Arrow
    | reserved "#"         = Pound
    | reserved id          = Identifier(id)

  fun iseof EOF = true
    | iseof _   = false
end
