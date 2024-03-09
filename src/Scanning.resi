type position = {index: int, charNum: int, lineNum: int}

type tokenType =
  | LeftRoundBracket
  | RightRoundBracket
  | Slash
  | Star
  | Minus
  | Plus
  | Exclamation
  | MoreThan
  | MoreThanEq
  | LessThan
  | LessThanEq
  | Equals
  | NotEquals
  | Bar
  | Comma
  | TIf
  | Then
  | Else
  | True
  | False
  | Let
  | In
  | Ident(string)
  | String(string)
  | Number(float)
  | EndOfInput
  | Unrecognised(string)

type token = {tType: tokenType, start: position, end: position}

type scanResult = {token: token, newPos: position}

let scanTokens: string => array<token>
