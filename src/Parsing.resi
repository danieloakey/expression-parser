open Scanning

type rec expr =
  | Number(float)
  | String(string)
  | Boolean(bool)
  | Unit
  | Eq(expr, expr)
  | NotEq(expr, expr)
  | MoreThan(expr, expr)
  | MoreThanEq(expr, expr)
  | LessThan(expr, expr)
  | LessThanEq(expr, expr)
  | Minus(expr, expr)
  | Plus(expr, expr)
  | Times(expr, expr)
  | Div(expr, expr)
  | Not(expr)
  | Neg(expr)
  | Symbol(string)
  | Grouping(expr)
  | If(expr, expr, expr)
  | Let(string, expr, expr)
  | Lambda(string, expr)
  | Apply(expr, expr)

type parseErrorType = UnexpectedEndOfInput | WrongToken({expected: string, actual: token})

type parseError = {pos: option<position>, eType: parseErrorType}

type parseSuccess<'t> = {result: 't, rest: array<token>}

type parseResult<'t> = result<parseSuccess<'t>, parseError>

let parseExpr: array<token> => parseResult<expr>
