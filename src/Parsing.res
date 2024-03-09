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

let dropOne = (items: array<'t>): array<'t> => items->Array.sliceToEnd(~start=1)

let expectThen = (tokens: array<token>, what: string, f: tokenType => option<'t>): parseResult<
  't,
> => {
  let peek = tokens->Array.get(0)
  switch peek {
  | Some(t) =>
    switch f(t.tType) {
    | Some(result) => Ok({result, rest: tokens->dropOne})
    | None => Error({pos: Some(t.start), eType: WrongToken({expected: what, actual: t})})
    }
  | None => Error({pos: None, eType: UnexpectedEndOfInput})
  }
}

let expect = (tokens: array<token>, what: string, p: tokenType => bool): parseResult<token> => {
  let peek = tokens->Array.get(0)
  switch peek {
  | Some(t) if p(t.tType) => Ok({result: t, rest: tokens->dropOne})
  | Some(t) => Error({pos: Some(t.start), eType: WrongToken({expected: what, actual: t})})
  | None => Error({pos: None, eType: UnexpectedEndOfInput})
  }
}

let expectExactly = (tokens: array<token>, what: string, expected: tokenType): parseResult<token> =>
  expect(tokens, what, t => t == expected)

let rec resultLoop = (
  curr: parseResult<'t>,
  f: parseSuccess<'t> => option<parseResult<'t>>,
): parseResult<'t> => {
  switch curr {
  | Ok(left) =>
    switch f(left) {
    | Some(next) => resultLoop(next, f)
    | None => Ok(left)
    }
  | err => err
  }
}

let rec parsePrimary = (tokens: array<token>): parseResult<expr> => {
  switch tokens->Array.get(0) {
  | Some(token) =>
    switch token.tType {
    | Ident(name) => Ok({result: Symbol(name), rest: tokens->Array.sliceToEnd(~start=1)})
    | Number(value) => Ok({result: Number(value), rest: tokens->Array.sliceToEnd(~start=1)})
    | True => Ok({result: Boolean(true), rest: tokens->Array.sliceToEnd(~start=1)})
    | False => Ok({result: Boolean(false), rest: tokens->Array.sliceToEnd(~start=1)})
    | String(value) => Ok({result: String(value), rest: tokens->Array.sliceToEnd(~start=1)})
    | LeftRoundBracket =>
      switch tokens->dropOne->expectExactly("`)`", RightRoundBracket) {
      | Ok(bracket) => Ok({result: Unit, rest: bracket.rest})
      | _ =>
        parseExpr(tokens->dropOne)->Result.flatMap(inner => {
          inner.rest
          ->expectExactly("Closing round bracket", RightRoundBracket)
          ->Result.map(bracket => {result: Grouping(inner.result), rest: bracket.rest})
        })
      }
    | _ =>
      Error({
        pos: Some(token.start),
        eType: WrongToken({expected: "Literal expression", actual: token}),
      })
    }
  | None => Error({pos: None, eType: UnexpectedEndOfInput})
  }
}

and parseAppl = (tokens: array<token>): parseResult<expr> => {
  parsePrimary(tokens)->Result.flatMap(left => {
    switch left.rest->expectExactly("`(`", LeftRoundBracket) {
    | Ok(openingBracket) =>
      parseExpr(openingBracket.rest)->Result.flatMap(arg => {
        arg.rest
        ->expectExactly("`)`", RightRoundBracket)
        ->Result.map(
          closingBracket => {
            rest: closingBracket.rest,
            result: Apply(left.result, arg.result),
          },
        )
      })
    | _ => Ok(left)
    }
  })
}

and parseUnary = (tokens: array<token>): parseResult<expr> => {
  let rec loop = (ctr: expr => expr, tokens: array<token>): parseResult<expr> => {
    let peek = tokens->Array.get(0)->Option.map(t => t.tType)
    switch peek {
    | Some(Exclamation) => loop(e => ctr(Not(e)), tokens->dropOne)
    | Some(Minus) => loop(e => ctr(Neg(e)), tokens->dropOne)
    | _ => parseAppl(tokens)->Result.map(inner => {result: ctr(inner.result), rest: inner.rest})
    }
  }

  loop(e => e, tokens)
}

and parseFactor = (tokens: array<token>): parseResult<expr> =>
  parseUnary(tokens)->resultLoop(left => {
    let peek = left.rest->Array.get(0)->Option.map(t => t.tType)
    let makeOperator: option<(expr, expr) => expr> = switch peek {
    | Some(Slash) => Some((e1, e2) => Div(e1, e2))
    | Some(Star) => Some((e1, e2) => Times(e1, e2))
    | _ => None
    }

    makeOperator->Option.map(ctr => {
      parseUnary(left.rest->dropOne)->Result.map(
        right => {
          {result: ctr(left.result, right.result), rest: right.rest}
        },
      )
    })
  })

and parseTerm = (tokens: array<token>): parseResult<expr> =>
  parseFactor(tokens)->resultLoop(left => {
    let peek = left.rest->Array.get(0)->Option.map(t => t.tType)
    let makeOperator: option<(expr, expr) => expr> = switch peek {
    | Some(Minus) => Some((e1, e2) => Minus(e1, e2))
    | Some(Plus) => Some((e1, e2) => Plus(e1, e2))
    | _ => None
    }

    makeOperator->Option.map(ctr => {
      parseFactor(left.rest->dropOne)->Result.map(
        right => {
          {result: ctr(left.result, right.result), rest: right.rest}
        },
      )
    })
  })

and parseComparison = (tokens: array<token>): parseResult<expr> =>
  parseTerm(tokens)->resultLoop(left => {
    let peek = left.rest->Array.get(0)->Option.map(t => t.tType)
    let makeOperator: option<(expr, expr) => expr> = switch peek {
    | Some(MoreThan) => Some((e1, e2) => MoreThan(e1, e2))
    | Some(MoreThanEq) => Some((e1, e2) => MoreThanEq(e1, e2))
    | Some(LessThan) => Some((e1, e2) => LessThan(e1, e2))
    | Some(LessThanEq) => Some((e1, e2) => LessThanEq(e1, e2))
    | _ => None
    }

    makeOperator->Option.map(ctr => {
      parseTerm(left.rest->dropOne)->Result.map(
        right => {
          {result: ctr(left.result, right.result), rest: right.rest}
        },
      )
    })
  })

and parseEquality = (tokens: array<token>): parseResult<expr> =>
  parseComparison(tokens)->resultLoop(left => {
    switch left.rest->Array.get(0)->Option.map(t => t.tType) {
    | Some(Equals) =>
      Some(
        parseComparison(left.rest->dropOne)->Result.map(right => {
          {result: Eq(left.result, right.result), rest: right.rest}
        }),
      )
    | Some(NotEquals) =>
      Some(
        parseComparison(left.rest->dropOne)->Result.map(right => {
          {result: NotEq(left.result, right.result), rest: right.rest}
        }),
      )
    | _ => None
    }
  })

and parseLambda = (tokens: array<token>): parseResult<expr> => {
  switch tokens->expectExactly("`|`", Bar) {
  | Ok(matchedBar) =>
    matchedBar.rest
    ->expectThen("identifier", tType => {
      switch tType {
      | Ident(name) => Some(name)
      | _ => None
      }
    })
    ->Result.flatMap(ident => {
      ident.rest
      ->expectExactly("`|`", Bar)
      ->Result.flatMap(matchedBar2 => {
        parseExpr(matchedBar2.rest)->Result.map(
          e => {rest: e.rest, result: Lambda(ident.result, e.result)},
        )
      })
    })
  | _ => parseEquality(tokens)
  }
}

and parseIf = (tokens: array<token>): parseResult<expr> => {
  let withIfTrue = (pred, ifTrue) => {
    ifTrue.rest
    ->expectExactly("`else`", Else)
    ->Result.flatMap(matchedElse => {
      parseExpr(matchedElse.rest)->Result.map(ifFalse => {
        {result: If(pred.result, ifTrue.result, ifFalse.result), rest: ifFalse.rest}
      })
    })
  }

  let withPred = pred => {
    pred.rest
    ->expectExactly("`then`", Then)
    ->Result.flatMap(matchedThen => {
      parseExpr(matchedThen.rest)->Result.flatMap(withIfTrue(pred, _))
    })
  }

  switch tokens->expectExactly("`if`", TIf) {
  | Ok(matchedIf) => parseLambda(matchedIf.rest)->Result.flatMap(withPred)
  | _ => parseLambda(tokens)
  }
}

and parseLet = (tokens: array<token>): parseResult<expr> => {
  let withExp = (ident, e) => {
    e.rest
    ->expectExactly("`in`", In)
    ->Result.flatMap(matchedIn => {
      parseExpr(matchedIn.rest)->Result.map(body => {
        rest: body.rest,
        result: Let(ident.result, e.result, body.result),
      })
    })
  }

  let withIdent = ident => {
    ident.rest
    ->expectExactly("`=`", Equals)
    ->Result.flatMap(equals => parseIf(equals.rest)->Result.flatMap(e => withExp(ident, e)))
  }

  switch tokens->expectExactly("`let`", Let) {
  | Ok(matchedLet) =>
    matchedLet.rest
    ->expectThen("identifier", tType =>
      switch tType {
      | Ident(name) => Some(name)
      | _ => None
      }
    )
    ->Result.flatMap(withIdent)
  | _ => parseIf(tokens)
  }
}

and parseExpr = (tokens: array<token>): parseResult<expr> => parseLet(tokens)
