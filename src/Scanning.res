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

exception UnterminatedString

type scanResult = {token: token, newPos: position}

let charCode = s => String.charCodeAt(s, 0)
let isLowerCase = s => charCode(s) >= 97.0 && charCode(s) <= 122.0
let isUpperCase = s => charCode(s) >= 65.0 && charCode(s) <= 90.0
let isLetter = s => isLowerCase(s) || isUpperCase(s)
let isUnderscore = s => s == "_"
let isNumber = s => charCode(s) >= 48.0 && charCode(s) <= 57.0
let isIdentRest = s => isLetter(s) || isUnderscore(s) || isNumber(s)
let isIdentStarter = s => isLetter(s) || isUnderscore(s)

let plusChars = (pos: position, charCount: int): position => {
  ...pos,
  index: pos.index + charCount,
  charNum: pos.charNum + charCount,
}

let trim = (s: string, pos: position): position => {
  let rec loop = (curr: position): position => {
    switch s->String.charAt(curr.index) {
    | "\n" => loop({index: curr.index + 1, charNum: 1, lineNum: curr.lineNum + 1})
    | "\t" | " " => loop({...curr, index: curr.index + 1, charNum: curr.charNum + 1})
    | _ => curr
    }
  }
  loop(pos)
}

let lineComment = (s: string, pos: position): position => {
  let rec loop = (curr: position): position => {
    switch s->String.charAt(curr.index) {
    | "\n" => {index: curr.index + 1, charNum: 1, lineNum: curr.lineNum + 1}
    | "" => curr
    | _ => loop({...curr, index: curr.index + 1, charNum: curr.charNum + 1})
    }
  }
  loop(pos)
}

let blockComment = (s: string, pos: position): position => {
  let rec loop = (curr: position): position => {
    switch s->String.charAt(curr.index) {
    | "\n" => loop({index: curr.index + 1, charNum: 1, lineNum: curr.lineNum + 1})
    | "*" =>
      if s->String.charAt(curr.index + 1) == "/" {
        {...curr, index: curr.index + 2, charNum: curr.charNum + 2}
      } else {
        loop({...curr, index: curr.index + 2, charNum: curr.charNum + 2})
      }
    | _ => loop({...curr, index: curr.index + 1, charNum: curr.charNum + 1})
    }
  }
  loop(pos)
}

let number = (s: string, pos: position): position => {
  let rec loop = (curr: position): position => {
    switch s->String.charAt(curr.index) {
    | x if isNumber(x) => loop({...curr, index: curr.index + 1, charNum: curr.charNum + 1})
    | _ => curr
    }
  }
  let maybeDecimal = loop(pos)
  if s->String.charAt(maybeDecimal.index) == "." {
    loop({...maybeDecimal, index: maybeDecimal.index + 1, charNum: maybeDecimal.charNum + 1})
  } else {
    maybeDecimal
  }
}

let string_ = (s: string, pos: position): position => {
  let rec loop = (curr: position): position => {
    switch s->String.charAt(curr.index) {
    | "\\" => loop({...curr, index: curr.index + 2, charNum: curr.charNum + 2})
    | "\"" => {...curr, index: curr.index + 1, charNum: curr.charNum + 1}
    | "\n" => loop({index: curr.index + 1, charNum: 1, lineNum: curr.lineNum + 1})
    | "" => raise(UnterminatedString)
    | _ => loop({...curr, index: curr.index + 1, charNum: curr.charNum + 1})
    }
  }
  loop(pos->plusChars(1))
}

let word = (s: string, pos: position): position => {
  let rec loop = (curr: position): position => {
    switch s->String.charAt(curr.index) {
    | c if isIdentRest(c) =>
      loop({
        ...curr,
        index: curr.index + 1,
        charNum: curr.charNum + 1,
      })
    | _ => curr
    }
  }
  loop(pos)
}

let startPosition = {index: 0, charNum: 1, lineNum: 1}

let nextToken = (s: string, pos: position): scanResult => {
  let rec loop = (pos: position): scanResult => {
    let start = trim(s, pos)

    let singleChar = tType => {
      let end = start->plusChars(1)
      {token: {tType, start, end}, newPos: end}
    }

    let twoChars = tType => {
      let end = start->plusChars(2)
      {token: {tType, start, end}, newPos: end}
    }

    switch String.charAt(s, start.index) {
    | "(" => singleChar(LeftRoundBracket)
    | ")" => singleChar(RightRoundBracket)
    | "/" =>
      if String.charAt(s, start.index + 1) == "/" {
        let afterComment = lineComment(s, start->plusChars(1))
        loop(afterComment)
      } else if String.charAt(s, start.index + 1) == "*" {
        let afterComment = blockComment(s, start->plusChars(1))
        loop(afterComment)
      } else {
        singleChar(Slash)
      }
    | "*" => singleChar(Star)
    | "-" => singleChar(Minus)
    | "+" => singleChar(Plus)
    | "!" =>
      if String.charAt(s, start.index + 1) == "=" {
        twoChars(NotEquals)
      } else {
        singleChar(Exclamation)
      }
    | ">" =>
      if String.charAt(s, start.index + 1) == "=" {
        twoChars(MoreThanEq)
      } else {
        singleChar(MoreThan)
      }
    | "<" =>
      if String.charAt(s, start.index + 1) == "=" {
        twoChars(LessThanEq)
      } else {
        singleChar(LessThan)
      }
    | "=" => singleChar(Equals)
    | "|" => singleChar(Bar)
    | "," => singleChar(Comma)
    | "" => {token: {tType: EndOfInput, start, end: start}, newPos: start}
    | c =>
      if isIdentStarter(c) {
        let end = word(s, start)
        let value = s->String.substring(~start=start.index, ~end=end.index)
        let tType = switch value {
        | "if" => TIf
        | "then" => Then
        | "else" => Else
        | "true" => True
        | "false" => False
        | "let" => Let
        | "in" => In
        | x => Ident(x)
        }
        {
          token: {
            tType,
            start,
            end,
          },
          newPos: end,
        }
      } else if isNumber(c) {
        let end = number(s, start)
        let value = s->String.substring(~start=start.index, ~end=end.index)
        {token: {tType: Number(Float.parseFloat(value)), start, end}, newPos: end}
      } else if c == "\"" {
        let end = string_(s, start)
        let value = s->String.substring(~start=start.index + 1, ~end=end.index - 1)
        {
          token: {
            tType: String(value),
            start,
            end,
          },
          newPos: end,
        }
      } else {
        singleChar(Unrecognised(c))
      }
    }
  }

  loop(pos)
}

let scanTokens = (str: string): array<token> => {
  let rec loop = (position: position, output: array<token>): array<token> => {
    let result = nextToken(str, position)
    if result.token.tType == EndOfInput {
      output
    } else {
      loop(result.newPos, Array.concat(output, [result.token]))
    }
  }

  loop(startPosition, [])
}
