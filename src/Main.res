let input = `
/*
    This is a
    block comment
*/
// This is a line comment

let x = 7 in
  let y = 8 in

  // Return a lambda with an argument b
  |b| if z = 2 then b <= 2 else m > 11 
`

let tokens = Scanning.scanTokens(input)
let expr = Parsing.parseExpr(tokens)

Console.log(JSON.stringifyAny(expr))
