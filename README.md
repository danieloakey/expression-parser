# Expression Parser - Demo of ReScript

This is a simple expression parser I wrote just to try out the [ReScript language](https://rescript-lang.org/).

There are likely loads of bugs and edge cases, but maybe it's of use to the world as an example of ReScript and/or scanning and parsing.

It features tracking of the position of tokens (e.g. character and line number), which is good for pointing out the location of errors.

## Try it out

Prerequisite: Node.js.

1. Clone the repository.

2. Install dependencies with `npm install`.

3. Build with `npm run res:build`, or use `npm run res:dev` to rebuild as you make changes.

4. Check out `Main.res` for an example program using the parser. Run `node ./src/Main.res.js`, which will parse a simple program and output the AST as JSON.

## Grammar

The grammar is loosely based on the parsing chapter in [Crafting Interpreters](https://craftinginterpreters.com/), which is really worth checking out if you're interested in parsing etc.

```
expr = let
let = "let" identifier "=" if "in" expr | if
if = "if" lambda "then" expr "else" expr | lambda
lambda = "|" identifier "|" expr | equality
equality = comparison (("=" | "!=") comparison)*
comparison = term ((">" | ">=" | "<" | "<=") term)*
term = factor (("-" | "+") factor)*
factor = unary (("/" | "*") unary)*
unary = ("-" | "!")* application
application = primary ("(" expr ")")?
primary = identifier | number | "true" | "false" | string | "(" expr ")"
identifier = [a-zA-Z_][a-zA-Z0-9_]*
number = [0-9]+(\.[0-9]*)?
string = <string in double quotes with backslash escapes>
```

## Example Input

```
/*
    This is a
    block comment
*/
// This is a line comment

let x = 7 in
  let y = 8 in

  // Return a lambda with an argument b
  |b| if z = 2 then b <= 2 else m > 11 
```
