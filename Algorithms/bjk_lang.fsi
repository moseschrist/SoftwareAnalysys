// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | PROG
  | LBRACE
  | RBRACE
  | SKIP
  | ASSIGN
  | VAR of (int)
  | OR
  | CHOOSE
  | ELSE
  | IF
  | LOOP
  | INT of (int)
  | PLUS
  | TIMES
  | RPAREN
  | LPAREN
  | SEMI
type tokenId = 
    | TOKEN_EOF
    | TOKEN_PROG
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_SKIP
    | TOKEN_ASSIGN
    | TOKEN_VAR
    | TOKEN_OR
    | TOKEN_CHOOSE
    | TOKEN_ELSE
    | TOKEN_IF
    | TOKEN_LOOP
    | TOKEN_INT
    | TOKEN_PLUS
    | TOKEN_TIMES
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_SEMI
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startPROG
    | NONTERM_PROG
    | NONTERM_COMMAND
    | NONTERM_EXP
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val PROG : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Core.Cmd) 
