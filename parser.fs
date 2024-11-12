//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Nathen Priyonggo
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)


  //
  // beginswith
  //
  let beginswith (pattern: string) (literal: string) =
    // checks if a string starts with a given pattern.
    literal.StartsWith(pattern)


  //
  // simpleC
  //
  let rec private simpleC tokens = 
    // parses the top-level structure of a SimpleC
    // program.
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9


  //
  // stmts
  //
  and private stmts tokens = 
    // parses a sequence of statements.
    let T2 = stmt tokens
    morestmts T2

  
  //
  // morestmts
  //
  and private morestmts tokens = 
    // handles additional statements or 
    // an end token.
    match tokens with
    | "}" :: _ -> tokens
    | [] -> tokens
    | _ -> 
      let T2 = stmt tokens
      morestmts T2


  //
  // stmt
  //
  and private stmt tokens = 
    // parses an individual statement.
    match tokens with
    | [] -> failwith ("unexpected end of tokens")
    | ";" :: _ -> empty tokens
    | "int" :: _ -> vardecl tokens
    | "cin" :: _ -> input tokens
    | "cout" :: _ -> output tokens
    | "if" :: _ -> ifstmt tokens
    | token :: _ when beginswith "identifier" token -> assignment tokens
    | token :: _ -> failwith ("expecting statement, but found " + token)
    

  //
  // empty
  //
  and private empty tokens = 
    // parses an empty statement.
    matchToken ";" tokens
  

  //
  // vardecl
  //
  and private vardecl tokens =
    // parses a variable declaration.
    let T2 = matchToken "int" tokens
    let token = List.head T2
    if beginswith "identifier" token then  
      let T3 = List.tail T2
      matchToken ";" T3
    else
      failwith ("expecting identifier, but found " + token)


  // 
  // input
  //
  and private input tokens = 
    // parses an input statement.
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let token = List.head T3
    if beginswith "identifier" token then  
      let T4 = List.tail T3
      matchToken ";" T4
    else
      failwith ("expecting identifier, but found " + token)


  //
  // output
  //
  and private output tokens = 
    // parses an output statement.
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    matchToken ";" T4

  
  //
  // output_value
  //
  and private output_value tokens = 
    // parses an output value (literal or expr).
    match tokens with
    | "endl" :: _ -> matchToken "endl" tokens
    | _ -> expr_value tokens


  // 
  // assignment
  //
  and private assignment tokens = 
    // parses an assignment statement.
    let token= List.head tokens
    if beginswith "identifier" token then 
      let T2 = List.tail tokens
      let T3 = matchToken "=" T2
      let T4 = expr T3
      matchToken ";" T4
    else
      failwith ("expecting identifier, but found " + token)


  // 
  // ifstmt
  //
  and private ifstmt tokens =
    // parses an if statement.
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let T4 = condition T3
    let T5 = matchToken ")" T4
    let T6 = then_part T5
    else_part T6


  // 
  // condition
  //
  and private condition tokens =
    // parses a condition expression.
    expr tokens


  // 
  // then_part
  //
  and private then_part tokens =
    // parses the 'then' part of an if statement.
    stmt tokens


  // 
  // else_part
  //
  and private else_part tokens =
    // parses the 'else' part of an if statement if present.
    match tokens with
    | "else" :: _ ->
      let T2 = matchToken "else" tokens
      stmt T2
    | _ -> tokens


  // 
  // expr
  //
  and private expr tokens =
    // parses an expression.
    let T2 = expr_value tokens
    match T2 with
    | [] -> failwith ("unexpected end of tokens")
    | ("+" | "-" | "*" | "/" | "^" | "<" | "<=" | ">" | ">=" | "==" | "!=") :: rest ->
      let T3 = expr_op T2
      expr_value T3
    | _ -> T2


  //
  // expr_value
  //
  and private expr_value tokens =
    // parses an expression value.
    match tokens with
    | token :: rest when beginswith "identifier" token -> rest
    | token :: rest when beginswith "int_literal" token -> rest
    | token :: rest when beginswith "str_literal" token -> rest
    | "true" :: rest -> rest
    | "false" :: rest -> rest
    | token :: _ -> failwith ("expecting identifier or literal, but found " + token)
    | [] -> failwith ("unexpected end of tokens")


  //
  // expr_op
  //
  and private expr_op tokens =
    // parses an expression operator.
    match tokens with 
    | [] -> failwith ("unexpected end of tokens")
    | "+" :: _ -> matchToken "+" tokens
    | "-" :: _ -> matchToken "-" tokens
    | "*" :: _ -> matchToken "*" tokens
    | "/" :: _ -> matchToken "/" tokens
    | "^" :: _ -> matchToken "^" tokens
    | "<" :: _ -> matchToken "<" tokens
    | "<=" :: _ -> matchToken "<=" tokens
    | ">" :: _ -> matchToken ">" tokens
    | ">=" :: _ -> matchToken ">=" tokens
    | "==" :: _ -> matchToken "==" tokens
    | "!=" :: _ -> matchToken "!=" tokens
    | token :: _ -> failwith ("expecting expression operator, but found " + token )
  

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message
