primitive OpenBrace
  fun string(): String =>
    "OPEN_BRACE"

primitive CloseBrace
  fun string(): String =>
    "CLOSE_BRACE"

primitive OpenParenthesis
  fun string(): String =>
    "OPEN_PARENTHESIS"

primitive CloseParenthesis
  fun string(): String =>
    "CLOSE_PARENTHESIS"

primitive Semicolon
  fun string(): String =>
    "SEMICOLON"

primitive ReturnKeyword
  fun string(): String =>
    "RETURN_KEYWORD"

primitive IntKeyword
  fun string(): String =>
    "INT_KEYWORD"

type Keyword is ( ReturnKeyword | IntKeyword )

class val Identifier
  let id: String

  new val create(id': String) =>
    id = id'

  fun string(): String =>
    "IDENTIFIER<" + id + ">"

class val IntegerLiteral
  let value: USize

  new val create(value': USize) =>
    value = value'

  fun string(): String =>
    "INTEGER_LITERAL<" + value.string() + ">"

type Token is 
  ( OpenBrace
  | CloseBrace
  | OpenParenthesis
  | CloseParenthesis
  | Semicolon
  | Keyword
  | Identifier
  | IntegerLiteral )

primitive Lexer
  fun apply(code: String): Array[Token] iso^ ? =>
    recover iso
      let token_array = Array[Token]
      var current_token_value: String iso = recover String end
      for char in code.values() do
        match char
        | ' ' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
        | '\t' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
        | '\n' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
        | '\v' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
        | '\f' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
        | '\r' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
        | '{' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
          token_array.push(OpenBrace)
        | '}' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
          token_array.push(CloseBrace)
        | '(' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
          token_array.push(OpenParenthesis)
        | ')' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
          token_array.push(CloseParenthesis)
        | ';' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as Token) end
          token_array.push(Semicolon)
        else
          current_token_value.push(char)
        end
      end
      let token = finalise_token(current_token_value = recover String end)?
      try token_array.push(token as Token) end
      token_array
    end

  fun finalise_token(current_token_value: String): ( Token val | None ) ? =>
    if current_token_value.size() > 0 then
      match current_token_value
      | "return" =>
        return ReturnKeyword
      | "int" =>
        return IntKeyword
      else
        let first_char: U8 = current_token_value(0)?
        let alphabet_char: Bool =
          ((first_char >= 0x41) and (first_char <= 0x5A)) or
          ((first_char >= 0x61) and (first_char <= 0x7A))
        if alphabet_char then
          return Identifier(current_token_value)
        elseif ((first_char >= 0x30) and (first_char <= 0x5A)) then
          return IntegerLiteral(current_token_value.usize()?)
        else error end
      end
    end
    None
