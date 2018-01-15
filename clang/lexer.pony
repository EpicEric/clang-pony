primitive LexerOpenBrace
  fun string(): String =>
    "OPEN_BRACE"

primitive LexerCloseBrace
  fun string(): String =>
    "CLOSE_BRACE"

primitive LexerOpenParenthesis
  fun string(): String =>
    "OPEN_PARENTHESIS"

primitive LexerCloseParenthesis
  fun string(): String =>
    "CLOSE_PARENTHESIS"

primitive LexerSemicolon
  fun string(): String =>
    "SEMICOLON"

primitive LexerNegation
  fun string(): String =>
    "NEGATION"

primitive LexerBitwiseComplement
  fun string(): String =>
    "BITWISE_COMPLEMENT"

primitive LexerLogicalNegation
  fun string(): String =>
    "LOGICAL_NEGATION"

type LexerUnaryOP is
  ( LexerNegation
  | LexerBitwiseComplement
  | LexerLogicalNegation )

primitive LexerAddition
  fun string(): String =>
    "ADDITION"

primitive LexerMultiplication
  fun string(): String =>
    "MULTIPLICATION"

primitive LexerDivision
  fun string(): String =>
    "DIVISION"

primitive LexerLogicalAnd
  fun string(): String =>
    "LOGICAL_AND"

primitive LexerLogicalOr
  fun string(): String =>
    "LOGICAL_OR"

primitive LexerEqualTo
  fun string(): String =>
    "EQUAL_TO"

primitive LexerNotEqualTo
  fun string(): String =>
    "NOT_EQUAL_TO"

primitive LexerLessThan
  fun string(): String =>
    "LESS_THAN"

primitive LexerLessThanOrEqualTo
  fun string(): String =>
    "LESS_THAN_OR_EQUAL_TO"

primitive LexerGreaterThan
  fun string(): String =>
    "GREATER_THAN"

primitive LexerGreaterThanOrEqualTo
  fun string(): String =>
    "GREATER_THAN_OR_EQUAL_TO"

type LexerBinaryOP is
  ( LexerAddition
  | LexerNegation
  | LexerMultiplication
  | LexerDivision
  | LexerLogicalAnd
  | LexerLogicalOr
  | LexerEqualTo
  | LexerNotEqualTo
  | LexerLessThan
  | LexerLessThanOrEqualTo
  | LexerGreaterThan
  | LexerGreaterThanOrEqualTo )

primitive LexerReturnKeyword
  fun string(): String =>
    "RETURN_KEYWORD"

primitive LexerIntKeyword
  fun string(): String =>
    "INT_KEYWORD"

type LexerKeyword is ( LexerReturnKeyword | LexerIntKeyword )

class val LexerIdentifier
  let id: String

  new val create(id': String) =>
    id = id'

  fun string(): String =>
    "IDENTIFIER<" + id + ">"

class val LexerIntegerLiteral
  let value: USize

  new val create(value': USize) =>
    value = value'

  fun string(): String =>
    "INTEGER_LITERAL<" + value.string() + ">"

type LexerToken is 
  ( LexerOpenBrace
  | LexerCloseBrace
  | LexerOpenParenthesis
  | LexerCloseParenthesis
  | LexerSemicolon
  | LexerUnaryOP
  | LexerBinaryOP
  | LexerKeyword
  | LexerIdentifier
  | LexerIntegerLiteral )

primitive Lexer
  fun apply(code: String): Array[LexerToken] iso^ ? =>
    recover iso
      let token_array = Array[LexerToken]
      var current_token_value: String iso = recover String end
      for char in code.values() do
        match char
        | ' ' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
        | '\t' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
        | '\n' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
        | '\v' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
        | '\f' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
        | '\r' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
        | '{' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerOpenBrace)
        | '}' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerCloseBrace)
        | '(' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerOpenParenthesis)
        | ')' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerCloseParenthesis)
        | ';' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerSemicolon)
        | '-' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerNegation)
        | '~' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerBitwiseComplement)
        | '!' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          current_token_value.push(char)
        | '+' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerAddition)
        | '*' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerMultiplication)
        | '/' =>
          let token = finalise_token(current_token_value = recover String end)?
          try token_array.push(token as LexerToken) end
          token_array.push(LexerDivision)
        | '&' =>
          if current_token_value != "&" then
            let token = finalise_token(current_token_value = recover String end)?
            try token_array.push(token as LexerToken) end
          end
          current_token_value.push(char)
        | '|' =>
          if current_token_value != "|" then
            let token = finalise_token(current_token_value = recover String end)?
            try token_array.push(token as LexerToken) end
          end
          current_token_value.push(char)
        | '=' =>
          if (current_token_value != "=") and (current_token_value != "!") and
            (current_token_value != "<") and (current_token_value != ">") then
            let token = finalise_token(current_token_value = recover String end)?
            try token_array.push(token as LexerToken) end
          end
          current_token_value.push(char)
        | '<' =>
          if current_token_value != "" then
            let token = finalise_token(current_token_value = recover String end)?
            try token_array.push(token as LexerToken) end
          end
          current_token_value.push(char)
        | '>' =>
          if current_token_value != "" then
            let token = finalise_token(current_token_value = recover String end)?
            try token_array.push(token as LexerToken) end
          end
          current_token_value.push(char)
        else
          if current_token_value == "!" then
            let token = finalise_token(current_token_value = recover String end)?
            try token_array.push(token as LexerToken) end
          end
          current_token_value.push(char)
        end
      end
      let token = finalise_token(current_token_value = recover String end)?
      try token_array.push(token as LexerToken) end
      token_array
    end

  fun finalise_token(current_token_value: String): ( LexerToken val | None ) ? =>
    if current_token_value.size() > 0 then
      match current_token_value
      | "return" =>
        return LexerReturnKeyword
      | "int" =>
        return LexerIntKeyword
      | "!" =>
        return LexerLogicalNegation
      | "&&" =>
        return LexerLogicalAnd
      | "||" =>
        return LexerLogicalOr
      | "==" =>
        return LexerEqualTo
      | "!=" =>
        return LexerNotEqualTo
      | "<" =>
        return LexerLessThan
      | "<=" =>
        return LexerLessThanOrEqualTo
      | ">" =>
        return LexerGreaterThan
      | ">=" =>
        return LexerGreaterThanOrEqualTo
      else
        let first_char: U8 = current_token_value(0)?
        let alphabet_char: Bool =
          ((first_char >= 0x41) and (first_char <= 0x5A)) or
          ((first_char >= 0x61) and (first_char <= 0x7A))
        if alphabet_char then
          return LexerIdentifier(current_token_value)
        elseif ((first_char >= 0x30) and (first_char <= 0x5A)) then
          return LexerIntegerLiteral(current_token_value.usize()?)
        else error end
      end
    end
    None
