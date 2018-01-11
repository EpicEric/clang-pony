class val ParserProgram
  let function: ParserFunction

  new val create(function': ParserFunction) =>
    function = function'

class val ParserFunction
  let id: String
  let statement: ParserStatement

  new val create(id': String, statement': ParserStatement) =>
    id = id'
    statement = statement'

class val ParserReturn
  let exp: ParserExp

  new val create(exp': ParserExp) =>
    exp = exp'

type ParserStatement is
  ( ParserReturn )

class val ParserConst
  let int: USize

  new val create(int': USize) =>
    int = int'

type ParserExp is
  ( ParserConst )

class val ParserID
  let id: String

  new val create(id': String) =>
    id = id'

type ParserRule is
  ( ParserProgram
  | ParserFunction
  | ParserStatement
  | ParserExp
  | ParserID )

primitive Parser
  fun apply(token_array: Array[LexerToken] val)
    : ParserProgram ?
  =>
    var curr_array = token_array
    (let program, curr_array) = parse_program(curr_array)?
    if curr_array.size() > 0 then error end
    program

  fun parse_program(token_array: Array[LexerToken] val)
    : ( ParserProgram, Array[LexerToken] val ) ?
  =>
    """
    <program> ::= <function>
    """
    var curr_array = token_array
    (let function, curr_array) = parse_function(curr_array)?
    let program = ParserProgram(function)
    (program, curr_array)

  fun parse_function(token_array: Array[LexerToken] val)
    : ( ParserFunction, Array[LexerToken] val ) ?
  =>
    """
    <function> ::= "int" <id> "(" ")" "{" <statement> "}"
    """
    var curr_array = token_array
    match curr_array(0)?
    | let a: LexerIntKeyword =>
      curr_array = curr_array.trim(1)
      (let identifier, curr_array) = parse_id(curr_array)?
      match curr_array(0)?
      | let b: LexerOpenParenthesis =>
        curr_array = curr_array.trim(1)
        match curr_array(0)?
        | let c: LexerCloseParenthesis =>
          curr_array = curr_array.trim(1)
          match curr_array(0)?
          | let d: LexerOpenBrace =>
            curr_array = curr_array.trim(1)
            (let statement, curr_array) = parse_statement(curr_array)?
            match curr_array(0)?
            | let e: LexerCloseBrace =>
              curr_array = curr_array.trim(1)
              let function = ParserFunction(identifier.id, statement)
              return (function, curr_array)
            end
          end
        end
      end
    end
    error

  fun parse_statement(token_array: Array[LexerToken] val)
    : ( ParserStatement, Array[LexerToken] val ) ?
  =>
    """
    <statement> ::= "return" <exp> ";"
    """
    var curr_array = token_array
    match curr_array(0)?
    | let a: LexerReturnKeyword =>
      curr_array = curr_array.trim(1)
      (let exp, curr_array) = parse_exp(curr_array)?
      match curr_array(0)?
      | let b: LexerSemicolon =>
        curr_array = curr_array.trim(1)
        let statement = ParserReturn(exp)
        return (statement, curr_array)
      end
    end
    error

  fun parse_exp(token_array: Array[LexerToken] val)
    : ( ParserExp, Array[LexerToken] val ) ?
  =>
    """
    <exp> ::= INTEGER
    """
    var curr_array = token_array
    match curr_array(0)?
    | let a: LexerIntegerLiteral =>
      curr_array = curr_array.trim(1)
      let exp = ParserConst(a.value)
      return (exp, curr_array)
    end
    error

  fun parse_id(token_array: Array[LexerToken] val)
    : ( ParserID, Array[LexerToken] val ) ?
  =>
    """
    <id> ::= IDENTIFIER
    """
    var curr_array = token_array
    match curr_array(0)?
    | let a: LexerIdentifier =>
      curr_array = curr_array.trim(1)
      let id = ParserID(a.id)
      return (id, curr_array)
    end
    error
