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

primitive ParserAddition
primitive ParserSubtraction

type ParserExpBinaryOperator is
  ( ParserAddition
  | ParserSubtraction )

class val ParserExpBinaryOP
  let op: ParserExpBinaryOperator
  let term1: (ParserTerm | ParserExpBinaryOP)
  let term2: ParserTerm

  new val create(
    op': ParserExpBinaryOperator,
    term1': (ParserTerm | ParserExpBinaryOP),
    term2': ParserTerm)
  =>
    op = op'
    term1 = term1'
    term2 = term2'

type ParserExp is
  ( ParserExpBinaryOP
  | ParserTerm )

primitive ParserMultiplication
primitive ParserDivision

type ParserTermBinaryOperator is
  ( ParserMultiplication
  | ParserDivision )

class val ParserTermBinaryOP
  let op: ParserTermBinaryOperator
  let factor1: (ParserFactor | ParserTermBinaryOP)
  let factor2: ParserFactor

  new val create(
    op': ParserTermBinaryOperator,
    factor1': (ParserFactor | ParserTermBinaryOP),
    factor2': ParserFactor)
  =>
    op = op'
    factor1 = factor1'
    factor2 = factor2'

type ParserTerm is
  ( ParserTermBinaryOP
  | ParserFactor )

type ParserBinaryOperator is
  ( ParserExpBinaryOperator
  | ParserTermBinaryOperator )

class val ParserConst
  let int: USize

  new val create(int': USize) =>
    int = int'

primitive ParserNegation
primitive ParserBitwiseComplement
primitive ParserLogicalNegation

type ParserUnaryOperator is
  ( ParserNegation
  | ParserBitwiseComplement
  | ParserLogicalNegation )

class val ParserUnaryOP
  let op: ParserUnaryOperator
  let factor: ParserFactor

  new val create(op': ParserUnaryOperator, factor': ParserFactor) =>
    op = op'
    factor = factor'

class val ParserFactorExp
  let exp: ParserExp

  new val create(exp': ParserExp) =>
    exp = exp'

type ParserFactor is
  ( ParserFactorExp
  | ParserConst
  | ParserUnaryOP )

class val ParserID
  let id: String

  new val create(id': String) =>
    id = id'

type ParserRule is
  ( ParserProgram
  | ParserFunction
  | ParserStatement
  | ParserExp
  | ParserTerm
  | ParserFactor
  | ParserBinaryOperator
  | ParserUnaryOperator
  | ParserID )

primitive Parser
  fun apply(token_array: Array[LexerToken] val)
    : ParserProgram ?
  =>
    var curr_array = token_array
    (let program, curr_array) = parse_program(curr_array)?
    if curr_array.size() > 0 then error end
    program

  fun print_ast(rule: ParserRule, level: USize = 0) : String =>
    match rule
    | let r: ParserProgram =>
      print_level(level) + "PROGRAM\n" +
      print_ast(r.function, level + 1)
    | let r: ParserFunction =>
      print_level(level) + "FUNCTION " + r.id + "\n" +
      print_ast(r.statement, level + 1)
    | let r: ParserReturn =>
      print_level(level) + "RETURN\n" +
      print_ast(r.exp, level + 1)
    | let r: ParserAddition =>
      print_level(level) + "ADDITION\n"
    | let r: ParserSubtraction =>
      print_level(level) + "SUBTRACTION\n"
    | let r: ParserExpBinaryOP =>
      print_level(level) + "EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.term1, level + 1) +
      print_ast(r.term2, level + 1)
    | let r: ParserMultiplication =>
      print_level(level) + "MULTIPLICATION\n"
    | let r: ParserDivision =>
      print_level(level) + "DIVISION\n"
    | let r: ParserTermBinaryOP =>
      print_level(level) + "TERM_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.factor1, level + 1) +
      print_ast(r.factor2, level + 1)
    | let r: ParserFactorExp =>
      print_ast(r.exp, level + 1)
    | let r: ParserConst =>
      print_level(level) + "CONST " + r.int.string() + "\n"
    | let r: ParserNegation =>
      print_level(level) + "NEGATION\n"
    | let r: ParserBitwiseComplement =>
      print_level(level) + "BITWISE_COMPLEMENT\n"
    | let r: ParserLogicalNegation =>
      print_level(level) + "LOGICAL_NEGATION\n"
    | let r: ParserUnaryOP =>
      print_level(level) + "UNARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.factor, level + 1)
    | let r: ParserID =>
      print_level(level) + "ID " + r.id + "\n"
    // else
    //   print_level(level) + "UNKNOWN\n"
    end

  fun print_level(level: USize): String iso^ =>
    if level == 0 then
      return recover String end
    end
    let string: String iso = recover String(level * 2) end
    var i: USize = 0
    while i < (level - 1) do
      string.append("--")
      i = i + 1
    end
    if level > 0 then string.append("- ") end
    consume string

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
    <exp> ::= <term> { ("+" | "-") <term> }
    """
    var curr_array = token_array
    (var exp: ParserExp, curr_array) = parse_term(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerAddition =>
        curr_array = curr_array.trim(1)
        (let next_term, curr_array) = parse_term(curr_array)?
        exp = ParserExpBinaryOP(ParserAddition, exp, next_term)
      | let a: LexerNegation =>
        curr_array = curr_array.trim(1)
        (let next_term, curr_array) = parse_term(curr_array)?
        exp = ParserExpBinaryOP(ParserSubtraction, exp, next_term)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_term(token_array: Array[LexerToken] val)
    : ( ParserTerm, Array[LexerToken] val ) ?
  =>
    """
    <term> ::= <factor> { ("*" | "/") <factor> }
    """
    var curr_array = token_array
    (var term: ParserTerm, curr_array) = parse_factor(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerMultiplication =>
        curr_array = curr_array.trim(1)
        (let next_factor, curr_array) = parse_factor(curr_array)?
        term = ParserTermBinaryOP(ParserMultiplication, term, next_factor)
      | let a: LexerDivision =>
        curr_array = curr_array.trim(1)
        (let next_factor, curr_array) = parse_factor(curr_array)?
        term = ParserTermBinaryOP(ParserDivision, term, next_factor)
      else
        break
      end
    end
    (term, curr_array)

  fun parse_factor(token_array: Array[LexerToken] val)
    : ( ParserFactor, Array[LexerToken] val ) ?
  =>
    """
    <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
    """
    var curr_array = token_array
    match curr_array(0)?
    | let a: LexerOpenParenthesis =>
      curr_array = curr_array.trim(1)
      (let exp, curr_array) = parse_exp(curr_array)?
      match curr_array(0)?
      | let b: LexerCloseParenthesis =>
        curr_array = curr_array.trim(1)
        let factor = ParserFactorExp(exp)
        return (factor, curr_array)
      end
    | let a: LexerUnaryOP =>
      (let unary_op, curr_array) = parse_unary_op(curr_array)?
      (let factor_in, curr_array) = parse_factor(curr_array)?
      let factor = ParserUnaryOP(unary_op, factor_in)
      return (factor, curr_array)
    | let a: LexerIntegerLiteral =>
      curr_array = curr_array.trim(1)
      let factor = ParserConst(a.value)
      return (factor, curr_array)
    end
    error

  fun parse_unary_op(token_array: Array[LexerToken] val)
    : ( ParserUnaryOperator, Array[LexerToken] val ) ?
  =>
    """
    <unary_op> ::= "!" | "~" | "-"
    """
    var curr_array = token_array
    match curr_array(0)?
    | let a: LexerNegation =>
      curr_array = curr_array.trim(1)
      let op = ParserNegation
      return (op, curr_array)
    | let a: LexerBitwiseComplement =>
      curr_array = curr_array.trim(1)
      let op = ParserBitwiseComplement
      return (op, curr_array)
    | let a: LexerLogicalNegation =>
      curr_array = curr_array.trim(1)
      let op = ParserLogicalNegation
      return (op, curr_array)
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
