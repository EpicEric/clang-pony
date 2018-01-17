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

primitive ParserLogicalOr

type ParserExpBinaryOperator is
  ( ParserLogicalOr )

class val ParserExpBinaryOP
  let op: ParserExpBinaryOperator
  let exp1: ParserExp
  let exp2: ParserLogicalAndExp

  new val create(
    op': ParserExpBinaryOperator,
    exp1': ParserExp,
    exp2': ParserLogicalAndExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserExp is
  ( ParserExpBinaryOP
  | ParserLogicalAndExp )

primitive ParserLogicalAnd

type ParserLogicalAndExpBinaryOperator is
  ( ParserLogicalAnd )

class val ParserLogicalAndExpBinaryOP
  let op: ParserLogicalAndExpBinaryOperator
  let exp1: ParserLogicalAndExp
  let exp2: ParserBitwiseOrExp

  new val create(
    op': ParserLogicalAndExpBinaryOperator,
    exp1': ParserLogicalAndExp,
    exp2': ParserBitwiseOrExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserLogicalAndExp is
  ( ParserLogicalAndExpBinaryOP
  | ParserBitwiseOrExp )

primitive ParserBitwiseOr

type ParserBitwiseOrExpBinaryOperator is
  ( ParserBitwiseOr )

class val ParserBitwiseOrExpBinaryOP
  let op: ParserBitwiseOrExpBinaryOperator
  let exp1: ParserBitwiseOrExp
  let exp2: ParserBitwiseXorExp

  new val create(
    op': ParserBitwiseOrExpBinaryOperator,
    exp1': ParserBitwiseOrExp,
    exp2': ParserBitwiseXorExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserBitwiseOrExp is
  ( ParserBitwiseOrExpBinaryOP
  | ParserBitwiseXorExp )

primitive ParserBitwiseXor

type ParserBitwiseXorExpBinaryOperator is
  ( ParserBitwiseXor )

class val ParserBitwiseXorExpBinaryOP
  let op: ParserBitwiseXorExpBinaryOperator
  let exp1: ParserBitwiseXorExp
  let exp2: ParserBitwiseAndExp

  new val create(
    op': ParserBitwiseXorExpBinaryOperator,
    exp1': ParserBitwiseXorExp,
    exp2': ParserBitwiseAndExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserBitwiseXorExp is
  ( ParserBitwiseXorExpBinaryOP
  | ParserBitwiseAndExp )

primitive ParserBitwiseAnd

type ParserBitwiseAndExpBinaryOperator is
  ( ParserBitwiseAnd )

class val ParserBitwiseAndExpBinaryOP
  let op: ParserBitwiseAndExpBinaryOperator
  let exp1: ParserBitwiseAndExp
  let exp2: ParserEqualityExp

  new val create(
    op': ParserBitwiseAndExpBinaryOperator,
    exp1': ParserBitwiseAndExp,
    exp2': ParserEqualityExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserBitwiseAndExp is
  ( ParserBitwiseAndExpBinaryOP
  | ParserEqualityExp )

primitive ParserEqualTo
primitive ParserNotEqualTo

type ParserEqualityExpBinaryOperator is
  ( ParserEqualTo
  | ParserNotEqualTo )

class val ParserEqualityExpBinaryOP
  let op: ParserEqualityExpBinaryOperator
  let exp1: ParserEqualityExp
  let exp2: ParserRelationalExp

  new val create(
    op': ParserEqualityExpBinaryOperator,
    exp1': ParserEqualityExp,
    exp2': ParserRelationalExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserEqualityExp is
  ( ParserEqualityExpBinaryOP
  | ParserRelationalExp )

primitive ParserLessThan
primitive ParserLessThanOrEqualTo
primitive ParserGreaterThan
primitive ParserGreaterThanOrEqualTo

type ParserRelationalExpBinaryOperator is
  ( ParserLessThan
  | ParserLessThanOrEqualTo
  | ParserGreaterThan
  | ParserGreaterThanOrEqualTo )

class val ParserRelationalExpBinaryOP
  let op: ParserRelationalExpBinaryOperator
  let exp1: ParserRelationalExp
  let exp2: ParserBitwiseShiftExp

  new val create(
    op': ParserRelationalExpBinaryOperator,
    exp1': ParserRelationalExp,
    exp2': ParserBitwiseShiftExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserRelationalExp is
  ( ParserRelationalExpBinaryOP
  | ParserBitwiseShiftExp )

primitive ParserBitwiseShiftLeft
primitive ParserBitwiseShiftRight

type ParserBitwiseShiftExpBinaryOperator is
  ( ParserBitwiseShiftLeft
  | ParserBitwiseShiftRight )

class val ParserBitwiseShiftExpBinaryOP
  let op: ParserBitwiseShiftExpBinaryOperator
  let exp1: ParserBitwiseShiftExp
  let exp2: ParserAdditiveExp

  new val create(
    op': ParserBitwiseShiftExpBinaryOperator,
    exp1': ParserBitwiseShiftExp,
    exp2': ParserAdditiveExp)
  =>
    op = op'
    exp1 = exp1'
    exp2 = exp2'

type ParserBitwiseShiftExp is
  ( ParserBitwiseShiftExpBinaryOP
  | ParserAdditiveExp )

primitive ParserAddition
primitive ParserSubtraction

type ParserAdditiveExpBinaryOperator is
  ( ParserAddition
  | ParserSubtraction )

class val ParserAdditiveExpBinaryOP
  let op: ParserAdditiveExpBinaryOperator
  let term1: ParserAdditiveExp
  let term2: ParserTerm

  new val create(
    op': ParserAdditiveExpBinaryOperator,
    term1': ParserAdditiveExp,
    term2': ParserTerm)
  =>
    op = op'
    term1 = term1'
    term2 = term2'

type ParserAdditiveExp is
  ( ParserAdditiveExpBinaryOP
  | ParserTerm )

primitive ParserMultiplication
primitive ParserDivision
primitive ParserModulo

type ParserTermBinaryOperator is
  ( ParserMultiplication
  | ParserDivision
  | ParserModulo )

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
  | ParserLogicalAndExpBinaryOperator
  | ParserBitwiseOrExpBinaryOperator
  | ParserBitwiseXorExpBinaryOperator
  | ParserBitwiseAndExpBinaryOperator
  | ParserEqualityExpBinaryOperator
  | ParserRelationalExpBinaryOperator
  | ParserBitwiseShiftExpBinaryOperator
  | ParserAdditiveExpBinaryOperator
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
  | ParserLogicalAndExp
  | ParserBitwiseOr
  | ParserBitwiseXor
  | ParserBitwiseAnd
  | ParserEqualityExp
  | ParserRelationalExp
  | ParserBitwiseShiftExp
  | ParserAdditiveExp
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
    | let r: ParserExpBinaryOP =>
      print_level(level) + "EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserLogicalAndExpBinaryOP =>
      print_level(level) + "LOGICAL_AND_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserBitwiseOrExpBinaryOP =>
      print_level(level) + "BITWISE_OR_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserBitwiseXorExpBinaryOP =>
      print_level(level) + "BITWISE_XOR_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserBitwiseAndExpBinaryOP =>
      print_level(level) + "BITWISE_AND_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserEqualityExpBinaryOP =>
      print_level(level) + "EQUALITY_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserRelationalExpBinaryOP =>
      print_level(level) + "RELATIONAL_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserBitwiseShiftExpBinaryOP =>
      print_level(level) + "BITWISE_SHIFT_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.exp1, level + 1) +
      print_ast(r.exp2, level + 1)
    | let r: ParserAdditiveExpBinaryOP =>
      print_level(level) + "ADDITIVE_EXP_BINARY_OP\n" +
      print_ast(r.op, level + 1) +
      print_ast(r.term1, level + 1) +
      print_ast(r.term2, level + 1)
    | let r: ParserAddition =>
      print_level(level) + "ADDITION\n"
    | let r: ParserSubtraction =>
      print_level(level) + "SUBTRACTION\n"
    | let r: ParserMultiplication =>
      print_level(level) + "MULTIPLICATION\n"
    | let r: ParserDivision =>
      print_level(level) + "DIVISION\n"
    | let r: ParserLogicalAnd =>
      print_level(level) + "LOGICAL_AND\n"
    | let r: ParserLogicalOr =>
      print_level(level) + "LOGICAL_OR\n"
    | let r: ParserEqualTo =>
      print_level(level) + "EQUAL_TO\n"
    | let r: ParserNotEqualTo =>
      print_level(level) + "NOT_EQUAL_TO\n"
    | let r: ParserLessThan =>
      print_level(level) + "LESS_THAN\n"
    | let r: ParserLessThanOrEqualTo =>
      print_level(level) + "LESS_THAN_OR_EQUAL_TO\n"
    | let r: ParserGreaterThan =>
      print_level(level) + "GREATER_THAN\n"
    | let r: ParserGreaterThanOrEqualTo =>
      print_level(level) + "GREATER_THAN_OR_EQUAL_TO\n"
    | let r: ParserModulo =>
      print_level(level) + "MODULO\n"
    | let r: ParserBitwiseAnd =>
      print_level(level) + "BITWISE_AND\n"
    | let r: ParserBitwiseXor =>
      print_level(level) + "BITWISE_XOR\n"
    | let r: ParserBitwiseOr=>
      print_level(level) + "BITWISE_OR\n"
    | let r: ParserBitwiseShiftLeft =>
      print_level(level) + "BITWISE_SHIFT_LEFT\n"
    | let r: ParserBitwiseShiftRight =>
      print_level(level) + "BITWISE_SHIFT_RIGHT\n"
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
    <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
    """
    var curr_array = token_array
    (var exp: ParserExp, curr_array) = parse_logical_and_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerLogicalOr =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_logical_and_exp(curr_array)?
        exp = ParserExpBinaryOP(ParserLogicalOr, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_logical_and_exp(token_array: Array[LexerToken] val)
    : ( ParserLogicalAndExp, Array[LexerToken] val ) ?
  =>
    """
    <logical-and-exp> ::= <bitwise-or-exp> { "&&" <bitwise-or-exp> }
    """
    var curr_array = token_array
    (var exp: ParserLogicalAndExp, curr_array) = parse_bitwise_or_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerLogicalAnd =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_or_exp(curr_array)?
        exp = ParserLogicalAndExpBinaryOP(ParserLogicalAnd, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_bitwise_or_exp(token_array: Array[LexerToken] val)
    : ( ParserBitwiseOrExp, Array[LexerToken] val ) ?
  =>
    """
    <bitwise-or-exp> ::= <bitwise-xor-exp> { "|" <bitwise-xor-exp> }
    """
    var curr_array = token_array
    (var exp: ParserBitwiseOrExp, curr_array) = parse_bitwise_xor_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerBitwiseOr =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_xor_exp(curr_array)?
        exp = ParserBitwiseOrExpBinaryOP(ParserBitwiseOr, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_bitwise_xor_exp(token_array: Array[LexerToken] val)
    : ( ParserBitwiseXorExp, Array[LexerToken] val ) ?
  =>
    """
    <bitwise-xor-exp> ::= <bitwise-and-exp> { "^" <bitwise-and-exp> }
    """
    var curr_array = token_array
    (var exp: ParserBitwiseXorExp, curr_array) = parse_bitwise_and_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerBitwiseXor =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_and_exp(curr_array)?
        exp = ParserBitwiseXorExpBinaryOP(ParserBitwiseXor, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_bitwise_and_exp(token_array: Array[LexerToken] val)
    : ( ParserBitwiseAndExp, Array[LexerToken] val ) ?
  =>
    """
    <bitwise-and-exp> ::= <equality-exp> { "&" <equality-exp> }
    """
    var curr_array = token_array
    (var exp: ParserBitwiseAndExp, curr_array) = parse_equality_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerBitwiseAnd =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_equality_exp(curr_array)?
        exp = ParserBitwiseAndExpBinaryOP(ParserBitwiseAnd, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_equality_exp(token_array: Array[LexerToken] val)
    : ( ParserEqualityExp, Array[LexerToken] val ) ?
  =>
    """
    <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
    """
    var curr_array = token_array
    (var exp: ParserEqualityExp, curr_array) = parse_relational_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerEqualTo =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_relational_exp(curr_array)?
        exp = ParserEqualityExpBinaryOP(ParserEqualTo, exp, next_exp)
      | let a: LexerNotEqualTo =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_relational_exp(curr_array)?
        exp = ParserEqualityExpBinaryOP(ParserNotEqualTo, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_relational_exp(token_array: Array[LexerToken] val)
    : ( ParserRelationalExp, Array[LexerToken] val ) ?
  =>
    """
    <relational-exp> ::= <shift-exp> { ("<" | ">" | "<=" | ">=") <shift-exp> }
    """
    var curr_array = token_array
    (var exp: ParserRelationalExp, curr_array) = parse_bitwise_shift_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerLessThan =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_shift_exp(curr_array)?
        exp = ParserRelationalExpBinaryOP(ParserLessThan, exp, next_exp)
      | let a: LexerLessThanOrEqualTo =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_shift_exp(curr_array)?
        exp = ParserRelationalExpBinaryOP(ParserLessThanOrEqualTo, exp, next_exp)
      | let a: LexerGreaterThan =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_shift_exp(curr_array)?
        exp = ParserRelationalExpBinaryOP(ParserGreaterThan, exp, next_exp)
      | let a: LexerGreaterThanOrEqualTo =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_bitwise_shift_exp(curr_array)?
        exp = ParserRelationalExpBinaryOP(ParserGreaterThanOrEqualTo, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_bitwise_shift_exp(token_array: Array[LexerToken] val)
    : ( ParserBitwiseShiftExp, Array[LexerToken] val ) ?
  =>
    """
    <shift-exp> ::= <additive-exp> { ("<<" | ">>") <additive-exp> }
    """
    var curr_array = token_array
    (var exp: ParserBitwiseShiftExp, curr_array) = parse_additive_exp(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerBitwiseShiftLeft =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_additive_exp(curr_array)?
        exp = ParserBitwiseShiftExpBinaryOP(ParserBitwiseShiftLeft, exp, next_exp)
      | let a: LexerBitwiseShiftRight =>
        curr_array = curr_array.trim(1)
        (let next_exp, curr_array) = parse_additive_exp(curr_array)?
        exp = ParserBitwiseShiftExpBinaryOP(ParserBitwiseShiftRight, exp, next_exp)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_additive_exp(token_array: Array[LexerToken] val)
    : ( ParserAdditiveExp, Array[LexerToken] val ) ?
  =>
    """
    <additive-exp> ::= <term> { ("+" | "-") <term> }
    """
    var curr_array = token_array
    (var exp: ParserAdditiveExp, curr_array) = parse_term(curr_array)?
    while true do
      match curr_array(0)?
      | let a: LexerAddition =>
        curr_array = curr_array.trim(1)
        (let next_term, curr_array) = parse_term(curr_array)?
        exp = ParserAdditiveExpBinaryOP(ParserAddition, exp, next_term)
      | let a: LexerNegation =>
        curr_array = curr_array.trim(1)
        (let next_term, curr_array) = parse_term(curr_array)?
        exp = ParserAdditiveExpBinaryOP(ParserSubtraction, exp, next_term)
      else
        break
      end
    end
    (exp, curr_array)

  fun parse_term(token_array: Array[LexerToken] val)
    : ( ParserTerm, Array[LexerToken] val ) ?
  =>
    """
    <term> ::= <factor> { ("*" | "/" | "%") <factor> }
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
      | let a: LexerModulo =>
        curr_array = curr_array.trim(1)
        (let next_factor, curr_array) = parse_factor(curr_array)?
        term = ParserTermBinaryOP(ParserModulo, term, next_factor)
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
