primitive Generator
  fun apply(program: ParserProgram): String =>
    let function = generate_function(program.function)
    function

  fun generate_function(function: ParserFunction): String =>
    let statement = generate_statement(function.statement)
    "\t.globl _" + function.id + "\n" +
    "_" + function.id + ":\n" +
    statement

  fun generate_statement(statement: ParserStatement): String =>
    match statement
    | let s: ParserReturn =>
      generate_exp(s.exp) +
      "\tret\n" 
    end

  fun generate_exp(exp: ParserExp): String =>
    match exp
    | let e: ParserConst =>
      "\tmovl\t$" + e.int.string() + ", %eax\n"
    | let e: ParserUnaryOP =>
      generate_unary_op(e)
    end

  fun generate_unary_op(unary_op: ParserUnaryOP): String =>
    match unary_op.op
    | let o: ParserNegation =>
      generate_exp(unary_op.exp) +
      "\tneg\t%eax\n"
    | let o: ParserBitwiseComplement =>
      generate_exp(unary_op.exp) +
      "\tnot\t%eax\n"
    | let o: ParserLogicalNegation =>
      generate_exp(unary_op.exp) +
      "\tcmpl\t$0, %eax\n" +
      "\tmovl\t$0, %eax\n" +
      "\tsete\t%al\n"
    end
