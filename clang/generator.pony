primitive Generator
  fun apply(program: ParserProgram): String =>
    let function = generate_function(program.function)
    function

  fun generate_function(function: ParserFunction): String =>
    let statement = generate_statement(function.statement)
    if Platform.osx() then
      "\t.globl _" + function.id + "\n" +
      "_" + function.id + ":\n"
    else
      "\t.globl " + function.id + "\n" +
      function.id + ":\n"
    end +
    statement

  fun generate_statement(statement: ParserStatement): String =>
    match statement
    | let s: ParserReturn =>
      generate_exp(s.exp) +
      "\tret\n" 
    end

  fun generate_exp(exp: ParserExp): String =>
    match exp
    | let e: ParserExpBinaryOP =>
      match e.term1
      | let t: ParserExpBinaryOP =>
        generate_exp(t)
      | let t: ParserTerm =>
        generate_term(t)
      end +
      "\tpush\t%eax\n" +
      generate_term(e.term2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserAddition =>
        "\taddl\t%ecx, %eax\n"
      | ParserSubtraction =>
        "\txchg\t%ecx, %eax\n" +
        "\tsubl\t%ecx, %eax\n"
      end
    | let e: ParserTerm =>
      generate_term(e)
    end

  fun generate_term(term: ParserTerm): String =>
    match term
    | let t: ParserTermBinaryOP =>
      match t.factor1
      | let f: ParserTermBinaryOP =>
        generate_term(f)
      | let f: ParserFactor =>
        generate_factor(f)
      end +
      "\tpush\t%eax\n" +
      generate_factor(t.factor2) +
      "\tpop\t%ecx\n" +
      match t.op
      | ParserMultiplication =>
        "\timul\t%ecx, %eax\n"
      | ParserDivision =>
        "\txchg\t%ecx, %eax\n" +
        "\tmovl\t$0, %edx\n" +
        "\tidivl\t%ecx\n"
      end
    | let t: ParserFactor =>
      generate_factor(t)
    end

  fun generate_factor(factor: ParserFactor): String =>
    match factor
    | let f: ParserFactorExp =>
      generate_exp(f.exp)
    | let f: ParserConst =>
      "\tmovl\t$" + f.int.string() + ", %eax\n"
    | let f: ParserUnaryOP =>
      generate_unary_op(f)
    end

  fun generate_unary_op(unary_op: ParserUnaryOP): String =>
    match unary_op.op
    | let o: ParserNegation =>
      generate_factor(unary_op.factor) +
      "\tneg\t%eax\n"
    | let o: ParserBitwiseComplement =>
      generate_factor(unary_op.factor) +
      "\tnot\t%eax\n"
    | let o: ParserLogicalNegation =>
      generate_factor(unary_op.factor) +
      "\tcmpl\t$0, %eax\n" +
      "\tmovl\t$0, %eax\n" +
      "\tsete\t%al\n"
    end
