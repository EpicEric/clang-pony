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
      match e.exp1
      | let t: ParserExpBinaryOP =>
        generate_exp(t)
      | let t: ParserLogicalAndExp =>
        generate_logical_and_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_logical_and_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserLogicalOr =>
        "\torl\t%ecx, %eax\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetne\t%al\n"
      end
    | let e: ParserLogicalAndExp =>
      generate_logical_and_exp(e)
    end

  fun generate_logical_and_exp(exp: ParserLogicalAndExp): String =>
    match exp
    | let e: ParserLogicalAndExpBinaryOP =>
      match e.exp1
      | let t: ParserLogicalAndExpBinaryOP =>
        generate_logical_and_exp(t)
      | let t: ParserBitwiseOrExp =>
        generate_bitwise_or_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_bitwise_or_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserLogicalAnd =>
        "\tcmpl\t$0, %ecx\n" +
        "\tsetne\t%cl\n" +
        "\tcmpl\t$0, %eax\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetne\t%al\n" +
        "\tandb\t%cl, %al\n"
      end
    | let e: ParserBitwiseOrExp =>
      generate_bitwise_or_exp(e)
    end

  fun generate_bitwise_or_exp(exp: ParserBitwiseOrExp): String =>
    match exp
    | let e: ParserBitwiseOrExpBinaryOP =>
      match e.exp1
      | let t: ParserBitwiseOrExpBinaryOP =>
        generate_bitwise_or_exp(t)
      | let t: ParserBitwiseXorExp =>
        generate_bitwise_xor_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_bitwise_xor_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserBitwiseOr =>
        "\torl\t%ecx, %eax\n"
      end
    | let e: ParserBitwiseXorExp =>
      generate_bitwise_xor_exp(e)
    end

  fun generate_bitwise_xor_exp(exp: ParserBitwiseXorExp): String =>
    match exp
    | let e: ParserBitwiseXorExpBinaryOP =>
      match e.exp1
      | let t: ParserBitwiseXorExpBinaryOP =>
        generate_bitwise_xor_exp(t)
      | let t: ParserBitwiseAndExp =>
        generate_bitwise_and_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_bitwise_and_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserBitwiseXor =>
        "\txorl\t%ecx, %eax\n"
      end
    | let e: ParserBitwiseAndExp =>
      generate_bitwise_and_exp(e)
    end

  fun generate_bitwise_and_exp(exp: ParserBitwiseAndExp): String =>
    match exp
    | let e: ParserBitwiseAndExpBinaryOP =>
      match e.exp1
      | let t: ParserBitwiseAndExpBinaryOP =>
        generate_bitwise_and_exp(t)
      | let t: ParserEqualityExp =>
        generate_equality_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_equality_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserBitwiseAnd =>
        "\tandl\t%ecx, %eax\n"
      end
    | let e: ParserEqualityExp =>
      generate_equality_exp(e)
    end

  fun generate_equality_exp(exp: ParserEqualityExp): String =>
    match exp
    | let e: ParserEqualityExpBinaryOP =>
      match e.exp1
      | let t: ParserEqualityExpBinaryOP =>
        generate_equality_exp(t)
      | let t: ParserRelationalExp =>
        generate_relational_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_relational_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserEqualTo =>
        "\tcmpl\t%eax, %ecx\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsete\t%al\n"
      | ParserNotEqualTo =>
        "\tcmpl\t%eax, %ecx\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetne\t%al\n"
      end
    | let e: ParserRelationalExp =>
      generate_relational_exp(e)
    end

  fun generate_relational_exp(exp: ParserRelationalExp): String =>
    match exp
    | let e: ParserRelationalExpBinaryOP =>
      match e.exp1
      | let t: ParserRelationalExpBinaryOP =>
        generate_relational_exp(t)
      | let t: ParserBitwiseShiftExp =>
        generate_bitwise_shift_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_bitwise_shift_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserLessThan =>
        "\tcmpl\t%eax, %ecx\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetl\t%al\n"
      | ParserLessThanOrEqualTo =>
        "\tcmpl\t%eax, %ecx\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetle\t%al\n"
      | ParserGreaterThan =>
        "\tcmpl\t%eax, %ecx\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetg\t%al\n"
      | ParserGreaterThanOrEqualTo =>
        "\tcmpl\t%eax, %ecx\n" +
        "\tmovl\t$0, %eax\n" +
        "\tsetge\t%al\n"
      end
    | let e: ParserBitwiseShiftExp =>
      generate_bitwise_shift_exp(e)
    end

  fun generate_bitwise_shift_exp(exp: ParserBitwiseShiftExp): String =>
        match exp
    | let e: ParserBitwiseShiftExpBinaryOP =>
      match e.exp1
      | let t: ParserBitwiseShiftExpBinaryOP =>
        generate_relational_exp(t)
      | let t: ParserAdditiveExp =>
        generate_additive_exp(t)
      end +
      "\tpush\t%eax\n" +
      generate_additive_exp(e.exp2) +
      "\tpop\t%ecx\n" +
      match e.op
      | ParserBitwiseShiftLeft =>
        "\txchg\t%ecx, %eax\n" +
        "\tshll\t%cl, %eax\n"
      | ParserBitwiseShiftRight =>
        "\txchg\t%ecx, %eax\n" +
        "\tshrl\t%cl, %eax\n"
      end
    | let e: ParserAdditiveExp =>
      generate_additive_exp(e)
    end

  fun generate_additive_exp(exp: ParserAdditiveExp): String =>
    match exp
    | let e: ParserAdditiveExpBinaryOP =>
      match e.term1
      | let t: ParserAdditiveExpBinaryOP =>
        generate_additive_exp(t)
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
      | ParserModulo =>
        "\txchg\t%ecx, %eax\n" +
        "\tmovl\t$0, %edx\n" +
        "\tidivl\t%ecx\n" +
        "\txchg\t%edx, %eax\n"
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
