primitive Generator
  fun apply(program: ParserProgram): String =>
    let function = generate_function(program.function)
    function

  fun generate_function(function: ParserFunction): String =>
    let statement = generate_statement(function.statement)
    "\t.globl " + function.id + "\n" +
    function.id + ":\n" +
    statement

  fun generate_statement(statement: ParserStatement): String =>
    match statement
    | let s: ParserReturn =>
      let value = (s.exp).int
      "\tmovl\t$" + value.string() + ", %eax\n" +
      "\tret\n" 
    end