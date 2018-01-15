use "cli"
use "files"

actor Main
  let env: Env
  var lexer: Bool = false
  var parser: Bool = false
  var assembly: Bool = false
  var filepath: String = ""
  var output_filepath: String = ""

  new create(env': Env) =>
    env = env'
    let cs =
      try
        CommandSpec.leaf("clang", "A simple C compiler written in Pony", [
          OptionSpec.bool("lexer",
            "Show the generated lexer tokens."
            where short' = 'l', default' = false)
          OptionSpec.bool("parser",
            "Show the generated AST."
            where short' = 'p', default' = false)
          OptionSpec.bool("assemble",
            "Generate an assembly file instead of compiling."
            where short' = 's', default' = false)
        ], [
          ArgSpec.string("file", "A '.c' file to assemble.")
        ])? .> add_help()?
      else
        env.exitcode(-1)
        return
      end
    let cmd =
      match CommandParser(cs).parse(env.args, env.vars())
      | let c: Command => c
      | let ch: CommandHelp =>
          ch.print_help(env.out)
          env.exitcode(0)
          return
      | let se: SyntaxError =>
          env.out.print(se.string())
          env.exitcode(-1)
          return
      end
    lexer = cmd.option("lexer").bool()
    parser = cmd.option("parser").bool()
    assembly = cmd.option("assemble").bool()
    if parser and lexer then
      env.out.print("(Warning: Parser won't be run)")
    end
    if assembly and (parser or lexer) then
      env.out.print("(Warning: Generator won't be run)")
    end
    filepath = cmd.arg("file").string()
    if filepath.trim(filepath.size() - 2) != ".c" then
      env.err.print(
        "Error: Provided file is not a '.c' file.")
      env.exitcode(1)
    else
      output_filepath = filepath.trim(0, filepath.size() - 2)
      load_source_file()
    end

  fun ref load_source_file() =>
    try
      let auth: AmbientAuth = env.root as AmbientAuth
      with file = OpenFile(FilePath(auth, filepath)?) as File do
        let source_code: String = String.from_array(file.read(file.size()))
        lex_code(source_code)
      end
    else
      env.err.print("Error: Couldn't read source file.")
      env.exitcode(2)
    end

  fun ref lex_code(code: String) =>
    try
      let tokenized_code: Array[LexerToken] val = Lexer(code)?
      if lexer then
        for i in tokenized_code.values() do
          env.out.print(i.string())
        end
      else
        parse_code(tokenized_code)
      end
    else
      env.err.print("Error: Lexer failed.")
      env.exitcode(3)
    end

  fun ref parse_code(tokens: Array[LexerToken] val) =>
    try
      let production_rules: ParserProgram = Parser(tokens)?
      if parser then
        env.out.print(Parser.print_ast(production_rules))
      else
        generate_code(production_rules)
      end
    else
      env.err.print("Error: Parser failed.")
      env.exitcode(4)
    end

  fun ref generate_code(program: ParserProgram) =>
    let assembly_code: String = Generator(program)
    let new_filepath_string = filepath.trim(0, filepath.size() - 1) + "s"
    try
      let auth: AmbientAuth = env.root as AmbientAuth
      let new_filepath = FilePath(auth, new_filepath_string)?
      new_filepath.remove()
      with new_file = CreateFile(new_filepath) as File do
        if not(new_file.write(assembly_code)) then error end
      end
      if not(assembly) then
        assemble_code(new_filepath_string)
      end
    else
      env.err.print("Error: Couldn't write assembly file.")
      env.exitcode(5)
    end

  fun ref assemble_code(assembly_file: String) =>
    let command: String = "gcc -m32 " + assembly_file + " -o " + output_filepath
    try
      let auth: AmbientAuth = env.root as AmbientAuth
      try
        Shell(command)?
      else
        env.err.print("Error: Couldn't assemble executable.")
        env.exitcode(6)
      then
        FilePath(auth, assembly_file)?.remove()
      end
    end
