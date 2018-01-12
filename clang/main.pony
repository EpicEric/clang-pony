use "files"

actor Main
  let env: Env
  let lexer: Bool
  let parser: Bool
  let assembly: Bool
  let filepath: String
  let output_filepath: String

  new create(env': Env) =>
    env = env'
    lexer = env.args.contains("-l" where predicate = {(l, r) => l == r})
    parser = env.args.contains("-p"  where predicate = {(l, r) => l == r})
    assembly = env.args.contains("-s"  where predicate = {(l, r) => l == r})
    if parser and lexer then
      env.out.print("(Warning: Parser won't be run)")
    end
    if assembly and (parser or lexer) then
      env.out.print("(Warning: Generator won't be run)")
    end
    filepath =
      try
        env.args(env.args.find(
          ".c" where predicate = {(l, r) => l.trim(l.size() - 2) == r}
        )?)?
      else
        ""
      end
    output_filepath =
      if filepath.size() > 0 then
        filepath.trim(0, filepath.size() - 2)
      else
        ""
      end
    if filepath.size() > 0 then
      load_source_file()
    else
      env.err.print("Error: No '.c' file provided.")
      env.exitcode(1)
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
      Shell(command)?
      FilePath(auth, assembly_file)?.remove()
    else
      env.err.print("Error: Couldn't assemble executable.")
      env.exitcode(6)
    end
