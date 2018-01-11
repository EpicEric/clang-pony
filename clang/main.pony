use "files"

actor Main
  new create(env: Env) =>
    try
      let filepath: FilePath = FilePath(env.root as AmbientAuth, env.args(1)?)?
      let file: File = OpenFile(filepath) as File
      let code: String = String.from_array(file.read(file.size()))
      try
        let tokenized_code: Array[LexerToken] val = Lexer(code)?
        try
          let production_rules: ParserProgram = Parser(tokenized_code)?
        else
          env.err.print("Error: Parser failed.")
          env.exitcode(2)
        end
      else
        env.err.print("Error: Lexer failed.")
        env.exitcode(1)
      end
    else
      env.err.print("Error: Couldn't open file.")
      env.exitcode(-1)
    end
