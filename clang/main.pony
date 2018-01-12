use "files"

actor Main
  new create(env: Env) =>
    try
      let auth: AmbientAuth = env.root as AmbientAuth
      let filepath: FilePath = FilePath(auth, env.args(1)?)?
      with file = OpenFile(filepath) as File do
        let source_code: String = String.from_array(file.read(file.size()))
        try
          let tokenized_code: Array[LexerToken] val = Lexer(source_code)?
          try
            let production_rules: ParserProgram = Parser(tokenized_code)?
            let assembly_code: String = Generator(production_rules)
            var new_filepath_string = filepath.path
            let end_of_string = new_filepath_string.trim(
              new_filepath_string.size() - 2)
            if end_of_string == ".c" then
              new_filepath_string = new_filepath_string.trim(
                0, new_filepath_string.size() - 2)
            end
            new_filepath_string = new_filepath_string + ".s"
            try
              let new_filepath = FilePath(auth, new_filepath_string)?
              with new_file = CreateFile(new_filepath) as File do
                if not(new_file.write(assembly_code)) then error end
              end
            else
              env.err.print("Error: Couldn't save file.")
              env.exitcode(10)
            end
          else
            env.err.print("Error: Parser failed.")
            env.exitcode(2)
          end
        else
          env.err.print("Error: Lexer failed.")
          env.exitcode(1)
        end
      end
    else
      env.err.print("Error: Couldn't read file.")
      env.exitcode(-1)
    end
