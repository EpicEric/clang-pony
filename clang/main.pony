use "files"

actor Main
  new create(env: Env) =>
    try
      let filepath: FilePath = FilePath(env.root as AmbientAuth, env.args(1)?)?
      let file: File = OpenFile(filepath) as File
      let code: String = String.from_array(file.read(file.size()))
      try
        let tokenized_code: Array[Token] = Lexer(code)?
        var tokenized_code_string: String iso = recover String end
        for i in tokenized_code.values() do
          tokenized_code_string.append(i.string() + " ")
        end
        env.out.print(consume tokenized_code_string)
      else
        env.out.print("Error: Lexer failed.")
        env.exitcode(1)
      end
    else
      env.out.print("Error: Couldn't open file.")
        env.exitcode(-1)
    end
