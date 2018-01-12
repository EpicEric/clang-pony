use @system[I32](command: Pointer[U8] tag)

primitive Shell
  fun tag apply(
    command: String,
    exit_code_fn: (_ExitCodeFn | None) = None) ?
  =>
    var rc = @system(command.cstring())
    if (rc < 0) or (rc > 255) then rc = 1 end // clip out-of-bounds exit codes
    try (exit_code_fn as _ExitCodeFn)(rc) end
    if rc != 0 then error end

interface _ExitCodeFn
  fun ref apply(code: I32)
