import Config

if config_env() == :test do
  # set this to true to get parser trace information printed to the terminal
  # example:
  # BEGIN[N0,L1,C3]: parse_program
  #   BEGIN[N0,L1,C3]: parse_expression
  #     BEGIN[N0,L1,C3]: parse_sigil
  #       BEGIN[N0,L1,C3]: parse_interpolation
  #       END: parse_interpolation
  #     END: parse_sigil
  #   END: parse_expression
  # END: parse_program

  config :spitfire, trace: false

  is_ci = "CI" |> System.get_env("false") |> String.to_existing_atom()

  config :spitfire, is_ci: is_ci

  max_runs =
    if is_ci do
      500_000
    else
      1000
    end

  config :stream_data, max_runs: max_runs
end
