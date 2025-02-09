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
end
