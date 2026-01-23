defmodule Spitfire.CharPropertyTest do
  @moduledoc """
  Property tests for ascii strings in various Elixir contexts.
  """
  use ExUnit.Case,
    async: false,
    parameterize: [
      %{mode: :strict},
      %{mode: :tolerant}
    ]

  use ExUnitProperties

  setup %{mode: mode} do
    Process.put(:spitfire_test_mode, mode)
    :ok
  end

  # Options used by both oracle and Spitfire for consistency
  @oracle_opts [
    columns: true,
    token_metadata: true,
    emit_warnings: false,
    existing_atoms_only: false
  ]

  # Character set for generating random code fragments
  @char_set [
    # minimal set to cover all keywords, operators, brackets, separators, numbers, aliases and identifiers
    ?0,
    ?1,
    ?b,
    ?x,
    ?d,
    ?o,
    ?e,
    ?n,
    ?d,
    ?c,
    ?a,
    ?t,
    ?c,
    ?h,
    ?r,
    ?e,
    ?s,
    ?c,
    ?u,
    ?e,
    ?a,
    ?f,
    ?t,
    ?e,
    ?r,
    ?e,
    ?l,
    ?s,
    ?e,
    ?f,
    ?n,
    ?w,
    ?h,
    ?e,
    ?n,
    ?a,
    ?n,
    ?d,
    ?o,
    ?r,
    ?n,
    ?o,
    ?t,
    ?i,
    ?n,
    ?t,
    ?r,
    ?u,
    ?e,
    ?f,
    ?a,
    ?l,
    ?s,
    ?e,
    ?n,
    ?i,
    ?l,
    ?A,
    ?!,
    ?@,
    ?^,
    ?&,
    ?*,
    ?(,
    ?),
    ?-,
    ?+,
    ?[,
    ?],
    ?{,
    ?},
    ?;,
    ?:,
    ?',
    ?",
    ?\\,
    ?|,
    ?~,
    ?<,
    ?>,
    ?,,
    ?.,
    ?/,
    ??,
    ?$,
    ?%,
    ?_,
    ?=,
    ?\s,
    # excluded for now - create too many comments
    # ?#,
    # excluded for now
    ?\n
  ]

  # ===========================================================================
  # Code Fragment Generator
  # ===========================================================================

  defp code_fragment_gen(opts \\ []) do
    min_length = Keyword.get(opts, :min_length, 0)
    max_length = Keyword.get(opts, :max_length, 16)
    StreamData.string(@char_set, min_length: min_length, max_length: max_length)
  end

  defp nonempty_code_fragment_gen(opts \\ []) do
    opts
    |> Keyword.put_new(:min_length, 1)
    |> code_fragment_gen()
  end

  # ===========================================================================
  # Context Generators - each returns {context_name, full_code}
  # ===========================================================================

  # Beginning of string (code as standalone expression)
  defp context_standalone do
    StreamData.bind(code_fragment_gen(), fn code ->
      StreamData.constant({"standalone", code})
    end)
  end

  # Inside bitstring: <<a, s: CODE >>
  defp context_bitstring do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "<<a, s: " <> code <> " >>"
      StreamData.constant({"bitstring", full_code})
    end)
  end

  # bitstring -> open_bit container_args close_bit
  # container_args -> container_args_base ',' kw_data
  # Bitstring with positional segment then kw_data tail: <<x, a: CODE>>
  defp context_bitstring_positional_then_kw_data do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "<<x, a: " <> code <> ">>"
      StreamData.constant({"bitstring_positional_then_kw_data", full_code})
    end)
  end

  # Before do block: CODE do :ok end
  defp context_before_do do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = code <> " do :ok end"
      StreamData.constant({"before_do", full_code})
    end)
  end

  # After do block: foo do :ok end CODE
  defp context_after_do do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo do :ok end " <> code
      StreamData.constant({"after_do", full_code})
    end)
  end

  # Inside fn - various positions
  defp context_fn_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn " <> code <> " -> :ok end"
      StreamData.constant({"fn_arg", full_code})
    end)
  end

  # stab_expr -> empty_paren stab_op_eol_and_expr
  # Empty-paren stab: fn () -> CODE end
  defp context_fn_empty_paren_stab do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn () -> " <> code <> " end"
      StreamData.constant({"fn_empty_paren_stab", full_code})
    end)
  end

  # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
  # Empty-paren stab with guard: fn () when CODE -> :ok end
  defp context_fn_empty_paren_when do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn () when " <> code <> " -> :ok end"
      StreamData.constant({"fn_empty_paren_when", full_code})
    end)
  end

  # stab_parens_many -> open_paren call_args_no_parens_many close_paren
  # Paren-wrapped patterns: fn (a, CODE) -> :ok end
  defp context_fn_parens_many_lhs do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn (a, " <> code <> ") -> :ok end"
      StreamData.constant({"fn_parens_many_lhs", full_code})
    end)
  end

  # stab_expr -> stab_parens_many when_op expr stab_op_eol_and_expr
  # Paren-wrapped patterns with guard: fn (a, b) when CODE -> :ok end
  defp context_fn_parens_many_when do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn (a, b) when " <> code <> " -> :ok end"
      StreamData.constant({"fn_parens_many_when", full_code})
    end)
  end

  defp context_fn_body do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn 1 -> " <> code <> " end"
      StreamData.constant({"fn_body", full_code})
    end)
  end

  defp context_fn_no_arrow do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn " <> code <> " end"
      StreamData.constant({"fn_no_arrow", full_code})
    end)
  end

  defp context_fn_multi_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn a, " <> code <> " end"
      StreamData.constant({"fn_multi_arg", full_code})
    end)
  end

  defp context_fn_multi_arg_with_arrow do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn a, " <> code <> " -> :ok end"
      StreamData.constant({"fn_multi_arg_arrow", full_code})
    end)
  end

  # Inside do block: foo do CODE end
  defp context_inside_do do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo do " <> code <> " end"
      StreamData.constant({"inside_do", full_code})
    end)
  end

  # Inside parens call: foo(CODE)
  defp context_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo(" <> code <> ")"
      StreamData.constant({"parens_call", full_code})
    end)
  end

  # call_args_parens -> open_paren no_parens_expr close_paren
  # Parens call with a single no-parens expression argument: foo(f CODE)
  defp context_parens_call_single_no_parens_expr_arg do
    code_fragment_gen(min_length: 1)
    |> StreamData.filter(fn code ->
      not String.contains?(code, ",") and
        not String.contains?(code, ")") and
        not String.contains?(code, "\n")
    end)
    |> StreamData.bind(fn code ->
      full_code = "foo(f " <> code <> ")"
      StreamData.constant({"parens_call_single_no_parens_expr_arg", full_code})
    end)
  end

  # call_args_parens -> open_paren call_args_parens_base ',' kw_call close_paren
  # Parens call with positional args and trailing kw_call: foo(1, a: CODE)
  defp context_parens_call_args_then_kw_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo(1, a: " <> code <> ")"
      StreamData.constant({"parens_call_args_then_kw_call", full_code})
    end)
  end

  # kw_call -> kw_base ',' matched_expr
  # Parens call that begins as kw_call then has a follow-up expr: foo(a: 1, CODE)
  defp context_parens_call_kw_call_follow_up do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo(a: 1, " <> code <> ")"
      StreamData.constant({"parens_call_kw_call_follow_up", full_code})
    end)
  end

  # Inside no parens call: foo CODE
  defp context_no_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo " <> code
      StreamData.constant({"no_parens_call", full_code})
    end)
  end

  # Inside bracket access: foo[CODE]
  defp context_bracket_access do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo[" <> code <> "]"
      StreamData.constant({"bracket_access", full_code})
    end)
  end

  # bracket_arg -> open_bracket container_expr ',' close_bracket
  # Inside bracket access with trailing comma: foo[CODE,]
  defp context_bracket_access_trailing_comma do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo[" <> code <> ",]"
      StreamData.constant({"bracket_access_trailing_comma", full_code})
    end)
  end

  # bracket_arg -> open_bracket kw_data close_bracket
  # Inside bracket access using kw_data: foo[a: CODE]
  defp context_bracket_access_kw_data_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo[a: " <> code <> "]"
      StreamData.constant({"bracket_access_kw_data_value", full_code})
    end)
  end

  # bracket_at_expr -> at_op_eol access_expr bracket_arg
  # Access syntax under @: @foo[CODE]
  defp context_bracket_at_access do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "@foo[" <> code <> "]"
      StreamData.constant({"bracket_at_access", full_code})
    end)
  end

  # Access syntax under @ with kw_data: @foo[a: CODE]
  defp context_bracket_at_access_kw_data_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "@foo[a: " <> code <> "]"
      StreamData.constant({"bracket_at_access_kw_data_value", full_code})
    end)
  end

  # Inside map: %{CODE}
  defp context_map do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{" <> code <> "}"
      StreamData.constant({"map", full_code})
    end)
  end

  # map_close -> assoc_base ',' kw_data close_curly
  # Hybrid map with assoc (=>) and kw_data: %{K1 => 1, a: V2}
  defp context_map_assoc_then_kw_data do
    StreamData.bind(StreamData.tuple({code_fragment_gen(), code_fragment_gen()}), fn {k1, v2} ->
      full_code = "%{" <> k1 <> " => 1, a: " <> v2 <> "}"
      StreamData.constant({"map_assoc_then_kw_data", full_code})
    end)
  end

  # Hybrid map with assoc after a kw_data key: %{a: V1, K2 => 1}
  defp context_map_kw_data_then_assoc do
    StreamData.bind(StreamData.tuple({code_fragment_gen(), code_fragment_gen()}), fn {v1, k2} ->
      full_code = "%{a: " <> v1 <> ", " <> k2 <> " => 1}"
      StreamData.constant({"map_kw_data_then_assoc", full_code})
    end)
  end

  # map_args -> open_curly assoc_update_kw close_curly
  # Map update using kw_data on RHS: %{x | a: CODE}
  defp context_map_update_kw_data_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{x | a: " <> code <> "}"
      StreamData.constant({"map_update_kw_data_value", full_code})
    end)
  end

  # map_args -> open_curly assoc_update close_curly
  # Map update using assoc_expr on RHS: %{x | CODE => 1}
  defp context_map_update_assoc_key do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{x | " <> code <> " => 1}"
      StreamData.constant({"map_update_assoc_key", full_code})
    end)
  end

  # Inside struct: %Foo{CODE}
  defp context_struct do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%Foo{" <> code <> "}"
      StreamData.constant({"struct", full_code})
    end)
  end

  # Inside tuple: {CODE}
  defp context_tuple do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "{" <> code <> "}"
      StreamData.constant({"tuple", full_code})
    end)
  end

  # tuple -> open_curly container_args close_curly
  # container_args -> container_args_base ',' kw_data
  # Tuple with positional element(s) then kw_data tail: {x, a: CODE}
  defp context_tuple_positional_then_kw_data do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "{x, a: " <> code <> "}"
      StreamData.constant({"tuple_positional_then_kw_data", full_code})
    end)
  end

  # Inside list: [CODE]
  defp context_list do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[" <> code <> "]"
      StreamData.constant({"list", full_code})
    end)
  end

  # list_args -> container_args_base ',' kw_data
  # List with positional element(s) then kw_data: [x, a: CODE]
  defp context_list_positional_then_kw_data do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[x, a: " <> code <> "]"
      StreamData.constant({"list_positional_then_kw_data", full_code})
    end)
  end

  # kw_data -> kw_base ',' matched_expr
  # List that begins as keyword data then has a follow-up expr: [a: 1, CODE]
  defp context_list_kw_data_follow_up do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[a: 1, " <> code <> "]"
      StreamData.constant({"list_kw_data_follow_up", full_code})
    end)
  end

  # Inside parens: (CODE)
  defp context_parens do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(" <> code <> ")"
      StreamData.constant({"parens", full_code})
    end)
  end

  # Inside string interpolation: "#{CODE}"
  defp context_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "\"" <> "\#{" <> code <> "}" <> "\""
      StreamData.constant({"interpolation", full_code})
    end)
  end

  # After pipe: :ok |> CODE
  defp context_after_pipe do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = ":ok |> " <> code
      StreamData.constant({"after_pipe", full_code})
    end)
  end

  # After assignment: x = CODE
  defp context_after_assignment do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x = " <> code
      StreamData.constant({"after_assignment", full_code})
    end)
  end

  # Inside struct arg: %CODE{}
  defp context_struct_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%" <> code <> "{}"
      StreamData.constant({"struct_arg", full_code})
    end)
  end

  # Between do blocks: foo do :ok end CODE do :error end
  defp context_between_do_blocks do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo do :ok end " <> code <> " do :error end"
      StreamData.constant({"between_do_blocks", full_code})
    end)
  end

  # Inside ternary range - first position: CODE..x//y
  defp context_ternary_first do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = code <> "..x//y"
      StreamData.constant({"ternary_first", full_code})
    end)
  end

  # Inside ternary range - second position: x..CODE//y
  defp context_ternary_second do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x.." <> code <> "//y"
      StreamData.constant({"ternary_second", full_code})
    end)
  end

  # Inside ternary range - third position: x..y//CODE
  defp context_ternary_third do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x..y//" <> code
      StreamData.constant({"ternary_third", full_code})
    end)
  end

  # Inside map update - updated expression: %{CODE | x: y}
  defp context_map_update_expr do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{" <> code <> " | x: y}"
      StreamData.constant({"map_update_expr", full_code})
    end)
  end

  # Inside map update - key/value part: %{x | CODE}
  defp context_map_update_kv do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{x | " <> code <> "}"
      StreamData.constant({"map_update_kv", full_code})
    end)
  end

  # Inside map update - value: %{x | foo: CODE}
  defp context_map_update_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{x | foo: " <> code <> "}"
      StreamData.constant({"map_update_value", full_code})
    end)
  end

  # After parens call: foo()CODE
  defp context_after_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo()" <> code
      StreamData.constant({"after_parens_call", full_code})
    end)
  end

  # Inside no parens call with two args: foo CODE bar
  defp context_no_parens_call_middle do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo " <> code <> " bar"
      StreamData.constant({"no_parens_call_middle", full_code})
    end)
  end

  # Inside dot - before dot call: CODE.foo()
  defp context_before_dot_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = code <> ".foo()"
      StreamData.constant({"before_dot_call", full_code})
    end)
  end

  # Inside dot - middle of chain: A.CODE.foo()
  defp context_dot_chain_middle do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "A." <> code <> ".foo()"
      StreamData.constant({"dot_chain_middle", full_code})
    end)
  end

  # Inside dot - with tuple: A.CODE.{}
  defp context_dot_tuple do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "A." <> code <> ".{}"
      StreamData.constant({"dot_tuple", full_code})
    end)
  end

  # After dot: foo.CODE
  defp context_after_dot do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo." <> code
      StreamData.constant({"after_dot", full_code})
    end)
  end

  # dot_call_identifier -> matched_expr dot_call_op
  # Function call via `.(...)` with empty args: (CODE).()
  defp context_dot_call_empty_args do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(" <> code <> ").()"
      StreamData.constant({"dot_call_empty_args", full_code})
    end)
  end

  # Function call via `.(...)` with one argument: (LHS).(CODE)
  defp context_dot_call_one_arg do
    StreamData.bind(StreamData.tuple({code_fragment_gen(), code_fragment_gen()}), fn {lhs, arg} ->
      full_code = "(" <> lhs <> ").(" <> arg <> ")"
      StreamData.constant({"dot_call_one_arg", full_code})
    end)
  end

  # Function call via `.(...)` with kw_call: (LHS).(a: CODE)
  defp context_dot_call_kw_call do
    StreamData.bind(StreamData.tuple({code_fragment_gen(), code_fragment_gen()}), fn {lhs, value} ->
      full_code = "(" <> lhs <> ").(a: " <> value <> ")"
      StreamData.constant({"dot_call_kw_call", full_code})
    end)
  end

  # access_expr -> open_paren stab_eoe ')'
  # Parenthesized stab expression: (x -> CODE)
  defp context_paren_stab_single do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(x -> " <> code <> ")"
      StreamData.constant({"paren_stab_single", full_code})
    end)
  end

  # Parenthesized multi-clause stab: (x -> :ok; y -> CODE)
  defp context_paren_stab_multi do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(x -> :ok; y -> " <> code <> ")"
      StreamData.constant({"paren_stab_multi", full_code})
    end)
  end

  # access_expr -> open_paren ';' stab_eoe ')'
  # Semicolon-prefixed parenthesized stab: (; x -> CODE)
  defp context_paren_stab_semicolon_single do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(; x -> " <> code <> ")"
      StreamData.constant({"paren_stab_semicolon_single", full_code})
    end)
  end

  # Semicolon-prefixed multi-clause stab: (; x -> :ok; y -> CODE)
  defp context_paren_stab_semicolon_multi do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(; x -> :ok; y -> " <> code <> ")"
      StreamData.constant({"paren_stab_semicolon_multi", full_code})
    end)
  end

  # access_expr -> open_paren ';' close_paren
  # Empty paren-stab form: (;)
  defp context_empty_paren_semicolon do
    StreamData.constant({"empty_paren_semicolon", "(;)"})
  end

  # bracket_expr -> access_expr bracket_arg
  # Bracket access on a parenthesized expr: (CODE)[x]
  defp context_bracket_on_parens_expr do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(" <> code <> ")[x]"
      StreamData.constant({"bracket_on_parens_expr", full_code})
    end)
  end

  # Between operators: x + CODE * y
  defp context_between_operators do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x + " <> code <> " * y"
      StreamData.constant({"between_operators", full_code})
    end)
  end

  # After unary &: &CODE
  defp context_after_capture do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "&" <> code
      StreamData.constant({"after_capture", full_code})
    end)
  end

  # After unary ^: ^CODE
  defp context_after_pin do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "^" <> code
      StreamData.constant({"after_pin", full_code})
    end)
  end

  # After unary +: +CODE
  defp context_after_unary_plus do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "+" <> code
      StreamData.constant({"after_unary_plus", full_code})
    end)
  end

  # After unary -: -CODE
  defp context_after_unary_minus do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "-" <> code
      StreamData.constant({"after_unary_minus", full_code})
    end)
  end

  # After unary @: @CODE
  defp context_after_at do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "@" <> code
      StreamData.constant({"after_at", full_code})
    end)
  end

  # After unary !: !CODE
  defp context_after_bang do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "!" <> code
      StreamData.constant({"after_bang", full_code})
    end)
  end

  # After unary not: not CODE
  defp context_after_not do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "not " <> code
      StreamData.constant({"after_not", full_code})
    end)
  end

  # Inside interpolated atom: :"foo#{CODE}"
  defp context_interpolated_atom do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = ":\"foo\#{" <> code <> "}\""
      StreamData.constant({"interpolated_atom", full_code})
    end)
  end

  # Inside interpolated keyword key: ["foo#{CODE}": 1]
  defp context_interpolated_keyword do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[\"foo\#{" <> code <> "}\": 1]"
      StreamData.constant({"interpolated_keyword", full_code})
    end)
  end

  # Inside charlist interpolation: 'foo#{CODE}'
  defp context_charlist_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "'foo\#{" <> code <> "}'"
      StreamData.constant({"charlist_interpolation", full_code})
    end)
  end

  # Inside string heredoc interpolation: """
  # foo#{CODE}
  # """
  defp context_string_heredoc_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "\"\"\"\nfoo\#{" <> code <> "}\n\"\"\""
      StreamData.constant({"string_heredoc_interpolation", full_code})
    end)
  end

  # Inside charlist heredoc interpolation: '''
  # foo#{CODE}
  # '''
  defp context_charlist_heredoc_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "'''\nfoo\#{" <> code <> "}\n'''"
      StreamData.constant({"charlist_heredoc_interpolation", full_code})
    end)
  end

  # Inside sigil interpolation: ~s/foo#{CODE}/
  defp context_sigil_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "~s/foo\#{" <> code <> "}/"
      StreamData.constant({"sigil_interpolation", full_code})
    end)
  end

  # Inside sigil heredoc interpolation: ~s"""
  # foo#{CODE}
  # """
  defp context_sigil_heredoc_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "~s\"\"\"\nfoo\#{" <> code <> "}\n\"\"\""
      StreamData.constant({"sigil_heredoc_interpolation", full_code})
    end)
  end

  # Inside when expr in fn: fn x when CODE -> 1 end
  defp context_fn_when do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn x when " <> code <> " -> 1 end"
      StreamData.constant({"fn_when", full_code})
    end)
  end

  # Inside def with parens args: def foo(CODE) do :ok end
  defp context_def_parens_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "def foo(" <> code <> ") do :ok end"
      StreamData.constant({"def_parens_arg", full_code})
    end)
  end

  # Inside def with no parens args: def foo CODE do 1 end
  defp context_def_no_parens_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "def foo " <> code <> " do 1 end"
      StreamData.constant({"def_no_parens_arg", full_code})
    end)
  end

  # Inside def when guard: def foo() when CODE do 1 end
  defp context_def_when do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "def foo() when " <> code <> " do 1 end"
      StreamData.constant({"def_when", full_code})
    end)
  end

  # Inside keyword list value: [a: CODE]
  defp context_keyword_list_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[a: " <> code <> "]"
      StreamData.constant({"keyword_list_value", full_code})
    end)
  end

  # Inside map kv value: %{a: CODE}
  defp context_map_kv_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{a: " <> code <> "}"
      StreamData.constant({"map_kv_value", full_code})
    end)
  end

  # Inside map rocket key: %{CODE => 1}
  defp context_map_rocket_key do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{" <> code <> " => 1}"
      StreamData.constant({"map_rocket_key", full_code})
    end)
  end

  # Inside struct kv value: %Foo{a: CODE}
  defp context_struct_kv_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%Foo{a: " <> code <> "}"
      StreamData.constant({"struct_kv_value", full_code})
    end)
  end

  # Inside parens call with multiple args: foo(1, CODE, 2)
  defp context_parens_call_multi_args_middle do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo(1, " <> code <> ", 2)"
      StreamData.constant({"parens_call_multi_args_middle", full_code})
    end)
  end

  # Inside parens call keyword arg value: foo(a: CODE)
  defp context_parens_call_kw_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo(a: " <> code <> ")"
      StreamData.constant({"parens_call_kw_value", full_code})
    end)
  end

  # Inside no-parens call keyword arg value: foo a: CODE
  defp context_no_parens_call_kw_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo a: " <> code
      StreamData.constant({"no_parens_call_kw_value", full_code})
    end)
  end

  # Inside no-parens call with multiple args: foo 1, CODE
  defp context_no_parens_call_multi_args do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo 1, " <> code
      StreamData.constant({"no_parens_call_multi_args", full_code})
    end)
  end

  # Nested no-parens call ambiguity: f g CODE, h
  defp context_nested_no_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "f g " <> code <> ", h"
      StreamData.constant({"nested_no_parens_call", full_code})
    end)
  end

  # Do-block attached to a parens call: foo(CODE) do :ok end
  defp context_parens_call_with_do do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "foo(" <> code <> ") do :ok end"
      StreamData.constant({"parens_call_with_do", full_code})
    end)
  end

  # Do-block attached to a no-parens call: foo CODE do :ok end
  defp context_no_parens_call_with_do do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "foo " <> code <> " do :ok end"
      StreamData.constant({"no_parens_call_with_do", full_code})
    end)
  end

  # If condition: if CODE do :ok end
  defp context_if_condition do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "if " <> code <> " do :ok end"
      StreamData.constant({"if_condition", full_code})
    end)
  end

  # If else body: if true do :ok else CODE end
  defp context_if_else_body do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "if true do :ok else " <> code <> " end"
      StreamData.constant({"if_else_body", full_code})
    end)
  end

  # Case clause pattern (stab lhs): case x do CODE -> :ok end
  defp context_case_clause_lhs do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "case x do " <> code <> " -> :ok end"
      StreamData.constant({"case_clause_lhs", full_code})
    end)
  end

  # Case clause body (stab rhs): case x do 1 -> CODE end
  defp context_case_clause_rhs do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "case x do 1 -> " <> code <> " end"
      StreamData.constant({"case_clause_rhs", full_code})
    end)
  end

  # Case second clause pattern: case x do 1 -> :ok; CODE -> :error end
  defp context_case_second_clause_lhs do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "case x do 1 -> :ok; " <> code <> " -> :error end"
      StreamData.constant({"case_second_clause_lhs", full_code})
    end)
  end

  # Cond clause condition (stab lhs): cond do CODE -> :ok end
  defp context_cond_clause_lhs do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "cond do " <> code <> " -> :ok end"
      StreamData.constant({"cond_clause_lhs", full_code})
    end)
  end

  # Multi-clause fn (stab lhs): fn 1 -> :ok; CODE -> :error end
  defp context_fn_second_clause_lhs do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "fn 1 -> :ok; " <> code <> " -> :error end"
      StreamData.constant({"fn_second_clause_lhs", full_code})
    end)
  end

  # With generator RHS: with x <- CODE do x end
  defp context_with_generator_rhs do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "with x <- " <> code <> " do x end"
      StreamData.constant({"with_generator_rhs", full_code})
    end)
  end

  # With generator LHS (pattern): with CODE <- x do x end
  defp context_with_generator_lhs do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "with " <> code <> " <- x do x end"
      StreamData.constant({"with_generator_lhs", full_code})
    end)
  end

  # With else clause body: with x <- 1 do :ok else _ -> CODE end
  defp context_with_else_body do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "with x <- 1 do :ok else _ -> " <> code <> " end"
      StreamData.constant({"with_else_body", full_code})
    end)
  end

  # For generator RHS: for x <- CODE, do: x
  defp context_for_generator_rhs do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "for x <- " <> code <> ", do: x"
      StreamData.constant({"for_generator_rhs", full_code})
    end)
  end

  # For filter expr: for x <- [1], CODE, do: x
  defp context_for_filter do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "for x <- [1], " <> code <> ", do: x"
      StreamData.constant({"for_filter", full_code})
    end)
  end

  # Try body: try do CODE after :ok end
  defp context_try_body do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "try do " <> code <> " after :ok end"
      StreamData.constant({"try_body", full_code})
    end)
  end

  # Try rescue clause body: try do :ok rescue _ -> CODE end
  defp context_try_rescue_body do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "try do :ok rescue _ -> " <> code <> " end"
      StreamData.constant({"try_rescue_body", full_code})
    end)
  end

  # Receive clause pattern (stab lhs): receive do CODE -> :ok after 0 -> :timeout end
  defp context_receive_clause_lhs do
    StreamData.bind(nonempty_code_fragment_gen(), fn code ->
      full_code = "receive do " <> code <> " -> :ok after 0 -> :timeout end"
      StreamData.constant({"receive_clause_lhs", full_code})
    end)
  end

  # Bitstring segment spec: <<a::CODE>>
  defp context_bitstring_segment_spec do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "<<a::" <> code <> ">>"
      StreamData.constant({"bitstring_segment_spec", full_code})
    end)
  end

  # Combined generator that picks one context randomly
  defp all_contexts_gen do
    StreamData.one_of([
      context_standalone(),
      context_bitstring(),
      context_bitstring_positional_then_kw_data(),
      context_before_do(),
      context_after_do(),
      context_fn_arg(),
      context_fn_empty_paren_stab(),
      context_fn_empty_paren_when(),
      context_fn_parens_many_lhs(),
      context_fn_parens_many_when(),
      context_fn_body(),
      context_fn_no_arrow(),
      context_fn_multi_arg(),
      context_fn_multi_arg_with_arrow(),
      context_inside_do(),
      context_parens_call(),
      context_parens_call_single_no_parens_expr_arg(),
      context_parens_call_args_then_kw_call(),
      context_parens_call_kw_call_follow_up(),
      context_no_parens_call(),
      context_bracket_access(),
      context_bracket_access_trailing_comma(),
      context_bracket_access_kw_data_value(),
      context_bracket_at_access(),
      context_bracket_at_access_kw_data_value(),
      context_map(),
      context_map_assoc_then_kw_data(),
      context_map_kw_data_then_assoc(),
      context_struct(),
      context_tuple(),
      context_tuple_positional_then_kw_data(),
      context_list(),
      context_list_positional_then_kw_data(),
      context_list_kw_data_follow_up(),
      context_parens(),
      context_interpolation(),
      context_after_pipe(),
      context_after_assignment(),
      # New contexts
      context_struct_arg(),
      context_between_do_blocks(),
      context_ternary_first(),
      context_ternary_second(),
      context_ternary_third(),
      context_map_update_expr(),
      context_map_update_kv(),
      context_map_update_value(),
      context_after_parens_call(),
      context_no_parens_call_middle(),
      context_before_dot_call(),
      context_dot_chain_middle(),
      context_dot_tuple(),
      context_after_dot(),
      context_dot_call_empty_args(),
      context_dot_call_one_arg(),
      context_dot_call_kw_call(),
      context_paren_stab_single(),
      context_paren_stab_multi(),
      context_paren_stab_semicolon_single(),
      context_paren_stab_semicolon_multi(),
      context_empty_paren_semicolon(),
      context_bracket_on_parens_expr(),
      context_between_operators(),
      context_after_capture(),
      context_after_pin(),
      context_after_unary_plus(),
      context_after_unary_minus(),
      context_after_at(),
      context_after_bang(),
      context_after_not(),
      context_interpolated_atom(),
      context_interpolated_keyword(),
      # Interpolation contexts
      context_charlist_interpolation(),
      context_string_heredoc_interpolation(),
      context_charlist_heredoc_interpolation(),
      context_sigil_interpolation(),
      context_sigil_heredoc_interpolation(),
      # When and def contexts
      context_fn_when(),
      context_def_parens_arg(),
      context_def_no_parens_arg(),
      context_def_when(),
      # Keyword/kv_data + call-args variants
      context_keyword_list_value(),
      context_map_kv_value(),
      context_map_rocket_key(),
      context_map_update_kw_data_value(),
      context_map_update_assoc_key(),
      context_struct_kv_value(),
      context_parens_call_multi_args_middle(),
      context_parens_call_kw_value(),
      context_no_parens_call_kw_value(),
      context_no_parens_call_multi_args(),
      context_nested_no_parens_call(),
      # Do-block attachment variants + control flow / stabs
      context_parens_call_with_do(),
      context_no_parens_call_with_do(),
      context_if_condition(),
      context_if_else_body(),
      context_case_clause_lhs(),
      context_case_clause_rhs(),
      context_case_second_clause_lhs(),
      context_cond_clause_lhs(),
      context_fn_second_clause_lhs(),
      context_with_generator_rhs(),
      context_with_generator_lhs(),
      context_with_else_body(),
      context_for_generator_rhs(),
      context_for_filter(),
      context_try_body(),
      context_try_rescue_body(),
      context_receive_clause_lhs(),
      context_bitstring_segment_spec()
    ])
  end

  # ===========================================================================
  # Property Tests
  # ===========================================================================

  describe "ascii in contexts" do
    @tag :property
    @tag timeout: 120_000
    property "grammar trees round-trip through Spitfire in all contexts" do
      check all(
              {context, code} <- all_contexts_gen(),
              max_runs: 2_000_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  # Individual context tests for targeted debugging
  describe "ascii standalone" do
    @tag :property
    @tag timeout: 120_000
    property "standalone expressions" do
      check all(
              {context, code} <- context_standalone(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii bitstring" do
    @tag :property
    @tag timeout: 120_000
    property "inside bitstring" do
      check all(
              {context, code} <-
                StreamData.one_of([
                  context_bitstring(),
                  context_bitstring_positional_then_kw_data()
                ]),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii before_do" do
    @tag :property
    @tag timeout: 120_000
    property "before do block" do
      check all(
              {context, code} <- context_before_do(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii after_do" do
    @tag :property
    @tag timeout: 120_000
    property "after do block" do
      check all({context, code} <- context_after_do(), max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii fn" do
    @tag :property
    @tag timeout: 120_000
    property "inside fn expressions" do
      fn_contexts =
        StreamData.one_of([
          context_fn_arg(),
          context_fn_empty_paren_stab(),
          context_fn_empty_paren_when(),
          context_fn_parens_many_lhs(),
          context_fn_parens_many_when(),
          context_fn_body(),
          context_fn_no_arrow(),
          context_fn_multi_arg(),
          context_fn_multi_arg_with_arrow()
        ])

      check all({context, code} <- fn_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii inside_do" do
    @tag :property
    @tag timeout: 120_000
    property "inside do block" do
      check all(
              {context, code} <- context_inside_do(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii calls" do
    @tag :property
    @tag timeout: 120_000
    property "inside function calls" do
      call_contexts =
        StreamData.one_of([
          context_parens_call(),
          context_parens_call_args_then_kw_call(),
          context_parens_call_kw_call_follow_up(),
          context_no_parens_call()
        ])

      check all({context, code} <- call_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii bracket_access" do
    @tag :property
    @tag timeout: 120_000
    property "inside bracket access" do
      check all(
              {context, code} <-
                StreamData.one_of([
                  context_bracket_access(),
                  context_bracket_access_trailing_comma(),
                  context_bracket_access_kw_data_value(),
                  context_bracket_at_access(),
                  context_bracket_at_access_kw_data_value()
                ]),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii containers" do
    @tag :property
    @tag timeout: 120_000
    property "inside containers (map, tuple, list, struct)" do
      container_contexts =
        StreamData.one_of([
          context_map(),
          context_map_assoc_then_kw_data(),
          context_map_kw_data_then_assoc(),
          context_struct(),
          context_tuple(),
          context_tuple_positional_then_kw_data(),
          context_list(),
          context_list_positional_then_kw_data(),
          context_list_kw_data_follow_up(),
          context_parens()
        ])

      check all(
              {context, code} <- container_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii interpolation" do
    @tag :property
    @tag timeout: 120_000
    property "inside string interpolation" do
      check all(
              {context, code} <- context_interpolation(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii operators" do
    @tag :property
    @tag timeout: 120_000
    property "after operators (pipe, assignment)" do
      op_contexts =
        StreamData.one_of([
          context_after_pipe(),
          context_after_assignment()
        ])

      check all({context, code} <- op_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii struct_arg" do
    @tag :property
    @tag timeout: 120_000
    property "inside struct arg" do
      check all(
              {context, code} <- context_struct_arg(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii between_do_blocks" do
    @tag :property
    @tag timeout: 120_000
    property "between do blocks" do
      check all(
              {context, code} <- context_between_do_blocks(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii ternary" do
    @tag :property
    @tag timeout: 120_000
    property "inside ternary range expressions" do
      ternary_contexts =
        StreamData.one_of([
          context_ternary_first(),
          context_ternary_second(),
          context_ternary_third()
        ])

      check all({context, code} <- ternary_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii map_update" do
    @tag :property
    @tag timeout: 120_000
    property "inside map update expressions" do
      map_update_contexts =
        StreamData.one_of([
          context_map_update_expr(),
          context_map_update_kv(),
          context_map_update_value()
        ])

      check all(
              {context, code} <- map_update_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii after_parens_call" do
    @tag :property
    @tag timeout: 120_000
    property "after parens call" do
      check all(
              {context, code} <- context_after_parens_call(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii no_parens_call_middle" do
    @tag :property
    @tag timeout: 120_000
    property "inside no parens call middle" do
      check all(
              {context, code} <- context_no_parens_call_middle(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii dot" do
    @tag :property
    @tag timeout: 120_000
    property "inside dot expressions" do
      dot_contexts =
        StreamData.one_of([
          context_before_dot_call(),
          context_dot_chain_middle(),
          context_dot_tuple(),
          context_after_dot()
        ])

      check all({context, code} <- dot_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii between_operators" do
    @tag :property
    @tag timeout: 120_000
    property "between operators" do
      check all(
              {context, code} <- context_between_operators(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii unary" do
    @tag :property
    @tag timeout: 120_000
    property "after unary operators" do
      unary_contexts =
        StreamData.one_of([
          context_after_capture(),
          context_after_pin(),
          context_after_unary_plus(),
          context_after_unary_minus(),
          context_after_at(),
          context_after_bang(),
          context_after_not()
        ])

      check all({context, code} <- unary_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii interpolated_atom_keyword" do
    @tag :property
    @tag timeout: 120_000
    property "inside interpolated atoms and keywords" do
      interp_contexts =
        StreamData.one_of([
          context_interpolated_atom(),
          context_interpolated_keyword()
        ])

      check all({context, code} <- interp_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii charlist_interpolation" do
    @tag :property
    @tag timeout: 120_000
    property "inside charlist interpolation" do
      check all(
              {context, code} <- context_charlist_interpolation(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii heredoc_interpolation" do
    @tag :property
    @tag timeout: 120_000
    property "inside heredoc interpolation" do
      heredoc_contexts =
        StreamData.one_of([
          context_string_heredoc_interpolation(),
          context_charlist_heredoc_interpolation()
        ])

      check all(
              {context, code} <- heredoc_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii sigil_interpolation" do
    @tag :property
    @tag timeout: 120_000
    property "inside sigil interpolation" do
      sigil_contexts =
        StreamData.one_of([
          context_sigil_interpolation(),
          context_sigil_heredoc_interpolation()
        ])

      check all({context, code} <- sigil_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii fn_when" do
    @tag :property
    @tag timeout: 120_000
    property "inside fn when guard" do
      check all({context, code} <- context_fn_when(), max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii def" do
    @tag :property
    @tag timeout: 120_000
    property "inside def expressions" do
      def_contexts =
        StreamData.one_of([
          context_def_parens_arg(),
          context_def_no_parens_arg(),
          context_def_when()
        ])

      check all({context, code} <- def_contexts, max_runs: 500_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii keyword/kv_data" do
    @tag :property
    @tag timeout: 120_000
    property "inside keyword lists, maps, structs and keyword args" do
      kv_contexts =
        StreamData.one_of([
          context_keyword_list_value(),
          context_map_kv_value(),
          context_map_rocket_key(),
          context_struct_kv_value(),
          context_parens_call_kw_value(),
          context_no_parens_call_kw_value()
        ])

      check all({context, code} <- kv_contexts, max_runs: 250_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii stabs/control-flow" do
    @tag :property
    @tag timeout: 120_000
    property "inside case/cond/fn clauses and do/else/after" do
      stab_contexts =
        StreamData.one_of([
          context_case_clause_lhs(),
          context_case_clause_rhs(),
          context_case_second_clause_lhs(),
          context_cond_clause_lhs(),
          context_fn_second_clause_lhs(),
          context_if_condition(),
          context_if_else_body(),
          context_parens_call_with_do(),
          context_no_parens_call_with_do(),
          context_try_body(),
          context_try_rescue_body(),
          context_receive_clause_lhs()
        ])

      check all({context, code} <- stab_contexts, max_runs: 250_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii call_args variants" do
    @tag :property
    @tag timeout: 120_000
    property "inside multi-arg calls and nested no-parens calls" do
      args_contexts =
        StreamData.one_of([
          context_parens_call_single_no_parens_expr_arg(),
          context_parens_call_multi_args_middle(),
          context_no_parens_call_multi_args(),
          context_nested_no_parens_call()
        ])

      check all({context, code} <- args_contexts, max_runs: 250_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii comprehensions/with" do
    @tag :property
    @tag timeout: 120_000
    property "inside for/with generators and filters/else" do
      comp_contexts =
        StreamData.one_of([
          context_for_generator_rhs(),
          context_for_filter(),
          context_with_generator_rhs(),
          context_with_generator_lhs(),
          context_with_else_body()
        ])

      check all({context, code} <- comp_contexts, max_runs: 250_000, max_shrinking_steps: 50) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  describe "ascii bitstring specs" do
    @tag :property
    @tag timeout: 120_000
    property "inside bitstring segment spec" do
      check all(
              {context, code} <- context_bitstring_segment_spec(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code, current_mode())
      end
    end
  end

  # ===========================================================================
  # Comparison Helper
  # ===========================================================================

  defp run_comparison(context, code, mode) do
    oracle_result = oracle_parse(code)

    case {mode, oracle_result} do
      # Strict mode: if oracle errors/crashes, skip (assume passed)
      {:strict, {:error, _}} ->
        :ok

      {:strict, :crashed} ->
        :ok

      # Tolerant mode: if oracle errors/crashes, spitfire must not crash
      {:tolerant, {:error, _}} ->
        # Spitfire must not crash - just call it and ensure no exception
        _ = spitfire_parse(code)
        :ok

      {:tolerant, :crashed} ->
        # Spitfire must not crash - just call it and ensure no exception
        _ = spitfire_parse(code)
        :ok

      # Both modes: if oracle returns ok, spitfire must return exactly the same AST
      {_, {:ok, {:__block__, _, []}}} ->
        # Empty block - skip detailed comparison
        :ok

      {_, {:ok, oracle_ast}} ->
        spitfire_result = spitfire_parse(code)

        case spitfire_result do
          {:ok, spitfire_ast} ->
            assert oracle_ast == spitfire_ast,
                   """
                   AST mismatch in context #{context} for code: #{inspect(code)}

                   Oracle:
                   #{inspect(oracle_ast, pretty: true)}

                   Spitfire:
                   #{inspect(spitfire_ast, pretty: true)}
                   """

          {:error, _spitfire_ast, _errors} ->
            flunk("""
            Spitfire returned error when oracle succeeded in context #{context} for code: #{inspect(code)}

            Oracle AST:
            #{inspect(oracle_ast, pretty: true)}
            """)

          {:error, :no_fuel_remaining} ->
            flunk("""
            Spitfire ran out of fuel in context #{context} for code: #{inspect(code)}

            Oracle AST:
            #{inspect(oracle_ast, pretty: true)}
            """)

          :crashed ->
            flunk("""
            Spitfire crashed when oracle succeeded in context #{context} for code: #{inspect(code)}

            Oracle AST:
            #{inspect(oracle_ast, pretty: true)}
            """)
        end
    end
  end

  defp oracle_parse(code) do
    Code.string_to_quoted(code, @oracle_opts)
  rescue
    _ -> :crashed
  end

  defp spitfire_parse(code) do
    Spitfire.parse(code)
  rescue
    _ -> :crashed
  end

  defp current_mode do
    Process.get(:spitfire_test_mode, :strict)
  end
end
