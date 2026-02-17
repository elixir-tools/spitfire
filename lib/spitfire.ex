defmodule Spitfire do
  @moduledoc """
  Spitfire parser
  """
  import Spitfire.Context
  import Spitfire.Tracer
  import Spitfire.While
  import Spitfire.While2

  require Logger

  @trace? Application.compile_env(:spitfire, :trace, false)

  defmodule NoFuelRemaining do
    @moduledoc false
    defexception message: """
                 The parser ran out of fuel!

                 This happens when the parser recurses too many times without consuming a new token,
                 and most likely indicates a bug in the parser.
                 """
  end

  # precedences

  # pratt parsers are top down operator precedence recursive descent parsers
  #
  # operators have precedence (also known as binding power in some literature) and have a direction, left or right
  #
  # precedences increment by 2s to account for the left and right binding power. when doing the calculation (as seen in parse_expression/2)
  # if an operator has a right binding power, then you subtract 1 before comparing.

  # an example to differentiate the two binding powers are to compare the plus and concat operators.
  #
  # the implicit parentheses in the following two expressions makes this concept clearer
  #
  # one + two + three => ((one + two) + three)
  #   and
  # one ++ two ++ three => (one ++ (two ++ three))
  #
  # this is also made evident by comparing the resulting AST

  # iex(1)> quote do
  # ...(1)> one + two + three
  # ...(1)> end
  # {:+, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
  #  [
  #    {:+, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
  #     [{:one, [], Elixir}, {:two, [], Elixir}]},
  #    {:three, [], Elixir}
  #  ]}
  # iex(2)> quote do
  # ...(2)> one ++ two ++ three
  # ...(2)> end
  # {:++, [context: Elixir, imports: [{2, Kernel}]],
  #  [
  #    {:one, [], Elixir},
  #    {:++, [context: Elixir, imports: [{2, Kernel}]],
  #     [{:two, [], Elixir}, {:three, [], Elixir}]}
  #  ]}

  @lowest {:left, 2}
  @doo {:left, 4}
  @stab_op {:right, 6}
  # list comma are commas inside tuples, maps, and lists, and function parameter/argument lists
  @list_comma {:left, 8}
  @in_match_op {:left, 10}
  @whenn {:right, 12}
  # comma are commas inside a right stab argument list
  @comma {:left, 14}
  @kw_identifier {:left, 16}
  @assoc_op {:right, 18}
  @type_op {:right, 20}
  @pipe_op {:right, 22}
  @capture_op {:left, 24}
  @match_op {:right, 26}
  @or_op {:left, 28}
  @and_op {:left, 30}
  @comp_op {:left, 32}
  @rel_op {:left, 34}
  @arrow_op {:left, 36}
  @in_op {:left, 38}
  @xor_op {:left, 40}
  @ternary_op {:right, 42}
  @concat_op {:right, 44}
  @range_op {:right, 44}
  @dual_op {:left, 48}
  @mult_op {:left, 50}
  @power_op {:left, 52}
  @left_paren {:left, 54}
  @unary_op {:left, 56}
  @left_bracket {:left, 58}
  @dot_call_op {:left, 60}
  @dot_op {:left, 62}
  @at_op {:left, 64}

  @precedences %{
    :"," => @comma,
    :. => @dot_call_op,
    :"(" => @left_paren,
    :"[" => @left_bracket,
    dot_call_op: @dot_call_op,
    do: @doo,
    kw_identifier: @kw_identifier,
    stab_op: @stab_op,
    in_match_op: @in_match_op,
    when_op: @whenn,
    type_op: @type_op,
    pipe_op: @pipe_op,
    assoc_op: @assoc_op,
    capture_op: @capture_op,
    match_op: @match_op,
    or_op: @or_op,
    and_op: @and_op,
    comp_op: @comp_op,
    rel_op: @rel_op,
    arrow_op: @arrow_op,
    in_op: @in_op,
    xor_op: @xor_op,
    ternary_op: @ternary_op,
    concat_op: @concat_op,
    range_op: @range_op,
    dual_op: @dual_op,
    mult_op: @mult_op,
    power_op: @power_op,
    unary_op: @unary_op,
    dot_op: @dot_op,
    at_op: @at_op
  }

  # Operators that indicate an identifier should be treated as a lone identifier (not a call)
  @no_parens_stop_operators [
    :|,
    :"=>",
    :->,
    :+,
    :**,
    :-,
    :/,
    :*,
    :|>,
    :++,
    :||,
    :&&,
    :and,
    :or,
    :range_op,
    :power_op,
    :stab_op,
    :xor_op,
    :rel_op,
    :and_op,
    :or_op,
    :mult_op,
    :arrow_op,
    :assoc_op,
    :pipe_op,
    :concat_op,
    :dual_op,
    :ternary_op,
    :in_op,
    :in_match_op,
    :comp_op,
    :match_op,
    :type_op,
    :dot_call_op,
    :when_op
  ]

  @peeks MapSet.new(
           [:";", :eol, :eof, :end, :",", :")", :do, :., :"}", :"]", :">>", :block_identifier] ++
             @no_parens_stop_operators
         )

  @spec parse(String.t(), Keyword.t()) :: {:ok, Macro.t()} | {:error, :no_fuel_remaining} | {:error, Macro.t(), list()}
  def parse(code, opts \\ []) do
    parser = code |> new(opts) |> next_token() |> next_token()

    # Save initial position for empty programs
    initial_meta = current_meta(parser)

    parser =
      while current_token(parser) in [:eol, :";"] <- parser do
        next_token(parser)
      end

    case parse_program(parser) do
      {{:__block__, [], []}, %{errors: []}} ->
        # Empty program - use initial position for metadata
        {:ok, {:__block__, initial_meta, []}}

      {ast, %{errors: []}} ->
        {:ok, ast}

      {ast, %{errors: errors}} ->
        {:error, ast, Enum.reverse(errors)}
    end
  rescue
    NoFuelRemaining ->
      {:error, :no_fuel_remaining}
  after
    Process.delete(:comma_list_parsers)
  end

  def parse!(code, opts \\ []) do
    case parse(code, opts) do
      {:ok, ast} ->
        ast

      {:error, :no_fuel_remaining} ->
        raise "No fuel remaining!"

      {:error, _ast, _errors} ->
        raise "Failed to parse!"
    end
  end

  def parse_with_comments(code, opts \\ []) do
    Process.put(:code_formatter_comments, [])

    opts = [{:preserve_comments, &preserve_comments/5} | opts]
    result = parse(code, opts)
    comments = Enum.reverse(Process.get(:code_formatter_comments))

    case result do
      {:ok, ast} -> {:ok, ast, comments}
      {:error, ast, errors} -> {:error, ast, comments, errors}
      {:error, :no_fuel_remaining} -> {:error, :no_fuel_remaining}
    end
  after
    Process.delete(:code_formatter_comments)
  end

  def container_cursor_to_quoted(code, opts \\ []) do
    opts =
      opts
      |> Keyword.put(:cursor_completion, true)
      |> Keyword.put(:emit_warnings, false)
      |> Keyword.put(:check_terminators, {:cursor, []})

    Spitfire.parse(code, opts)
  end

  defp parse_program(parser) do
    trace "parse_program", trace_meta(parser) do
      {exprs, parser} =
        while2 current_token(parser) != :eof <- parser do
          {ast, parser} = parse_expression(parser, @lowest, false, false, true)

          {ast, parser} =
            case ast do
              {:comma, meta, _} ->
                parser = put_error(parser, {meta, "unexpected token: ,"})
                {{:__block__, [{:error, true} | meta], []}, parser}

              _ ->
                {ast, parser}
            end

          parser =
            cond do
              match?({:__block__, [{:error, true} | _], _}, ast) ->
                next_token(parser)

              peek_token(parser) in [:eol, :";", :eof] and parser.tokens != :eot ->
                next_token(parser)

              current_token(parser) in [:")", :"]", :"}", :">>"] ->
                next_token(parser)

              peek_token(parser) in [:")", :"]", :"}", :">>"] ->
                next_token(parser)

              true ->
                parser
            end

          ast = push_eoe(ast, current_eoe(parser))

          {ast, eat_eoe(parser)}
        end

      exprs = build_block_nr(exprs)

      {exprs, parser}
    end
  end

  defp calc_prec(parser, associativity, precedence) do
    {_associativity, power} = peek_precedence(parser)

    precedence =
      case associativity do
        :left -> precedence
        :right -> precedence - 1
      end

    precedence < power
  end

  @terminals MapSet.new([:eol, :eof, :"}", :")", :"]", :">>", :block_identifier])
  @terminals_with_comma MapSet.put(@terminals, :",")
  defp parse_expression(parser, assoc, is_list, is_map, is_top)

  defp parse_expression(parser, {associativity, precedence}, is_list, is_map, is_top) do
    trace "parse_expression", trace_meta(parser) do
      parser = consume_fuel(parser)

      if is_map do
        with_context(parser, %{in_map: true}, fn parser ->
          do_parse_expression(parser, {associativity, precedence}, is_list, is_map, is_top)
        end)
      else
        do_parse_expression(parser, {associativity, precedence}, is_list, is_map, is_top)
      end
    end
  end

  defp do_parse_expression(parser, {associativity, precedence}, is_list, is_map, is_top) do
    stop_before_stab_op? = Map.get(parser, :stop_before_stab_op?, false)
    stop_before_map_op? = Map.get(parser, :stop_before_map_op?, false)

    prefix =
      case current_token_type(parser) do
        :identifier -> parse_identifier(parser)
        :do_identifier -> parse_do_identifier(parser)
        :paren_identifier -> parse_paren_identifier(parser)
        :bracket_identifier -> parse_lone_identifier(parser)
        :op_identifier -> parse_identifier(parser)
        :alias -> parse_alias(parser)
        :"<<" -> parse_bitstring(parser)
        :kw_identifier when is_list or is_map -> parse_kw_identifier(parser)
        :kw_identifier_safe when is_list or is_map -> parse_kw_identifier(parser)
        :kw_identifier_unsafe when is_list or is_map -> parse_kw_identifier(parser)
        :kw_identifier when not is_list and not is_map -> parse_bracketless_kw_list(parser)
        :kw_identifier_safe when not is_list and not is_map -> parse_bracketless_kw_list(parser)
        :kw_identifier_unsafe when not is_list and not is_map -> parse_bracketless_kw_list(parser)
        :int -> parse_int(parser)
        :flt -> parse_float(parser)
        :atom -> parse_atom(parser)
        :atom_quoted -> parse_atom(parser)
        :atom_safe -> parse_atom(parser)
        :atom_unsafe -> parse_atom(parser)
        true -> parse_boolean(parser)
        false -> parse_boolean(parser)
        :bin_string -> parse_string(parser)
        :bin_heredoc -> parse_string(parser)
        :list_string -> parse_string(parser)
        :list_heredoc -> parse_string(parser)
        :char -> parse_char(parser)
        :sigil -> parse_sigil(parser)
        :fn -> parse_anon_function(parser)
        :at_op -> parse_prefix_expression(parser)
        :unary_op -> parse_prefix_expression(parser)
        :capture_op -> parse_prefix_expression(parser)
        :dual_op -> parse_prefix_expression(parser)
        :capture_int -> parse_capture_int(parser)
        :stab_op -> parse_stab_expression(parser)
        :range_op -> parse_range_expression(parser)
        :ternary_op -> parse_ternary_prefix(parser)
        :"[" -> parse_list_literal(parser)
        :"(" -> parse_grouped_expression(parser)
        :"{" -> parse_tuple_literal(parser)
        :";" -> parse_unexpected_semicolon(parser)
        :%{} -> parse_map_literal(parser)
        :% -> parse_struct_literal(parser)
        :ellipsis_op -> parse_ellipsis_op(parser)
        nil -> parse_nil_literal(parser)
        _ -> nil
      end

    case prefix do
      {left, parser} ->
        terminals =
          if is_top do
            @terminals
          else
            @terminals_with_comma
          end

        {parser, is_valid} = validate_peek(parser, current_token_type(parser))

        if is_valid do
          while (not stab_state_set?(parser) and not MapSet.member?(terminals, peek_token(parser))) &&
                  (current_token(parser) != :do and peek_token(parser) != :eol) &&
                  (not stop_before_map_op? or
                     (peek_token_type(parser) != :assoc_op and
                        peek_token(parser) != :"=>")) &&
                  calc_prec(parser, associativity, precedence) <- {left, parser} do
            parser = consume_fuel(parser)
            peek_token_type = peek_token_type(parser)

            case peek_token_type do
              :match_op ->
                parse_infix_expression(next_token(parser), left)

              :when_op ->
                parse_infix_expression(next_token(parser), left)

              :pipe_op when is_map ->
                # When already inside map pairs (not first), treat | as infix operator
                if Map.get(parser, :inside_map_update_pairs, false) or Map.get(parser, :in_map_pairs, false) do
                  parse_infix_expression(next_token(parser), left)
                else
                  parse_pipe_op_in_map(next_token(parser), left)
                end

              :pipe_op ->
                parse_infix_expression(next_token(parser), left)

              :type_op ->
                parse_infix_expression(next_token(parser), left)

              :dual_op ->
                parse_infix_expression(next_token(parser), left)

              :mult_op ->
                parse_infix_expression(next_token(parser), left)

              :power_op ->
                parse_infix_expression(next_token(parser), left)

              :"[" ->
                parse_access_expression(next_token(parser), left)

              :concat_op ->
                parse_infix_expression(next_token(parser), left)

              :assoc_op ->
                parse_assoc_op(next_token(parser), left)

              :arrow_op ->
                parse_infix_expression(next_token(parser), left)

              :ternary_op ->
                parse_infix_expression(next_token(parser), left)

              :or_op ->
                parse_infix_expression(next_token(parser), left)

              :and_op ->
                parse_infix_expression(next_token(parser), left)

              :comp_op ->
                parse_infix_expression(next_token(parser), left)

              :rel_op ->
                parse_infix_expression(next_token(parser), left)

              :in_op ->
                parse_infix_expression(next_token(parser), left)

              :xor_op ->
                parse_infix_expression(next_token(parser), left)

              :in_match_op ->
                parse_infix_expression(next_token(parser), left)

              :range_op ->
                in_range = Map.get(parser, :in_range, false)
                parse_range_expression(next_token(parser), left, in_range)

              :stab_op when not stop_before_stab_op? ->
                parse_stab_expression(next_token(parser), left)

              :dot_call_op ->
                parse_dot_call_expression(next_token(parser), left)

              :"(" ->
                parse_call_expression(next_token(parser), left)

              :. ->
                parse_dot_expression(next_token(parser), left)

              :"," when is_top ->
                parse_comma(next_token(parser), left)

              :do when parser.nesting != 0 ->
                {left, next_token(parser)}

              :do ->
                parse_do_block(next_token(parser), left)

              _ when stop_before_stab_op? and peek_token_type == :stab_op ->
                parser = Map.put(parser, :stab_state, %{ast: left})
                # this will be ignored on the return
                {left, parser}

              _ ->
                {left, parser}
            end
          end
        else
          {left, parser}
        end

      nil ->
        meta = current_meta(parser)
        ctype = current_token_type(parser)
        parser = put_error(parser, {meta, "unknown token: #{ctype}"})

        parser =
          case ctype do
            :")" -> parser
            :"]" -> parser
            :"}" -> parser
            :">>" -> parser
            :end -> parser
            _ -> next_token(parser)
          end

        {{:__block__, [{:error, true} | meta], []}, parser}
    end
  end

  defp parse_grouped_expression(parser) do
    trace "parse_grouped_expression", trace_meta(parser) do
      opening_paren_meta = current_meta(parser)

      cond do
        peek_token(parser) == :eof ->
          meta = current_meta(parser)

          parser = put_error(parser, {meta, "missing closing parentheses"})

          {{:__block__, [{:error, true} | meta], []}, next_token(parser)}

        peek_token(parser) == :")" ->
          parser = parser |> next_token() |> eat_eoe()
          closing_paren_meta = current_meta(parser)
          {{:__block__, [parens: opening_paren_meta ++ [closing: closing_paren_meta]], []}, parser}

        true ->
          orig_meta = current_meta(parser)
          parser = next_token(parser)

          # Semicolons change which metadata format we use
          has_semicolon =
            current_token(parser) == :";" or
              (current_token(parser) == :eol and peek_token(parser) == :";")

          parser = eat_eoe(parser)
          old_nesting = parser.nesting

          parser = Map.put(parser, :nesting, 0)

          if current_token(parser) == :")" do
            closing_paren_meta = current_meta(parser)
            parser = Map.put(parser, :nesting, old_nesting)

            if has_semicolon do
              {{:__block__, [{:closing, closing_paren_meta} | opening_paren_meta], []}, parser}
            else
              {{:__block__, [parens: opening_paren_meta ++ [closing: closing_paren_meta]], []}, parser}
            end
          else
            # Stop at -> to allow stab expressions
            {expression, parser} = parse_stab_aware_expression(parser)

            # If stab_state was set, expression is already in stab_state.ast
            initial_stab_state = Map.get(parser, :stab_state)

            # Detect invalid `(a, b -> c, d)` pattern
            invalid_grouped_stab_trailing_comma? =
              match?({:comma, _, _}, expression) and peek_token(parser) == :->

            expression = push_eoe(expression, peek_eoe(parser))

            cond do
              # Next token is closing paren (possibly after eol/semicolons)
              peek_token(parser) == :")" || (peek_token(parser) in [:eol, :";"] && peek_token_skip_eoe(parser) == :")") ->
                parser =
                  parser
                  |> Map.put(:nesting, old_nesting)
                  |> next_token()
                  |> eat_eoe()

                closing_paren_meta = current_meta(parser)

                ast =
                  case expression do
                    # unquote_splicing with one arg is wrapped in a block
                    {:unquote_splicing, _, [_]} ->
                      {:__block__, [{:closing, current_meta(parser)} | orig_meta], [expression]}

                    # not/! with one arg is wrapped in a block
                    {op, _, [_]} when op in [:not, :!] ->
                      {:__block__, [], [expression]}

                    {:->, _, _} ->
                      [expression]

                    {f, meta, a} ->
                      {f, [parens: opening_paren_meta ++ [closing: closing_paren_meta]] ++ meta, a}

                    [{key, _value} | _] = kw when is_atom(key) ->
                      if Map.get(parser, :stop_before_stab_op?, false) do
                        {:__block__, [parens: opening_paren_meta ++ [closing: closing_paren_meta]], [kw]}
                      else
                        kw
                      end

                    [{{_, _, _}, _value} | _] = kw ->
                      if Map.get(parser, :stop_before_stab_op?, false) do
                        {:__block__, [parens: opening_paren_meta ++ [closing: closing_paren_meta]], [kw]}
                      else
                        kw
                      end

                    expression ->
                      expression
                  end

                {ast, parser}

              peek_token(parser) in [:eol, :";"] or current_token(parser) == :-> or
                stab_state_set?(parser) or peek_token(parser) == :-> ->
                # Multi-expression / stab loop
                {exprs, parser} =
                  while2 (current_token(parser) == :-> and peek_token(parser) != :")") ||
                           stab_state_set?(parser) || peek_token(parser) == :-> ||
                           (peek_token(parser) in [:eol, :";"] && parser |> next_token() |> peek_token() != :")") <-
                           parser do
                    {ast, parser} =
                      case Map.get(parser, :stab_state) do
                        %{ast: lhs} ->
                          parser =
                            if current_token(parser) != :-> do
                              next_token(Map.delete(parser, :stab_state))
                            else
                              Map.delete(parser, :stab_state)
                            end

                          {ast, parser} = parse_stab_expression(parser, lhs)

                          {ast, parser} =
                            cond do
                              current_token(parser) == :-> ->
                                {ast, parser}

                              peek_token(parser) == :")" ->
                                {ast, parser}

                              true ->
                                eoe = current_eoe(parser)
                                ast = push_eoe(ast, eoe)
                                {ast, next_token(parser)}
                            end

                          {ast, parser}

                        nil ->
                          parser = parser |> next_token() |> eat_eoe()
                          {ast, parser} = parse_stab_aware_expression(parser)

                          {ast, parser} =
                            cond do
                              stab_state_set?(parser) and not match?({:->, _, _}, ast) ->
                                {:filter, {nil, parser}}

                              current_token(parser) == :-> ->
                                {ast, parser}

                              peek_token(parser) == :")" ->
                                {ast, parser}

                              true ->
                                eoe = peek_eoe(parser)
                                ast = push_eoe(ast, eoe)
                                {ast, parser}
                            end

                          {ast, parser}
                      end

                    {ast, parser}
                  end

                # Skip trailing eol/semicolons before closing paren
                parser =
                  while peek_token(parser) in [:eol, :";"] <- parser do
                    next_token(parser)
                  end

                if peek_token(parser) == :")" do
                  parser =
                    parser
                    |> Map.put(:nesting, old_nesting)
                    |> next_token()

                  exprs =
                    if initial_stab_state != nil do
                      exprs
                    else
                      [expression | exprs]
                    end

                  ast =
                    case exprs do
                      [{:->, _, _} | _] ->
                        exprs

                      _ ->
                        {:__block__, [{:closing, current_meta(parser)} | orig_meta], exprs}
                    end

                  {ast, parser}
                else
                  if invalid_grouped_stab_trailing_comma? and peek_token(parser) == :"," do
                    meta = current_meta(parser)

                    parser =
                      parser
                      |> put_error({meta, "syntax error"})
                      |> Map.put(:nesting, old_nesting)

                    {{:__block__, [{:error, true} | meta], []}, next_token(parser)}
                  else
                    meta = current_meta(parser)

                    parser =
                      parser
                      |> put_error({meta, "missing closing parentheses"})
                      |> Map.put(:nesting, old_nesting)

                    {{:__block__, [{:error, true} | meta], []}, next_token(parser)}
                  end
                end

              true ->
                meta = current_meta(parser)

                parser =
                  parser
                  |> put_error({meta, "missing closing parentheses"})
                  |> Map.put(:nesting, old_nesting)

                {{:__block__, [{:error, true} | meta], []}, next_token(parser)}
            end
          end
      end
    end
  end

  defp parse_unexpected_semicolon(parser) do
    meta = current_meta(parser)
    parser = put_error(parser, {meta, "unexpected token: ;"})
    parser = parser |> next_token() |> eat_eol()

    case current_token_type(parser) do
      type when type in [:eof, :end, :block_identifier, :")", :"]", :"}", :">>"] ->
        {{:__block__, [{:error, true} | meta], []}, parser}

      _ ->
        {expr, parser} = parse_expression(parser, @lowest, false, false, true)
        {{:__block__, [{:error, true} | meta], [expr]}, parser}
    end
  end

  defp parse_nil_literal(%{current_token: {nil, meta}} = parser) do
    trace "parse_nil_literal", trace_meta(parser) do
      ast = encode_literal(parser, nil, meta)
      {ast, parser}
    end
  end

  defp map_kw_identifier_to_atom_token(:kw_identifier_safe), do: :atom_safe
  defp map_kw_identifier_to_atom_token(:kw_identifier_unsafe), do: :atom_unsafe

  defp parse_kw_identifier(%{current_token: {:kw_identifier, meta, token}} = parser) do
    trace "parse_kw_identifier", trace_meta(parser) do
      token = encode_literal(parser, token, meta)
      parser = parser |> next_token() |> eat_eoe()

      {expr, parser} =
        with_context(parser, %{in_map: false}, fn parser ->
          parse_expression(parser, @list_comma, false, false, false)
        end)

      {{token, expr}, parser}
    end
  end

  defp parse_kw_identifier(%{current_token: {type, meta, tokens}} = parser)
       when type in [:kw_identifier_safe, :kw_identifier_unsafe] do
    trace "parse_kw_identifier (#{type})", trace_meta(parser) do
      {atom, parser} = parse_atom(%{parser | current_token: {map_kw_identifier_to_atom_token(type), meta, tokens}})
      parser = parser |> next_token() |> eat_eoe()

      {expr, parser} =
        with_context(parser, %{in_map: false}, fn parser ->
          parse_expression(parser, @list_comma, false, false, false)
        end)

      atom =
        case atom do
          {t, meta, args} ->
            {delimiter, meta} = Keyword.pop(meta, :delimiter)
            meta = meta |> Keyword.put(:format, :keyword) |> Keyword.put(:delimiter, delimiter)
            {t, meta, args}
        end

      {{atom, expr}, parser}
    end
  end

  defp parse_bracketless_kw_list(%{current_token: {:kw_identifier, meta, token}} = parser) do
    trace "parse_bracketless_kw_list", trace_meta(parser) do
      token = encode_literal(parser, token, meta)
      parser = parser |> next_token() |> eat_eoe()

      {value, parser} = parse_expression(parser, @list_comma, false, false, false)

      {kvs, parser} =
        if stab_state_set?(parser), do: {[], parser}, else: parse_kw_list_continuation(parser)

      kw_list = [{token, value} | kvs]
      parser = maybe_widen_stab_state(parser, kw_list)

      {kw_list, parser}
    end
  end

  defp parse_bracketless_kw_list(%{current_token: {type, meta, tokens}} = parser)
       when type in [:kw_identifier_safe, :kw_identifier_unsafe] do
    trace "parse_bracketless_kw_list (#{type})", trace_meta(parser) do
      {atom, parser} = parse_atom(%{parser | current_token: {map_kw_identifier_to_atom_token(type), meta, tokens}})
      parser = parser |> next_token() |> eat_eoe()

      atom =
        case atom do
          {t, meta, args} ->
            {delimiter, meta} = Keyword.pop(meta, :delimiter)
            meta = meta |> Keyword.put(:format, :keyword) |> Keyword.put(:delimiter, delimiter)
            {t, meta, args}
        end

      {value, parser} = parse_expression(parser, @list_comma, false, false, false)

      {kvs, parser} =
        if stab_state_set?(parser), do: {[], parser}, else: parse_kw_list_continuation(parser)

      kw_list = [{atom, value} | kvs]
      parser = maybe_widen_stab_state(parser, kw_list)

      {kw_list, parser}
    end
  end

  defp parse_kw_list_continuation(parser) do
    while2 peek_token(parser) == :"," <- parser do
      parser = next_token(parser)

      # Trailing comma before closing delimiter
      case peek_token(parser) do
        delimiter when delimiter in [:")", :"]", :"}"] ->
          {:filter, {nil, parser}}

        _ ->
          parser = next_token(parser)

          case current_token_type(parser) do
            type when type in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe] ->
              parse_kw_identifier(parser)

            _ ->
              parser =
                if match?({:paren_identifier, _, :__cursor__}, parser.current_token) do
                  parser
                else
                  put_error(
                    parser,
                    {current_meta(parser),
                     "unexpected expression after keyword list. Keyword lists must always come as the last argument. " <>
                       "Therefore, this is not allowed:\n\n    function_call(1, some: :option, 2)\n\n" <>
                       "Instead, wrap the keyword in brackets:\n\n    function_call(1, [some: :option], 2)"}
                  )
                end

              parse_expression(parser, @kw_identifier, true, false, false)
          end
      end
    end
  end

  defp parse_assoc_op(%{current_token: {:assoc_op, _, _token}} = parser, key) do
    trace "parse_assoc_op", trace_meta(parser) do
      assoc_meta = current_meta(parser)

      # Reject unparenthesized multi-arg call as map key: `foo a, b => c`
      parser =
        if Map.get(parser, :in_map, false) and invalid_assoc_key_in_map?(key) do
          put_error(parser, {assoc_meta, "syntax error"})
        else
          parser
        end

      parser = parser |> next_token() |> eat_eoe()

      # Map values should parse as basic expressions. Keeping `in_map: true`
      # here applies key-specific precedence caps and can misparse constructs
      # like `for ... <- ... || [] do ... end` inside map values.
      {value, parser} =
        with_context(parser, %{in_map: false}, fn parser ->
          parse_expression(parser, @lowest, false, false, false)
        end)

      {:pair, pair} = normalize_assoc_key(key, value, assoc_meta)
      {pair, parser}
    end
  end

  defp add_assoc_meta({f, meta, args}, assoc_meta) when is_list(meta) and (is_list(args) or is_nil(args)) do
    {f, [{:assoc, assoc_meta} | meta], args}
  end

  defp add_assoc_meta(other, _assoc_meta), do: other

  defp normalize_assoc_key(key, value, assoc_meta) do
    key = add_assoc_meta(key, assoc_meta)
    {:pair, {key, value}}
  end

  defp(parse_comma_list(parser, precedence \\ @list_comma, is_list \\ false, is_map \\ false))

  defp parse_comma_list(parser, precedence, is_list, is_map) do
    trace "parse_comma_list", trace_meta(parser) do
      {front, parser} = parse_expression(parser, precedence, is_list, is_map, false)
      # we zip together the expression and parser state so that we can potentially
      # backtrack later
      Process.put(:comma_list_parsers, [parser])

      # After first expression in map, subsequent | should be infix not map update
      parser = if is_map, do: Map.put(parser, :in_map_pairs, true), else: parser

      {items, parser} =
        while2 peek_token(parser) == :"," <- parser do
          parser = next_token(parser)

          case peek_token(parser) do
            delimiter when delimiter in [:"]", :"}", :")", :">>"] ->
              {:filter, {nil, parser}}

            _ ->
              parser = next_token(parser)
              {item, parser} = parse_expression(parser, precedence, is_list, is_map, false)

              clp = Process.get(:comma_list_parsers)
              Process.put(:comma_list_parsers, [parser | clp])

              {item, parser}
          end
        end

      parser = if is_map, do: Map.delete(parser, :in_map_pairs), else: parser
      {[front | items], parser}
    end
  end

  defp invalid_assoc_call_meta?(meta) when is_list(meta) do
    not Keyword.has_key?(meta, :parens) and
      not Keyword.has_key?(meta, :closing) and
      not Keyword.has_key?(meta, :delimiter) and
      not Keyword.has_key?(meta, :do)
  end

  defp invalid_assoc_key_in_map?({name, meta, args}) when is_atom(name) and is_list(meta) and is_list(args) do
    arity = length(args)

    arity > 1 and
      not Macro.operator?(name, arity) and
      not Macro.special_form?(name, arity) and
      invalid_assoc_call_meta?(meta)
  end

  defp invalid_assoc_key_in_map?({{:., _, [_lhs, _rhs]}, meta, args}) when is_list(meta) and is_list(args) do
    length(args) > 1 and invalid_assoc_call_meta?(meta)
  end

  defp invalid_assoc_key_in_map?({{name, meta, args}, _value}) when is_atom(name) and is_list(meta) and is_list(args) do
    invalid_assoc_key_in_map?({name, meta, args})
  end

  defp invalid_assoc_key_in_map?(_), do: false

  defp parse_prefix_expression(parser) do
    trace "parse_prefix_expression", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)

      # Capture precedence before advancing
      precedence =
        if current_token_type(parser) == :dual_op do
          # dual ops are treated as unary ops when being used as a prefix operator
          @unary_op
        else
          current_precedence(parser)
        end

      parser = parser |> next_token() |> eat_eoe()
      rhs_parser = parser
      # In maps, cap precedence at assoc_op to prevent => from being consumed
      effective_precedence =
        if Map.get(parser, :in_map, false) do
          {_, prec} = precedence
          {_, assoc_prec} = @assoc_op
          # Use :left to prevent right-associative decrement
          if prec > assoc_prec, do: precedence, else: {:left, assoc_prec}
        else
          precedence
        end

      {rhs, parser} = parse_expression(parser, effective_precedence, false, false, false)

      {rhs, parser} =
        if unparenthesized_do_end_block?(rhs) do
          if Map.get(parser, :in_map, false) do
            with_context(rhs_parser, %{stop_before_map_op?: true}, fn parser ->
              parse_expression(parser, @lowest, false, false, false)
            end)
          else
            parse_expression(rhs_parser, @lowest, false, false, false)
          end
        else
          {rhs, parser}
        end

      ast = {token, meta, [rhs]}

      {ast, parser}
    end
  end

  defp unparenthesized_do_end_block?(ast) do
    case ast do
      {_, meta, _} when is_list(meta) ->
        Keyword.has_key?(meta, :do) && Keyword.has_key?(meta, :end) &&
          not Keyword.has_key?(meta, :parens)

      _ ->
        false
    end
  end

  # An expression is "unmatched" if it contains an unparenthesized do-end block
  # anywhere in its AST. Binary operators with an unmatched operand produce
  # unmatched expressions.
  defp unmatched_expr?({_, meta, args} = ast) when is_list(meta) do
    unparenthesized_do_end_block?(ast) or
      (is_list(args) and Enum.any?(args, &unmatched_expr?/1))
  end

  defp unmatched_expr?(_), do: false

  defp parse_struct_type_prefix(parser) do
    trace "parse_struct_type_prefix", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)
      parser = parser |> next_token() |> eat_eoe()
      parser = Map.delete(parser, :inside_map_update_pairs)
      {rhs, parser} = parse_struct_type(parser)
      {{token, meta, [rhs]}, parser}
    end
  end

  # Parse // followed by a lone identifier for struct types
  defp parse_ternary_prefix_lone_identifier(parser) do
    trace "parse_ternary_prefix_lone_identifier", trace_meta(parser) do
      first_meta = current_meta(parser)
      first_slash = {:/, first_meta, nil}

      parser = parser |> next_token() |> eat_eoe()

      second_meta = [line: first_meta[:line], column: first_meta[:column] + 1]

      {rhs, parser} = parse_lone_identifier(parser)

      ast = {:/, second_meta, [first_slash, rhs]}
      {ast, parser}
    end
  end

  # Parse ... followed by a lone identifier for struct types
  defp parse_ellipsis_lone_identifier(parser) do
    trace "parse_ellipsis_lone_identifier", trace_meta(parser) do
      meta = current_meta(parser)
      parser = parser |> next_token() |> eat_eoe()

      {rhs, parser} = parse_lone_identifier(parser)

      ast = {:..., meta, [rhs]}
      {ast, parser}
    end
  end

  defp parse_capture_int(parser) do
    trace "parse_capture_int", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)
      parser = next_token(parser)
      {encoder, parser} = Map.pop(parser, :literal_encoder)
      {rhs, parser} = parse_int(parser)
      parser = Map.put(parser, :literal_encoder, encoder)

      ast = {token, meta, [rhs]}

      {ast, parser}
    end
  end

  # """
  # A stab expression without a lhs is only possible as the argument to an anonymous function and in the typespect of an anon function

  # ```elixir
  # fn -> :ok end
  # @spec start_link((-> term), GenServer.options()) :: on_start
  # ```
  # """

  defp parse_stab_expression(parser) do
    trace "parse_stab_expression", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)

      newlines =
        case current_newlines(parser) do
          nil -> get_newlines(parser)
          nl -> [newlines: nl]
        end

      parser = eat_at(parser, [:eol, :";"], 1)
      old_nesting = parser.nesting
      parser = Map.put(parser, :nesting, 0)

      {exprs, parser} =
        while2 peek_token(parser) not in [:end, :eof, :")", :block_identifier] <- parser do
          parser = parser |> next_token() |> eat_eoe()
          {ast, parser} = parse_expression(parser, @lowest, false, false, true)

          eoe = peek_eoe(parser)

          parser = eat_eoe_at(parser, 1)

          ast = push_eoe(ast, eoe)

          {ast, parser}
        end

      rhs = build_stab_rhs(exprs)

      ast =
        {token, newlines ++ meta, [[], rhs]}

      parser = Map.put(parser, :nesting, old_nesting)

      {ast, parser}
    end
  end

  defp build_stab_rhs([]), do: nil
  defp build_stab_rhs(exprs), do: build_block_nr(exprs)

  # """
  # A stab expression with a lhs is present in case, cond, and try blocks, as well as macros and typespecs.

  # The rhs of a stab expression can be a single expression or a block of expressions. The end of the
  # block is denoted by either the `end` keyword or by the start of another "bare" stab expression.

  # ```elixir
  # case foo do
  #   :bar ->
  #     Some.thing()
  #     :ok

  #   :baz ->
  #     Another.thing()
  #     :error
  # end
  # ```
  # """

  defp parse_stab_expression(parser, lhs) do
    trace "parse_stab_expression (with lhs)", trace_meta(parser) do
      case current_token(parser) do
        :<- ->
          parse_infix_expression(parser, lhs)

        :-> ->
          token = current_token(parser)
          meta = current_meta(parser)

          newlines =
            case current_newlines(parser) do
              nil -> get_newlines(parser)
              nl -> [newlines: nl]
            end

          # Check if we have a semicolon right after -> (possibly after eol)
          # Check if there's a leading semicolon right after ->
          # Handles both ";expr" and "\n;expr" patterns
          has_leading_semicolon =
            case peek_token(parser) do
              :";" ->
                true

              :eol ->
                # Peek at the next token after eol without consuming
                # We need to manually check the token sequence
                with {:eol, _} <- parser.current_token,
                     # Look at the tokens list to find what comes after eol
                     [{:";", _} | _] <- parser.tokens do
                  true
                else
                  _ -> false
                end

              _ ->
                false
            end

          parser = eat_eoe_at(parser, 1)

          old_nesting = parser.nesting
          parser = Map.put(parser, :nesting, 0)

          {exprs, parser} =
            while2 not stab_state_set?(parser) and peek_token(parser) not in [:eof, :end, :")", :block_identifier, :->] <-
                     parser do
              parser = next_token(parser)

              # If we encounter a semicolon, it represents a nil expression
              {ast, parser} =
                if current_token_type(parser) == :";" do
                  {nil, eat_eoe(parser)}
                else
                  parse_stab_aware_expression(parser)
                end

              if stab_state_set?(parser) do
                {:filter, {nil, next_token(parser)}}
              else
                eoe = peek_eoe(parser)
                ast = push_eoe(ast, eoe)
                parser = eat_eoe_at(parser, 1)

                {ast, eat_eoe(parser)}
              end
            end

          # Filter out leaked :comma nodes from stab body
          {exprs, parser} =
            Enum.reduce(exprs, {[], parser}, fn
              {:comma, _meta, [first_arg | _] = _args}, {acc, parser} ->
                # Error location from first arg (:comma nodes have empty metadata)
                error_meta =
                  case first_arg do
                    {_, arg_meta, _} -> Keyword.take(arg_meta, [:line, :column])
                    _ -> []
                  end

                parser = put_error(parser, {error_meta, "syntax error"})
                {[{:__block__, [{:error, true} | error_meta], []} | acc], parser}

              other, {acc, parser} ->
                {[other | acc], parser}
            end)

          exprs = Enum.reverse(exprs)

          exprs = if has_leading_semicolon, do: [nil | exprs], else: exprs
          rhs = build_stab_rhs(exprs)
          meta = newlines ++ meta

          meta =
            case lhs do
              {:when, [{:parens, _parens} = paren_meta | _], _} ->
                [paren_meta | meta]

              {type, [{:parens, _parens} = paren_meta | _], _} when type in [:__block__, :comma, :when] ->
                [paren_meta | meta]

              _ ->
                meta
            end

          lhs =
            case lhs do
              {:__block__, _, []} ->
                []

              {:__block__, [{:parens, _} | _], [[{key, _} | _] = kw]} when is_atom(key) ->
                [kw]

              {:__block__, [{:parens, _} | _], [[{{_, _, _}, _} | _] = kw]} ->
                [kw]

              {:comma, _, lhs} ->
                lhs

              {:when, [{:parens, _} | when_meta], when_args} ->
                [{:when, when_meta, when_args}]

              {:__block__, [{:parens, _} = paren_meta | _], exprs} ->
                case exprs do
                  [[{key, _} | _] = kw] when is_atom(key) ->
                    [{:__block__, [paren_meta], [kw]}]

                  [[{{_, _, _}, _} | _] = kw] ->
                    [{:__block__, [paren_meta], [kw]}]

                  [expr] ->
                    [{:__block__, [paren_meta], [expr]}]

                  _ ->
                    lhs
                end

              lhs ->
                [lhs]
            end

          ast =
            {token, meta, [lhs, rhs]}

          parser = Map.put(parser, :nesting, old_nesting)

          {ast, eat_eoe(parser)}
      end
    end
  end

  # Widen stab_state when outer expression is more complete than when `->` was first detected.
  defp maybe_widen_stab_state(parser, ast) do
    case {Map.get(parser, :stab_state), ast} do
      {%{ast: _}, {:->, _, _}} ->
        parser

      {%{ast: inner_ast}, outer_ast} when inner_ast != outer_ast ->
        Map.put(parser, :stab_state, %{ast: outer_ast})

      _ ->
        parser
    end
  end

  defp stab_state_set?(parser), do: Map.get(parser, :stab_state) != nil

  # Parse expression in stab-aware context: `->` sets stab_state instead of
  # being consumed as infix. Auto-widens stab_state to the full outer expression.
  defp parse_stab_aware_expression(parser, precedence \\ @lowest, is_top \\ true) do
    {ast, parser} =
      with_context(parser, %{stop_before_stab_op?: true}, fn parser ->
        parse_expression(parser, precedence, false, false, is_top)
      end)

    parser = maybe_widen_stab_state(parser, ast)
    {ast, parser}
  end

  defp parse_comma(parser, lhs) do
    trace "parse_comma", trace_meta(parser) do
      parser = parser |> next_token() |> eat_eoe()
      {exprs, parser} = parse_comma_list(parser, @comma)

      {{:comma, [], [lhs | exprs]}, eat_eoe(parser)}
    end
  end

  defp parse_infix_expression(parser, lhs) do
    trace "parse_infix_expression", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)
      precedence = current_precedence(parser)

      effective_precedence =
        if Map.get(parser, :in_map, false) do
          {_, prec} = precedence
          {_, assoc_prec} = @assoc_op
          if prec > assoc_prec, do: precedence, else: {:left, assoc_prec}
        else
          precedence
        end

      # we save this in case the next expression is an error
      pre_parser = parser

      newlines =
        case current_newlines(parser) || peek_newlines(parser, :eol) do
          nil -> []
          nl -> [newlines: nl]
        end

      parser = parser |> next_token() |> eat_eoe()

      # For `when`, prevent -> from being consumed by nested no-parens calls
      # e.g., `() when bar 1, 2, 3 -> foo()` should parse `bar 1, 2, 3` as the guard
      {rhs, parser} =
        if token == :when do
          # Check if when has simple LHS (empty block or comma args).
          # If so and we're in fn context, use lower precedence to allow <- in guard.
          in_fn_context = Map.get(parser, :stop_before_stab_op?, false)
          simple_lhs = match?({:__block__, _, []}, lhs) or match?({:comma, _, _}, lhs)
          when_precedence = if in_fn_context and simple_lhs, do: @list_comma, else: effective_precedence

          {rhs, parser} =
            with_context(parser, %{stop_before_stab_op?: true}, fn parser ->
              parse_expression(parser, when_precedence, false, false, false)
            end)

          parser = Map.delete(parser, :stab_state)
          {rhs, parser}
        else
          parse_expression(parser, effective_precedence, false, false, false)
        end

      {rhs, parser} =
        case rhs do
          {:__block__, [{:error, true} | _], []} ->
            parser = put_error(pre_parser, {meta, "malformed right-hand side of #{token} operator"})
            {{:__block__, [{:error, true} | meta], []}, parser}

          _ ->
            {rhs, parser}
        end

      ast =
        case token do
          :"not in" ->
            {:not, meta, [{:in, meta, [lhs, rhs]}]}

          :in ->
            case lhs do
              {op, _meta, [inner]} when op in [:!, :not] ->
                in_ast = {:in, meta, [inner, rhs]}
                {op, meta, [in_ast]}

              _ ->
                {token, newlines ++ meta, [lhs, rhs]}
            end

          :when ->
            case lhs do
              {:__block__, [{:parens, _} = paren_meta | _], []} ->
                # () when ... - empty parens, keep parens meta
                {token, [paren_meta | newlines ++ meta], [rhs]}

              {:__block__, _, []} ->
                # Empty block without parens
                {token, newlines ++ meta, [rhs]}

              {:__block__, [{:parens, _} = paren_meta | _], [[{key, _} | _] = kw]} when is_atom(key) ->
                # (a: 1) when ... - preserve parens meta for stab
                {token, [paren_meta | newlines ++ meta], [kw, rhs]}

              {:__block__, [{:parens, _} = paren_meta | _], [[{{_, _, _}, _} | _] = kw]} ->
                # Parenthesized kw list with interpolated key
                {token, [paren_meta | newlines ++ meta], [kw, rhs]}

              {:comma, [{:parens, _} = paren_meta | _], args} ->
                {token, [paren_meta | newlines ++ meta], args ++ [rhs]}

              {:comma, _, args} ->
                {token, newlines ++ meta, args ++ [rhs]}

              _ ->
                {token, newlines ++ meta, [lhs, rhs]}
            end

          _ ->
            {token, newlines ++ meta, [lhs, rhs]}
        end

      {ast, parser}
    end
  end

  defp parse_pipe_op_in_map(parser, lhs) do
    trace "parse_pipe_op_in_map", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)

      newlines =
        case current_newlines(parser) || peek_newlines(parser, :eol) do
          nil -> []
          nl -> [newlines: nl]
        end

      rhs_parser = parser |> next_token() |> eat_eoe()

      if rhs_has_binding_op?(rhs_parser) or
           (unmatched_expr?(lhs) and rhs_has_bare_comma?(rhs_parser)) do
        # When the RHS of `|` has low-precedence operators (::, when, <-, \\) or
        # the LHS is an unmatched_expr (do-end) and the RHS has no-parens commas,
        # treat `|` as a regular pipe operator (matching Elixir's LALR grammar).
        parse_infix_expression(parser, lhs)
      else
        {pairs, pairs_parser} = parse_map_update_pairs(rhs_parser)
        ast = {token, newlines ++ meta, [lhs, pairs]}
        {ast, pairs_parser}
      end
    end
  end

  # Operators with precedence between assoc_op (18) and pipe_op (22) that
  # should NOT be consumed inside parse_map_update_pairs.
  @low_prec_map_op_types MapSet.new([:type_op, :when_op, :in_match_op])

  defp rhs_has_binding_op?(parser) do
    scan_binding_op(eat_eoe(parser), 0)
  end

  defp scan_binding_op(parser, nesting) do
    token = peek_token(parser)
    token_type = peek_token_type(parser)

    cond do
      MapSet.member?(@low_prec_map_op_types, token_type) and nesting == 0 ->
        true

      token_type == :assoc_op and nesting == 0 ->
        false

      token_type in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe] and nesting == 0 ->
        false

      token == :"}" and nesting == 0 ->
        false

      token == :"," and nesting == 0 ->
        false

      token == :eof ->
        false

      token in [:"(", :"[", :"{", :"<<"] ->
        scan_binding_op(next_token(parser), nesting + 1)

      token in [:")", :"]", :"}", :">>"] ->
        scan_binding_op(next_token(parser), max(nesting - 1, 0))

      token == :do ->
        skip_do_end_for_binding_op(next_token(parser), 1, nesting)

      true ->
        scan_binding_op(next_token(parser), nesting)
    end
  end

  defp skip_do_end_for_binding_op(parser, 0, nesting) do
    scan_binding_op(parser, nesting)
  end

  defp skip_do_end_for_binding_op(parser, depth, nesting) do
    case peek_token(parser) do
      :end -> skip_do_end_for_binding_op(next_token(parser), depth - 1, nesting)
      :do -> skip_do_end_for_binding_op(next_token(parser), depth + 1, nesting)
      :eof -> false
      _ -> skip_do_end_for_binding_op(next_token(parser), depth, nesting)
    end
  end

  defp rhs_has_bare_comma?(parser) do
    rhs_scan_comma_before_assoc(eat_eoe(parser), 0, false)
  end

  defp rhs_scan_comma_before_assoc(parser, nesting, saw_do_end) do
    token = peek_token(parser)
    token_type = peek_token_type(parser)

    cond do
      token == :"," and nesting == 0 ->
        not saw_do_end and not rhs_has_do_before_assoc?(next_token(parser), 0)

      token == :"}" and nesting == 0 ->
        false

      token == :eof ->
        false

      token_type == :assoc_op and nesting == 0 ->
        false

      token_type in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe] and nesting == 0 ->
        false

      token in [:"(", :"[", :"{", :"<<"] ->
        rhs_scan_comma_before_assoc(next_token(parser), nesting + 1, saw_do_end)

      token in [:")", :"]", :"}", :">>"] ->
        rhs_scan_comma_before_assoc(next_token(parser), max(nesting - 1, 0), saw_do_end)

      token == :do ->
        rhs_skip_do_end(next_token(parser), 1, nesting, true)

      true ->
        rhs_scan_comma_before_assoc(next_token(parser), nesting, saw_do_end)
    end
  end

  defp rhs_has_do_before_assoc?(parser, nesting) do
    token = peek_token(parser)
    token_type = peek_token_type(parser)

    cond do
      token == :do and nesting == 0 ->
        true

      token_type == :assoc_op and nesting == 0 ->
        false

      token_type in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe] and nesting == 0 ->
        false

      token == :"}" and nesting == 0 ->
        false

      token == :eof ->
        false

      token in [:"(", :"[", :"{", :"<<"] ->
        rhs_has_do_before_assoc?(next_token(parser), nesting + 1)

      token in [:")", :"]", :"}", :">>"] ->
        rhs_has_do_before_assoc?(next_token(parser), max(nesting - 1, 0))

      true ->
        rhs_has_do_before_assoc?(next_token(parser), nesting)
    end
  end

  defp rhs_skip_do_end(parser, 0, nesting, saw_do_end) do
    rhs_scan_comma_before_assoc(parser, nesting, saw_do_end)
  end

  defp rhs_skip_do_end(parser, depth, nesting, saw_do_end) do
    case peek_token(parser) do
      :end -> rhs_skip_do_end(next_token(parser), depth - 1, nesting, saw_do_end)
      :do -> rhs_skip_do_end(next_token(parser), depth + 1, nesting, saw_do_end)
      :eof -> false
      _ -> rhs_skip_do_end(next_token(parser), depth, nesting, saw_do_end)
    end
  end

  # Parses the RHS of a map update (after `|`). Inside here, `|` is treated
  # as a regular pipe operator (not a nested map update), matching the elixir
  # grammar where `assoc_update` only appears at the top level of `map_args`.
  defp parse_map_update_pairs(parser) do
    with_context(parser, %{inside_map_update_pairs: true}, fn parser ->
      {first, parser} = parse_expression(parser, @list_comma, false, true, false)

      {items, parser} =
        while2 peek_token(parser) == :"," <- parser do
          parser = next_token(parser)

          case peek_token(parser) do
            delimiter when delimiter in [:"}", :"]", :")", :">>"] ->
              {:filter, {nil, parser}}

            _ ->
              parser = parser |> next_token() |> eat_eoe()
              {item, parser} = parse_expression(parser, @list_comma, false, true, false)
              {item, parser}
          end
        end

      pairs = [first | items]
      pairs = Enum.reject(pairs, &is_nil/1)

      {pairs, parser}
    end)
  end

  defp parse_access_expression(parser, lhs) do
    trace "parse_access_expression", trace_meta(parser) do
      meta = current_meta(parser)

      # Capture newlines before eating them
      newlines =
        case peek_newlines(parser, :eol) do
          nil -> []
          nl -> [newlines: nl]
        end

      parser = parser |> next_token() |> eat_eol()

      {rhs, parser} = parse_expression(parser, @lowest, false, false, false)

      extra_meta = [from_brackets: true]

      parser = parser |> next_token() |> eat_eol()

      parser =
        if current_token(parser) == :"," do
          parser |> next_token() |> eat_eol()
        else
          parser
        end

      closing = current_meta(parser)
      meta = extra_meta ++ newlines ++ [{:closing, closing} | meta]

      ast = {{:., meta, [Access, :get]}, meta, [lhs, rhs]}

      {ast, parser}
    end
  end

  defp parse_range_expression(parser) do
    trace "parse_range_expression", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)
      {{token, meta, []}, parser}
    end
  end

  defp parse_range_expression(parser, lhs, inside_range) do
    trace "parse_range_expression (with lhs)", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)
      precedence = current_precedence(parser)

      newlines =
        case current_newlines(parser) || peek_newlines(parser, :eol) do
          nil -> []
          nl -> [newlines: nl]
        end

      parser = parser |> next_token() |> eat_eoe()

      old_in_range = Map.get(parser, :in_range, false)
      parser = Map.put(parser, :in_range, true)
      {rhs, parser} = parse_expression(parser, precedence, false, false, false)
      parser = Map.put(parser, :in_range, old_in_range)

      if not inside_range and peek_token_skip_eol(parser) == :ternary_op do
        parser = parser |> eat_eoe_at(1) |> next_token() |> next_token() |> eat_eoe()
        {rrhs, parser} = parse_expression(parser, precedence, false, false, false)

        {{:..//, newlines ++ meta, [lhs, rhs, rrhs]}, eat_eoe(parser)}
      else
        {{token, newlines ++ meta, [lhs, rhs]}, eat_eoe(parser)}
      end
    end
  end

  defp has_do_end_block?({_, meta, _}) when is_list(meta) do
    Keyword.has_key?(meta, :do) and Keyword.has_key?(meta, :end)
  end

  defp has_do_end_block?(_), do: false

  @binary_op_types [
    :and_op,
    :or_op,
    :comp_op,
    :rel_op,
    :arrow_op,
    :in_op,
    :xor_op,
    :ternary_op,
    :concat_op,
    :dual_op,
    :mult_op,
    :power_op,
    :match_op,
    :pipe_op,
    :assoc_op,
    :range_op
  ]

  defp is_binary_op?(type), do: type in @binary_op_types

  # Handle // as a prefix operator
  defp parse_ternary_prefix(parser) do
    trace "parse_ternary_prefix", trace_meta(parser) do
      first_meta = current_meta(parser)
      first_slash = {:/, first_meta, nil}

      parser = parser |> next_token() |> eat_eoe()

      second_meta = [line: first_meta[:line], column: first_meta[:column] + 1]

      # Parse RHS at power_op precedence
      {rhs, parser} = parse_expression(parser, @power_op, false, false, false)

      # If RHS has do-end block followed by an operator, continue parsing
      {rhs, parser} =
        if has_do_end_block?(rhs) and is_binary_op?(peek_token_type(parser)) do
          parser = next_token(parser)
          parse_infix_expression(parser, rhs)
        else
          {rhs, parser}
        end

      ast = {:/, second_meta, [first_slash, rhs]}
      {ast, parser}
    end
  end

  # Do Block Algorithm: The Movie
  #
  # A do block consists of the keyword `do`, following by 0 or more expressions
  # separated by newlines/semicolons, followed by 0 or more block identifier
  # (else, rescue, after, catch) + 0 or more expressions separated by newlines/semicolons
  # and concluded with the keyword `end`
  #
  # - beginning of parse function, current_token = :do
  # - encode `:do` literal in case of literal_encoder
  # - save the old nesting level and insert a 0
  # - enter outer loop
  #   - the job of the outer loop is to collect the expressions for each do+block_identifier
  #     (from now on just referred to as block_identifier)
  #   - else, start inner loop
  #     - each iteration of the loop continues if the peek token (while eating an eol) is not in end, block_identifier, or eof
  #     - increment the token, and eat the eol token
  #     - if the current token is end or a block_identifier, then the expression
  #       list is empty. return the expressions and end the iteration
  #     - else, parse the current expression
  #       - each iteration of the loop continues if the peek token is not in end, block_identifier, or eof
  #       - increment the token, and eat the eol token
  #       - if stab_state
  #         - we are in the body of a stab expression, don't increment and parse the stab
  #       - else
  #         - parse expression
  #         - push eoe of the next token, but don't actually increment the parser
  #     - end inner loop
  #   - encode block_identifier and save as {type, expressions}
  #   - end outer loop
  # - if current token is block_identifier, that means the last section was empty. encode the token
  #   and create an empty list of expressions
  # - assert peek token is end
  # - various clean up and metadata

  defp parse_do_block(%{current_token: {:do, meta}} = parser, lhs) do
    trace "parse_do_block", trace_meta(parser) do
      do_meta = current_meta(parser)
      type = encode_literal(parser, :do, meta)

      old_nesting = parser.nesting
      # Clear stop_before_stab_op? - do-blocks handle stab detection independently
      parser =
        parser
        |> Map.put(:nesting, 0)
        |> Map.delete(:stop_before_stab_op?)

      {exprs, {_, parser}} =
        while2 peek_token_eat_eoe(parser) not in [:end, :eof] <- {type, parser} do
          {exprs, parser} =
            while2 peek_token_eat_eoe(parser) not in [:end, :block_identifier, :eof] <- parser do
              {ast, parser} =
                case Map.get(parser, :stab_state) do
                  %{ast: lhs} ->
                    parser =
                      if current_token(parser) != :-> do
                        next_token(Map.delete(parser, :stab_state))
                      else
                        Map.delete(parser, :stab_state)
                      end

                    parse_stab_expression(parser, lhs)

                  nil ->
                    parser = parser |> next_token() |> eat_eoe()

                    if current_token_type(parser) == :stab_op do
                      parse_stab_expression(parser)
                    else
                      # Use stab-aware expression parsing to properly handle stabs in do-block contexts
                      {ast, parser} = parse_stab_aware_expression(parser, @lowest, true)

                      case Map.get(parser, :stab_state) do
                        %{ast: lhs} ->
                          parser =
                            if current_token(parser) != :-> do
                              next_token(Map.delete(parser, :stab_state))
                            else
                              Map.delete(parser, :stab_state)
                            end

                          parse_stab_expression(parser, lhs)

                        nil ->
                          {ast, parser}
                      end
                    end
                end

              temp_parser = next_token(parser)
              eoe = current_eoe(temp_parser)
              ast = push_eoe(ast, eoe)

              {ast, parser}
            end

          case peek_token_eat_eoe(parser) do
            :block_identifier ->
              parser = parser |> next_token() |> eat_eoe()
              {:block_identifier, meta, token} = parser.current_token
              {{type, exprs}, {encode_literal(parser, token, meta), parser}}

            _ ->
              {{type, exprs}, {type, parser}}
          end
        end

      extra_exprs =
        if current_token_type(parser) == :block_identifier do
          {:block_identifier, meta, token} = parser.current_token
          [{encode_literal(parser, token, meta), []}]
        else
          []
        end

      {parser, end_meta} =
        if peek_token_eat_eoe(parser) == :end do
          parser = parser |> next_token() |> eat_eoe()
          {parser, current_meta(parser)}
        else
          {put_error(parser, {do_meta, "missing `end` for do block"}), do_meta}
        end

      exprs = exprs ++ extra_exprs

      exprs =
        case exprs do
          [] -> [{type, []}]
          exprs -> exprs
        end

      exprs =
        for {type, expr} <- exprs do
          {type, build_block_nr(expr)}
        end

      ast =
        case lhs do
          {token, meta, nil} ->
            {token, [do: do_meta, end: end_meta] ++ meta, [exprs]}

          {token, meta, args} when is_list(args) ->
            # Remove no_parens when attaching do-block
            meta = Keyword.delete(meta, :no_parens)
            {token, [do: do_meta, end: end_meta] ++ meta, args ++ [exprs]}
        end

      parser = Map.put(parser, :nesting, old_nesting)
      {ast, parser}
    end
  end

  defp parse_dot_expression(parser, lhs) do
    trace "parse_dot_expression", trace_meta(parser) do
      token = current_token(parser)
      precedence = current_precedence(parser)
      meta = current_meta(parser)

      case peek_token_type(parser) do
        # if the next token is an open brace, we are in a multi alias situation `alias Foo.{Bar, Baz}`
        # technically the contents of the braces can be anything, so we parse them as anything
        :"{" ->
          dot_meta = current_meta(parser)
          parser = next_token(parser)
          newlines = get_newlines(parser)

          parser = parser |> next_token() |> eat_eoe()

          old_nesting = parser.nesting
          parser = Map.put(parser, :nesting, 0)

          # Handle empty braces
          {multis, parser} =
            if current_token(parser) == :"}" do
              {[], parser}
            else
              {multis, parser} = parse_comma_list(parser)
              parser = parser |> next_token() |> eat_eoe()
              {multis, parser}
            end

          parser = Map.put(parser, :nesting, old_nesting)

          # Empty braces omit closing metadata
          outer_meta =
            if multis == [] do
              dot_meta
            else
              newlines ++ [{:closing, current_meta(parser)} | dot_meta]
            end

          multis =
            {{:., dot_meta, [lhs, :{}]}, outer_meta, multis}

          {multis, parser}

        # if the next token is an alias, then we are in a dot chain of aliases, eg: __MODULE__.Foo
        :alias ->
          parser = next_token(parser)

          {{:__aliases__, ameta, aliases}, parser} = parse_alias(parser)

          last = ameta[:last]
          {{:__aliases__, [{:last, last} | meta], [lhs | aliases]}, parser}

        # if the next token is a bracket_identifier, then we know that the whole dot expression needs to be used as an argument for the access expression. eg, foo.bar[:baz]
        :bracket_identifier ->
          parser = next_token(parser)
          %{current_token: {:bracket_identifier, token_meta, rhs}} = parser

          ident_meta =
            parser
            |> current_meta()
            |> push_delimiter(token_meta)

          rhs = {{token, meta, [lhs, rhs]}, [no_parens: true] ++ ident_meta, []}

          parser = next_token(parser)
          {ast, parser} = parse_access_expression(parser, rhs)

          {ast, eat_eoe(parser)}

        :paren_identifier ->
          parser = next_token(parser)
          {{rhs, next_meta, args}, parser} = parse_expression(parser, precedence, false, false, false)

          args = if args == nil, do: [], else: args
          ast = {{token, meta, [lhs, rhs]}, next_meta, args}
          {ast, parser}

        type when type in [:identifier, :do_identifier, :op_identifier] ->
          parser = next_token(parser)
          %{current_token: {_, token_meta, rhs_name}} = parser

          ident_meta =
            parser
            |> current_meta()
            |> push_delimiter(token_meta)

          # Check if this is a lone identifier or a no-parens call
          lone? = type != :op_identifier && MapSet.member?(@peeks, peek_token(parser))

          if lone? do
            ast = {{token, meta, [lhs, rhs_name]}, [no_parens: true] ++ ident_meta, []}

            # Handle trailing do-block
            if parser.nesting == 0 && peek_token(parser) == :do do
              parse_do_block(next_token(parser), ast)
            else
              {ast, parser}
            end
          else
            # No-parens call with args
            parser = next_token(parser)
            parser = push_nesting(parser)
            in_map = Map.get(parser, :in_map, false)

            {first_arg, parser} =
              if in_map do
                with_context(parser, %{stop_before_map_op?: true}, fn parser ->
                  parse_expression(parser, @lowest, false, false, false)
                end)
              else
                parse_expression(parser, @lowest, false, false, false)
              end

            {rest_args, parser} =
              while2 peek_token(parser) == :"," <- parser do
                parser = parser |> next_token() |> next_token()

                if in_map do
                  with_context(parser, %{stop_before_map_op?: true}, fn parser ->
                    parse_expression(parser, @lowest, false, false, false)
                  end)
                else
                  parse_expression(parser, @lowest, false, false, false)
                end
              end

            args = [first_arg | rest_args]
            parser = pop_nesting(parser)

            # Handle trailing do-block
            dot_call = {{token, meta, [lhs, rhs_name]}, ident_meta, args}

            cond do
              parser.nesting == 0 && current_token(parser) == :do ->
                parse_do_block(parser, dot_call)

              parser.nesting == 0 && peek_token(parser) == :do ->
                parse_do_block(next_token(parser), dot_call)

              true ->
                # NOTE(doorgan): we don't add ambiguous_op for dot calls, only for regular identifier calls
                {dot_call, parser}
            end
          end

        _ ->
          parser = next_token(parser)
          next_meta = current_meta(parser)
          {rhs, parser} = parse_expression(parser, @lowest, false, false, false)
          ast = {{token, meta, [lhs, rhs]}, next_meta, []}

          {ast, parser}
      end
    end
  end

  defp parse_anon_function(%{current_token: {:fn, _}} = parser) do
    trace "parse_anon_function", trace_meta(parser) do
      meta = current_meta(parser)

      newlines = get_newlines(parser)
      parser = parser |> next_token() |> eat_eoe()

      # fn creates its own stab scope
      parser = Map.delete(parser, :stab_state)

      {exprs, parser} =
        with_context(parser, %{stop_before_stab_op?: true}, fn parser ->
          while2 current_token(parser) not in [:end, :eof] <- parser do
            {ast, parser} =
              case Map.get(parser, :stab_state) do
                %{ast: lhs} ->
                  parser =
                    if current_token(parser) != :-> do
                      next_token(Map.delete(parser, :stab_state))
                    else
                      Map.delete(parser, :stab_state)
                    end

                  {ast, parser} = parse_stab_expression(parser, lhs)

                  {ast, parser} =
                    cond do
                      # Next stab's LHS detected - stay at -> for next iteration
                      stab_state_set?(parser) ->
                        {ast, parser}

                      current_token(parser) == :-> and peek_token(parser) == :-> ->
                        parser = next_token(parser)
                        {ast, parser}

                      current_token(parser) == :-> and peek_token(parser) == :end ->
                        parser = next_token(parser)
                        {ast, parser}

                      current_token(parser) == :end and peek_token(parser) != :end ->
                        {ast, parser}

                      peek_token(parser) == :end ->
                        parser = next_token(parser)
                        {ast, parser}

                      true ->
                        eoe = current_eoe(parser)
                        ast = push_eoe(ast, eoe)
                        {ast, parser |> next_token() |> eat_eoe()}
                    end

                  {ast, parser}

                nil ->
                  if current_token(parser) == :-> do
                    {ast, parser} = parse_stab_expression(parser)

                    {ast, parser} =
                      cond do
                        current_token(parser) == :-> and peek_token(parser) == :end ->
                          parser = next_token(parser)
                          {ast, parser}

                        current_token(parser) == :-> ->
                          {ast, parser}

                        current_token(parser) == :end and peek_token(parser) != :end ->
                          {ast, parser}

                        peek_token(parser) == :end ->
                          parser = next_token(parser)
                          {ast, parser}

                        true ->
                          eoe = current_eoe(parser)
                          ast = push_eoe(ast, eoe)
                          {ast, parser |> next_token() |> eat_eoe()}
                      end

                    {ast, parser}
                  else
                    # -> will set stab_state via stop_before_stab_op?
                    {ast, parser} = parse_expression(parser, @lowest, false, false, true)
                    parser = maybe_widen_stab_state(parser, ast)

                    {ast, parser} =
                      cond do
                        stab_state_set?(parser) and not match?({:->, _, _}, ast) ->
                          parser =
                            if current_token(parser) == :end and peek_token(parser) == :-> do
                              next_token(parser)
                            else
                              parser
                            end

                          {:filter, {nil, parser}}

                        current_token(parser) == :-> ->
                          {ast, parser}

                        current_token(parser) == :end and peek_token(parser) != :end ->
                          {ast, parser}

                        peek_token(parser) == :end ->
                          parser = next_token(parser)
                          {ast, parser}

                        true ->
                          eoe = current_eoe(parser)
                          ast = push_eoe(ast, eoe)
                          {ast, parser |> next_token() |> eat_eoe()}
                      end

                    {ast, parser}
                  end
              end

            {ast, parser}
          end
        end)

      {parser, meta} =
        case current_token(parser) do
          :end ->
            {parser, [{:closing, current_meta(parser)} | meta]}

          _ ->
            {put_error(parser, {meta, "missing closing end for anonymous function"}), meta}
        end

      {{:fn, newlines ++ meta, exprs}, parser}
    end
  end

  defp parse_dot_call_expression(parser, lhs) do
    trace "parse_dot_call_expression", trace_meta(parser) do
      meta = current_meta(parser)
      parser = next_token(parser)
      newlines = get_newlines(parser)

      parser = eat_eoe(parser)

      if peek_token(parser) == :")" do
        parser = next_token(parser)
        closing = [closing: current_meta(parser)]
        ast = {{:., meta, [lhs]}, newlines ++ closing ++ meta, []}
        {ast, parser}
      else
        {pairs, parser} = parse_comma_list(parser |> next_token() |> eat_eoe())
        parser = parser |> next_token() |> eat_eoe()
        closing = [closing: current_meta(parser)]
        ast = {{:., meta, [lhs]}, newlines ++ closing ++ meta, pairs}

        {ast, parser}
      end
    end
  end

  defp parse_atom(%{current_token: {:atom, meta, atom}} = parser) do
    trace "parse_atom", trace_meta(parser) do
      atom = encode_literal(parser, atom, meta)
      {atom, parser}
    end
  end

  defp parse_atom(%{current_token: {:atom_quoted, meta, atom}} = parser) do
    trace "parse_atom (quoted)", trace_meta(parser) do
      atom = encode_literal(parser, atom, meta)
      {atom, parser}
    end
  end

  defp parse_atom(%{current_token: {type, token_meta, tokens}} = parser) when type in [:atom_safe, :atom_unsafe] do
    trace "parse_atom (#{type})", trace_meta(parser) do
      meta = current_meta(parser)
      {args, parser} = parse_interpolation(parser, tokens)

      binary_to_atom_op =
        case type do
          :atom_safe -> :binary_to_existing_atom
          :atom_unsafe -> :binary_to_atom
        end

      delimiter_meta = push_delimiter(meta, token_meta)

      {{{:., meta, [:erlang, binary_to_atom_op]}, delimiter_meta, [{:<<>>, meta, args}, :utf8]}, parser}
    end
  end

  defp parse_boolean(%{current_token: {bool, meta}} = parser) do
    trace "parse_boolean", trace_meta(parser) do
      bool = encode_literal(parser, bool, meta)

      {bool, parser}
    end
  end

  defp parse_int(%{current_token: {:int, {_, _, int} = meta, _}} = parser) do
    trace "parse_int", trace_meta(parser) do
      int = encode_literal(parser, int, meta)
      {int, parser}
    end
  end

  defp parse_float(%{current_token: {:flt, {_, _, float} = meta, _}} = parser) do
    trace "parse_float", trace_meta(parser) do
      float = encode_literal(parser, float, meta)
      {float, parser}
    end
  end

  defp parse_string(%{current_token: {:bin_heredoc, meta, _indent, [string]}} = parser) do
    trace "parse_string (bin_heredoc)", trace_meta(parser) do
      string = encode_literal(parser, string, meta)
      {string, parser}
    end
  end

  defp parse_string(%{current_token: {:list_heredoc, meta, _indent, [string]}} = parser) do
    trace "parse_string (list_heredoc)", trace_meta(parser) do
      string = encode_literal(parser, String.to_charlist(string), meta)
      {string, parser}
    end
  end

  defp parse_string(%{current_token: {:bin_heredoc, _meta, indentation, tokens}} = parser) do
    trace "parse_string (bin_heredoc w/interpolation)", trace_meta(parser) do
      meta = current_meta(parser)

      {args, parser} = parse_interpolation(parser, tokens)

      meta =
        if indentation != nil do
          [{:indentation, indentation} | meta]
        else
          meta
        end

      {{:<<>>, [{:delimiter, ~s|"""|} | meta], args}, parser}
    end
  end

  defp parse_string(%{current_token: {:list_heredoc, _meta, indentation, tokens}} = parser) do
    trace "parse_string (list_heredoc w/interpolation)", trace_meta(parser) do
      meta = current_meta(parser)

      args =
        for token <- tokens do
          case token do
            token when is_binary(token) ->
              token

            {{line, col, _}, {cline, ccol, _}, tokens} ->
              meta = [line: line, column: col]
              # construct a new parser
              ast =
                if tokens == [] do
                  {:__block__, [], []}
                else
                  parser =
                    %{
                      tokens: tokens ++ [:eof],
                      current_token: nil,
                      peek_token: nil,
                      nesting: 0,
                      fuel: 150,
                      errors: [],
                      literal_encoder: parser.literal_encoder
                    }
                    |> next_token()
                    |> next_token()

                  initial_meta = current_meta(parser)
                  parser = eat_eoe(parser)

                  if current_token(parser) == :eof do
                    {:__block__, initial_meta, []}
                  else
                    {exprs, _parser} =
                      while2 current_token(parser) != :eof <- parser do
                        {ast, parser} = parse_expression(parser, @lowest, false, false, true)

                        parser =
                          if peek_token(parser) in [:eol, :";", :eof] do
                            next_token(parser)
                          else
                            parser
                          end

                        ast = push_eoe(ast, current_eoe(parser))
                        {ast, eat_eoe(parser)}
                      end

                    build_block_nr(exprs)
                  end
                end

              {{:., meta, [Kernel, :to_string]}, [from_interpolation: true, closing: [line: cline, column: ccol]] ++ meta,
               [ast]}
          end
        end

      extra_meta =
        if indentation != nil do
          [indentation: indentation]
        else
          []
        end

      {{{:., meta, [List, :to_charlist]}, [{:delimiter, ~s|'''|} | extra_meta ++ meta], [args]}, parser}
    end
  end

  defp parse_string(%{current_token: {:bin_string, meta, [string]}} = parser) when is_binary(string) do
    trace "parse_string (bin_string)", trace_meta(parser) do
      string = encode_literal(parser, string, meta)
      {string, parser}
    end
  end

  defp parse_string(%{current_token: {:bin_string, _, tokens}} = parser) do
    trace "parse_string (bin_string w/interpolation)", trace_meta(parser) do
      meta = current_meta(parser)

      {args, parser} = parse_interpolation(parser, tokens)

      {{:<<>>, [{:delimiter, "\""} | meta], args}, parser}
    end
  end

  defp parse_string(%{current_token: {:list_string, meta, [string]}} = parser) do
    trace "parse_string (list_string)", trace_meta(parser) do
      string = encode_literal(parser, String.to_charlist(string), meta)
      {string, parser}
    end
  end

  defp parse_string(%{current_token: {:list_string, _, tokens}} = parser) do
    trace "parse_string (list_string w/interpolation)", trace_meta(parser) do
      meta = current_meta(parser)

      args =
        for token <- tokens do
          case token do
            token when is_binary(token) ->
              token

            {{line, col, _}, {cline, ccol, _}, tokens} ->
              meta = [line: line, column: col]
              # construct a new parser
              ast =
                if tokens == [] do
                  {:__block__, [], []}
                else
                  parser =
                    %{
                      tokens: tokens ++ [:eof],
                      current_token: nil,
                      peek_token: nil,
                      nesting: 0,
                      fuel: 150,
                      errors: [],
                      literal_encoder: parser.literal_encoder
                    }
                    |> next_token()
                    |> next_token()

                  initial_meta = current_meta(parser)
                  parser = eat_eoe(parser)

                  if current_token(parser) == :eof do
                    {:__block__, initial_meta, []}
                  else
                    {exprs, _parser} =
                      while2 current_token(parser) != :eof <- parser do
                        {ast, parser} = parse_expression(parser, @lowest, false, false, true)

                        parser =
                          if peek_token(parser) in [:eol, :";", :eof] do
                            next_token(parser)
                          else
                            parser
                          end

                        ast = push_eoe(ast, current_eoe(parser))
                        {ast, eat_eoe(parser)}
                      end

                    build_block_nr(exprs)
                  end
                end

              {{:., meta, [Kernel, :to_string]}, [from_interpolation: true, closing: [line: cline, column: ccol]] ++ meta,
               [ast]}
          end
        end

      {{{:., meta, [List, :to_charlist]}, [{:delimiter, "'"} | meta], [args]}, parser}
    end
  end

  defp parse_char(%{current_token: {:char, {_, _, _token} = meta, num}} = parser) do
    trace "parse_char", trace_meta(parser) do
      char = encode_literal(parser, num, meta)
      {char, parser}
    end
  end

  defp parse_sigil(%{current_token: {:sigil, _meta, token, tokens, mods, indentation, delimiter}} = parser) do
    trace "parse_sigil", trace_meta(parser) do
      meta = current_meta(parser)

      {args, parser} = parse_interpolation(parser, tokens)

      bs_meta =
        if indentation != nil do
          [{:indentation, indentation} | meta]
        else
          meta
        end

      ast = {token, Keyword.put(meta, :delimiter, delimiter), [{:<<>>, bs_meta, args}, mods]}
      {ast, parser}
    end
  end

  defp parse_alias(%{current_token: {:alias, _, alias}} = parser) do
    trace "parse_alias", trace_meta(parser) do
      meta = current_meta(parser)
      Process.put(:alias_last_meta, meta)

      {aliases, parser} =
        while2 peek_token(parser) == :. && peek_token(next_token(parser)) == :alias <- parser do
          parser = next_token(parser)

          case parser.peek_token do
            {:alias, _, alias} ->
              parser = next_token(parser)
              meta = current_meta(parser)
              Process.put(:alias_last_meta, meta)
              {alias, parser}
          end
        end

      aliases = [alias | aliases]

      {{:__aliases__, [{:last, Process.get(:alias_last_meta)} | meta], aliases}, parser}
    end
  after
    Process.delete(:alias_last_meta)
  end

  defp parse_bitstring(%{current_token: {:"<<", _}} = parser) do
    trace "parse_bitstring", trace_meta(parser) do
      meta = current_meta(parser)
      orig_parser = parser
      newlines = get_newlines(parser)
      parser = parser |> next_token() |> eat_eol()

      cond do
        current_token(parser) == :">>" ->
          {{:<<>>, newlines ++ [{:closing, current_meta(parser)} | meta], []}, parser}

        current_token(parser) in [:end, :"}", :")", :"]"] ->
          # if the current token is the wrong kind of ending delimiter, we revert to the previous parser
          # state, put an error, and inject a closing bracket to simulate a completed list
          parser = put_error(orig_parser, {meta, "missing closing brackets for bitstring"})

          parser = next_token(parser)

          parser =
            parser
            |> put_in([:current_token], {:fake_closing_brackets, nil})
            |> put_in([:peek_token], parser.current_token)
            |> update_in([:tokens], &[parser.peek_token | &1])

          {{:<<>>, [{:closing, current_meta(parser)} | meta], []}, parser}

        true ->
          old_comma_list_parsers = Process.get(:comma_list_parsers)
          {pairs, parser} = parse_comma_list(parser, @list_comma, true, false)

          case peek_token_eat_eoe(parser) do
            :">>" ->
              parser = eat_eol_at(parser, 1)
              parser = next_token(parser)

              {{:<<>>, newlines ++ [{:closing, current_meta(parser)} | meta], wrap_trailing_bitstring_keywords(pairs)},
               eat_eol(parser)}

            _ ->
              all_pairs = pairs |> Enum.reverse() |> Enum.zip(Process.get(:comma_list_parsers))

              {pairs, parser} =
                with [{potential_error, parser}, {item, parser_for_errors} | rest] <- all_pairs,
                     {:__block__, [{:error, true} | _], []} <- potential_error do
                  {[{item, parser} | rest],
                   parser
                   |> put_in([:current_token], {:fake_closing_bracket, nil})
                   |> put_in([:peek_token], parser.current_token)
                   |> put_in([:errors], parser_for_errors.errors)
                   |> update_in([:tokens], &[parser.peek_token | &1])}
                else
                  _ ->
                    parser = next_token(parser)

                    {all_pairs,
                     parser
                     |> put_in([:current_token], {:">>", nil})
                     |> put_in([:peek_token], parser.current_token)
                     |> update_in([:tokens], &[parser.peek_token | &1])}
                end

              Process.put(:comma_list_parsers, old_comma_list_parsers)

              parser = put_error(parser, {meta, "missing closing brackets for bitstring"})

              {pairs, _} = pairs |> Enum.reverse() |> Enum.unzip()

              {{:<<>>, newlines ++ [{:closing, current_meta(parser)} | meta],
                wrap_trailing_bitstring_keywords(List.wrap(pairs))}, parser}
          end
      end
    end
  end

  # Wrap trailing keyword pairs in bitstring args into a single list
  defp wrap_trailing_bitstring_keywords([]), do: []

  defp wrap_trailing_bitstring_keywords(pairs) do
    {keywords, non_keywords} =
      pairs
      |> Enum.reverse()
      |> Enum.split_while(&is_kw_pair?/1)

    case keywords do
      [] -> pairs
      _ -> Enum.reverse(non_keywords, [Enum.reverse(keywords)])
    end
  end

  defp is_kw_pair?({key, _value}) when is_atom(key), do: true
  defp is_kw_pair?(_), do: false

  defp parse_map_literal(%{current_token: {:%{}, _}} = parser) do
    trace "parse_map_literal", trace_meta(parser) do
      meta = current_meta(parser)
      parser = next_token(parser)
      newlines = peek_newlines(parser)

      parser = parser |> next_token() |> eat_eol()
      old_nesting = parser.nesting
      parser = Map.put(parser, :nesting, 0)

      cond do
        current_token(parser) == :"}" ->
          closing = current_meta(parser)
          parser = Map.put(parser, :nesting, old_nesting)

          extra =
            if newlines do
              [{:newlines, newlines}, {:closing, closing}]
            else
              [{:closing, closing}]
            end

          {{:%{}, extra ++ meta, []}, parser}

        peek_token(parser) == :eof ->
          parser = put_error(parser, {meta, "missing closing brace for map"})
          {{:%{}, meta, []}, parser}

        true ->
          # Clear inside_map_update_pairs so nested maps (e.g., %{outer | key: %{inner | k => v}})
          # treat their own `|` as a map update, not as a regular pipe from the outer context.
          {pairs, parser} =
            with_context(parser, %{inside_map_update_pairs: false}, fn parser ->
              parse_comma_list(parser, @list_comma, false, true)
            end)

          parser = eat_eol_at(parser, 1)

          parser =
            case peek_token(parser) do
              :"}" ->
                next_token(parser)

              _ ->
                put_error(parser, {current_meta(parser), "missing closing brace for map"})
            end

          closing = current_meta(parser)
          parser = Map.put(parser, :nesting, old_nesting)

          extra =
            if newlines do
              [{:newlines, newlines}, {:closing, closing}]
            else
              [{:closing, closing}]
            end

          {{:%{}, extra ++ meta, pairs}, parser}
      end
    end
  end

  defp parse_struct_type(parser) do
    trace "parse_struct_type", trace_meta(parser) do
      # structs can only have certain expressions to denote the type,
      # so we special case them here rather than parse an arbitrary expression

      {associativity, precedence} = @lowest

      prefix =
        case current_token_type(parser) do
          :identifier ->
            parse_lone_identifier(parser)

          :bracket_identifier ->
            parse_lone_identifier(parser)

          :paren_identifier ->
            parse_paren_identifier(parser)

          :alias ->
            parse_alias(parser)

          :at_op ->
            parse_lone_module_attr(parser)

          :unary_op ->
            parse_struct_type_prefix(parser)

          :dual_op ->
            parse_struct_type_prefix(parser)

          :capture_op ->
            parse_struct_type_prefix(parser)

          :capture_int ->
            parse_capture_int(parser)

          :ternary_op ->
            parse_ternary_prefix_lone_identifier(parser)

          :ellipsis_op ->
            parse_ellipsis_lone_identifier(parser)

          :range_op ->
            parse_range_expression(parser)

          :sigil ->
            parse_sigil(parser)

          :list_string ->
            parse_string(parser)

          :bin_string ->
            parse_string(parser)

          :int ->
            parse_int(parser)

          :flt ->
            parse_float(parser)

          :char ->
            parse_char(parser)

          true ->
            parse_boolean(parser)

          false ->
            parse_boolean(parser)

          nil ->
            parse_nil_literal(parser)

          :atom ->
            parse_atom(parser)

          :atom_quoted ->
            parse_atom(parser)

          :%{} ->
            parse_map_literal(parser)

          :% ->
            parse_struct_literal(parser)

          :"(" ->
            parse_grouped_expression(parser)

          _ ->
            nil
        end

      case prefix do
        {left, parser} ->
          while peek_token(parser) in [:., :dot_call_op, :"[", :"("] &&
                  calc_prec(parser, associativity, precedence) <- {left, parser} do
            case peek_token(parser) do
              token when token in [:., :dot_call_op] ->
                {new_left, parser} = parse_dot_for_struct_type(next_token(parser), left)
                # Check if next token is ( for zero-arity calls
                if peek_token(parser) == :"(" do
                  parser = next_token(parser)

                  if current_token(parser) == :")" do
                    closing = current_meta(parser)
                    new_left = {new_left, [{:closing, closing}, {:line, 1}, {:column, 3}], []}
                    {new_left, next_token(parser)}
                  else
                    {new_left, parser}
                  end
                else
                  {new_left, parser}
                end

              :"[" ->
                parse_access_expression(next_token(parser), left)

              :"(" ->
                # Handle () after a dot expression
                parser = next_token(parser)

                if current_token(parser) == :")" do
                  closing = current_meta(parser)
                  {{:., current_meta(parser), [left]}, [{:closing, closing}], []}
                else
                  {left, parser}
                end
            end
          end

        nil ->
          meta = current_meta(parser)
          ctype = current_token_type(parser)
          parser = put_error(parser, {meta, "unknown token: #{ctype}"})

          parser =
            case ctype do
              :")" -> parser
              :"]" -> parser
              :"}" -> parser
              :">>" -> parser
              :end -> parser
              _ -> next_token(parser)
            end

          {{:__block__, [], []}, parser}
      end
    end
  end

  # Dot expression for struct types - never parses call arguments.
  defp parse_dot_for_struct_type(parser, lhs) do
    trace "parse_dot_for_struct_type", trace_meta(parser) do
      token = current_token(parser)
      meta = current_meta(parser)

      # For dot_call_op, we need to handle the () specially
      current_type = current_token_type(parser)

      if current_type == :dot_call_op do
        parse_dot_call_expression(parser, lhs)
      else
        case peek_token_type(parser) do
          :alias ->
            parser = next_token(parser)
            {{:__aliases__, ameta, aliases}, parser} = parse_alias(parser)
            last = ameta[:last]
            {{:__aliases__, [{:last, last} | meta], [lhs | aliases]}, parser}

          type when type in [:identifier, :do_identifier, :op_identifier] ->
            parser = next_token(parser)
            %{current_token: {_, token_meta, rhs_name}} = parser

            ident_meta =
              parser
              |> current_meta()
              |> push_delimiter(token_meta)

            # Check if there's a () after the identifier for zero-arity calls
            if peek_token(parser) == :"(" do
              parser = next_token(parser)

              if current_token(parser) == :")" do
                closing = current_meta(parser)
                ast = {{token, meta, [lhs, rhs_name]}, [{:closing, closing}] ++ ident_meta, []}
                {ast, next_token(parser)}
              else
                # Shouldn't happen in valid syntax, but handle gracefully
                ast = {{token, meta, [lhs, rhs_name]}, [no_parens: true] ++ ident_meta, []}
                {ast, parser}
              end
            else
              ast = {{token, meta, [lhs, rhs_name]}, [no_parens: true] ++ ident_meta, []}
              {ast, parser}
            end

          _ ->
            parser = next_token(parser)
            next_meta = current_meta(parser)
            {rhs, parser} = parse_expression(parser, @lowest, false, false, false)
            ast = {{token, meta, [lhs, rhs]}, next_meta, []}
            {ast, parser}
        end
      end
    end
  end

  defp parse_ellipsis_op(parser) do
    trace "parse_ellipsis_op", trace_meta(parser) do
      peek = peek_token_type(parser)

      # `...` is standalone when followed by a terminal, stab op, keyword
      # or binary operators (except :dual_op)
      if MapSet.member?(@terminals_with_comma, peek_token(parser)) or
           peek_token(parser) == :";" or
           peek in [:stab_op, :do, :end, :block_identifier] or
           (is_binary_op?(peek) and peek != :dual_op) do
        {{:..., current_meta(parser), []}, parser}
      else
        meta = current_meta(parser)
        parser = next_token(parser)
        rhs_parser = parser
        {rhs, parser} = parse_expression(parser, @capture_op, false, false, false)

        {rhs, parser} =
          if unparenthesized_do_end_block?(rhs) do
            if Map.get(parser, :in_map, false) do
              with_context(rhs_parser, %{stop_before_map_op?: true}, fn parser ->
                parse_expression(parser, @lowest, false, false, false)
              end)
            else
              parse_expression(rhs_parser, @lowest, false, false, false)
            end
          else
            {rhs, parser}
          end

        {{:..., meta, [rhs]}, parser}
      end
    end
  end

  # Formats a struct type AST to a string for error messages
  defp format_struct_type({:__aliases__, _, parts}) do
    Enum.map_join(parts, ".", fn
      part when is_atom(part) -> Atom.to_string(part)
      {:__MODULE__, _, _} -> "__MODULE__"
      {name, _, _} when is_atom(name) -> Atom.to_string(name)
      _ -> "?"
    end)
  end

  defp format_struct_type({:@, _, [inner]}) do
    case format_struct_type(inner) do
      nil -> nil
      s -> "@#{s}"
    end
  end

  defp format_struct_type({{:., _, [left, right]}, _, _}) do
    left_str = format_struct_type(left)
    right_str = if is_atom(right), do: Atom.to_string(right)
    if left_str && right_str, do: "#{left_str}.#{right_str}"
  end

  defp format_struct_type({:., _, [left, right]}) do
    left_str = format_struct_type(left)
    right_str = if is_atom(right), do: Atom.to_string(right)
    if left_str && right_str, do: "#{left_str}.#{right_str}"
  end

  defp format_struct_type({name, _, _}) when is_atom(name) do
    Atom.to_string(name)
  end

  defp format_struct_type(_), do: nil

  defp parse_struct_literal(%{current_token: {:%, _}} = parser) do
    trace "parse_struct_literal", trace_meta(parser) do
      meta = current_meta(parser)
      parser = next_token(parser)
      {type, parser} = parse_struct_type(parser)

      valid_type? = type != {:__block__, [], []}
      struct_name = format_struct_type(type)

      case peek_token_eat_eoe(parser) do
        :"{" ->
          parser = eat_eol(next_token(parser))
          brace_meta = current_meta(parser)
          parser = next_token(parser)

          newlines =
            case current_newlines(parser) do
              nil -> []
              nl -> [newlines: nl]
            end

          parser = eat_eol(parser)
          old_nesting = parser.nesting
          parser = Map.put(parser, :nesting, 0)

          if current_token(parser) == :"}" do
            closing = current_meta(parser)
            ast = {:%, meta, [type, {:%{}, newlines ++ [{:closing, closing} | brace_meta], []}]}
            parser = Map.put(parser, :nesting, old_nesting)
            {ast, parser}
          else
            {pairs, parser} =
              with_context(parser, %{inside_map_update_pairs: false}, fn parser ->
                parse_comma_list(parser, @list_comma, false, true)
              end)

            parser = eat_eol_at(parser, 1)

            parser =
              case peek_token(parser) do
                :"}" -> next_token(parser)
                _ -> put_error(parser, {current_meta(parser), "missing closing brace for struct %#{struct_name}"})
              end

            closing = current_meta(parser)
            ast = {:%, meta, [type, {:%{}, newlines ++ [{:closing, closing} | brace_meta], pairs}]}
            parser = Map.put(parser, :nesting, old_nesting)
            {ast, parser}
          end

        token when token in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe, :identifier] and valid_type? ->
          parser = put_error(parser, {current_meta(parser), "missing opening brace for struct %#{struct_name}"})
          parser = next_token(parser)
          brace_meta = current_meta(parser)

          old_nesting = parser.nesting
          parser = Map.put(parser, :nesting, 0)

          {pairs, parser} = parse_comma_list(parser, @list_comma, false, true)
          parser = eat_eol_at(parser, 1)

          {parser, closing_meta} =
            case peek_token(parser) do
              :"}" ->
                parser = next_token(parser)
                {parser, [{:closing, current_meta(parser)} | brace_meta]}

              _ ->
                parser = put_error(parser, {current_meta(parser), "missing closing brace for struct %#{struct_name}"})
                {parser, brace_meta}
            end

          ast = {:%, meta, [type, {:%{}, closing_meta, pairs}]}
          parser = Map.put(parser, :nesting, old_nesting)
          {ast, parser}

        _ ->
          parser =
            if valid_type?,
              do: put_error(parser, {current_meta(parser), "missing opening brace for struct %#{struct_name}"}),
              else: parser

          {{:%, meta, [type, {:%{}, [], []}]}, parser}
      end
    end
  end

  defp parse_tuple_literal(%{current_token: {:"{", orig_meta}} = parser) do
    trace "parse_tuple_literal", trace_meta(parser) do
      meta = current_meta(parser)
      orig_parser = parser
      newlines = peek_newlines(parser)

      parser = parser |> next_token() |> eat_eol()
      old_nesting = parser.nesting
      parser = Map.put(parser, :nesting, 0)

      cond do
        current_token(parser) == :"}" ->
          closing = current_meta(parser)
          parser = Map.put(parser, :nesting, old_nesting)

          extra =
            if newlines do
              [{:newlines, newlines}, {:closing, closing}]
            else
              [{:closing, closing}]
            end

          {{:{}, extra ++ meta, []}, parser}

        current_token(parser) in [:end, :"]", :")", :">>", :eof] ->
          # if the current token is the wrong kind of ending delimiter, we revert to the previous parser
          # state, put an error, and inject a closing brace to simulate a completed tuple
          parser = put_error(orig_parser, {meta, "missing closing brace for tuple"})

          parser = next_token(parser)

          parser =
            parser
            |> put_in([:current_token], {:fake_closing_brace, nil})
            |> put_in([:peek_token], parser.current_token)
            |> update_in([:tokens], &[parser.peek_token | &1])

          parser = put_in(parser.nesting, old_nesting)
          {{:{}, meta, []}, parser}

        true ->
          old_comma_list_parsers = Process.get(:comma_list_parsers)
          {pairs, parser} = parse_comma_list(parser)

          {pairs, parser} =
            case peek_token_eat_eoe(parser) do
              :"}" ->
                parser = eat_eol_at(parser, 1)
                {pairs, parser |> next_token() |> eat_eol()}

              _ ->
                all_pairs = pairs |> Enum.reverse() |> Enum.zip(Process.get(:comma_list_parsers))

                {pairs, parser} =
                  with [{potential_error, parser}, {item, parser_for_errors} | rest] <- all_pairs,
                       {:__block__, [{:error, true} | _], []} <- potential_error do
                    {[{item, parser} | rest],
                     parser
                     |> put_in([:current_token], {:fake_closing_brace, nil})
                     |> put_in([:peek_token], parser.current_token)
                     |> put_in([:errors], parser_for_errors.errors)
                     |> update_in([:tokens], &[parser.peek_token | &1])}
                  else
                    _ ->
                      parser = next_token(parser)

                      {all_pairs,
                       parser
                       |> put_in([:current_token], {:"}", nil})
                       |> put_in([:peek_token], parser.current_token)
                       |> update_in([:tokens], &[parser.peek_token | &1])}
                  end

                Process.put(:comma_list_parsers, old_comma_list_parsers)

                parser = put_error(parser, {meta, "missing closing brace for tuple"})

                {pairs, _} = Enum.unzip(pairs)

                {Enum.reverse(pairs), parser}
            end

          if length(pairs) == 2 do
            tuple = encode_literal(parser, pairs |> List.wrap() |> List.to_tuple(), orig_meta)
            parser = Map.put(parser, :nesting, old_nesting)
            {tuple, parser}
          else
            closing = current_meta(parser)
            parser = Map.put(parser, :nesting, old_nesting)

            extra =
              if newlines do
                [{:newlines, newlines}, {:closing, closing}]
              else
                [{:closing, closing}]
              end

            {{:{}, extra ++ meta, List.wrap(pairs)}, parser}
          end
      end
    end
  end

  defp parse_list_literal(%{current_token: {:"[", orig_meta}} = parser) do
    trace "parse_list_literal", trace_meta(parser) do
      meta = current_meta(parser)
      orig_parser = parser
      parser = parser |> next_token() |> eat_eol()
      old_nesting = parser.nesting
      parser = Map.put(parser, :nesting, 0)

      cond do
        current_token(parser) == :"]" ->
          parser = Map.put(parser, :nesting, old_nesting)
          {encode_literal(parser, [], orig_meta), parser}

        current_token(parser) in [:end, :"}", :")", :">>", :eof] ->
          # if the current token is the wrong kind of ending delimiter, we revert to the previous parser
          # state, put an error, and inject a closing bracket to simulate a completed list
          parser = put_error(orig_parser, {meta, "missing closing bracket for list"})

          parser = next_token(parser)

          parser =
            parser
            |> put_in([:current_token], {:fake_closing_bracket, nil})
            |> put_in([:peek_token], parser.current_token)
            |> update_in([:tokens], &[parser.peek_token | &1])

          parser = Map.put(parser, :nesting, old_nesting)
          {encode_literal(parser, [], orig_meta), parser}

        true ->
          old_comma_list_parsers = Process.get(:comma_list_parsers)
          {pairs, parser} = parse_comma_list(parser, @list_comma, true, false)

          # parser = eat_eoe_at(parser, 1)

          case peek_token_eat_eoe(parser) do
            :"]" ->
              parser = eat_eol_at(parser, 1)
              parser = Map.put(parser, :nesting, old_nesting)
              {encode_literal(parser, pairs, orig_meta), next_token(parser)}

            _ ->
              all_pairs = pairs |> Enum.reverse() |> Enum.zip(Process.get(:comma_list_parsers))

              {pairs, parser} =
                with [{potential_error, parser}, {item, parser_for_errors} | rest] <- all_pairs,
                     {:__block__, [{:error, true} | _meta], []} <- potential_error do
                  {[{item, parser} | rest],
                   parser
                   |> put_in([:current_token], {:fake_closing_bracket, nil})
                   |> put_in([:peek_token], parser.current_token)
                   |> put_in([:errors], parser_for_errors.errors)
                   |> update_in([:tokens], &[parser.peek_token | &1])}
                else
                  _ ->
                    parser = next_token(parser)

                    {all_pairs,
                     parser
                     |> put_in([:current_token], {:"]", nil})
                     |> put_in([:peek_token], parser.current_token)
                     |> update_in([:tokens], &[parser.peek_token | &1])}
                end

              Process.put(:comma_list_parsers, old_comma_list_parsers)

              parser = put_error(parser, {meta, "missing closing bracket for list"})

              {pairs, _} = Enum.unzip(pairs)

              pairs = Enum.reverse(pairs)
              parser = Map.put(parser, :nesting, old_nesting)
              {encode_literal(parser, pairs, orig_meta), parser}
          end
      end
    end
  end

  defp parse_paren_identifier(%{current_token: {:paren_identifier, token_meta, token}} = parser) do
    trace "parse_paren_identifier", trace_meta(parser) do
      meta =
        parser
        |> current_meta()
        |> push_delimiter(token_meta)

      parser = next_token(parser)
      newlines = get_newlines(parser)
      error_meta = current_meta(parser)

      cond do
        peek_token(parser) == :eof ->
          closing = current_meta(parser)
          parser = put_error(parser, {closing, "missing closing parentheses"})
          {{token, newlines ++ [{:closing, closing} | meta], []}, parser}

        peek_token(parser) == :")" ->
          parser = next_token(parser)
          closing = current_meta(parser)
          ast = {token, newlines ++ [{:closing, closing} | meta], []}

          if peek_token(parser) == :do and parser.nesting == 0 do
            parser = next_token(parser)
            parse_do_block(parser, ast)
          else
            {ast, parser}
          end

        true ->
          old_nesting = parser.nesting
          parser = Map.put(parser, :nesting, 0)

          parser =
            parser
            |> next_token()
            |> eat_eol()

          if current_token(parser) == :")" do
            parser = Map.put(parser, :nesting, old_nesting)
            closing = current_meta(parser)
            ast = {token, newlines ++ [{:closing, closing} | meta], []}

            if peek_token(parser) == :do and parser.nesting == 0 do
              parser = next_token(parser)
              parse_do_block(parser, ast)
            else
              {ast, parser}
            end
          else
            {pairs, parser} = parse_comma_list(parser)

            parser = Map.put(parser, :nesting, old_nesting)

            parser = eat_eol_at(parser, 1)

            case peek_token(parser) do
              :")" ->
                parser = next_token(parser)
                closing = current_meta(parser)

                ast = {token, newlines ++ [{:closing, closing} | meta], List.wrap(pairs)}

                if peek_token(parser) == :do and parser.nesting == 0 do
                  parser = next_token(parser)
                  parse_do_block(parser, ast)
                else
                  {ast, parser}
                end

              _ ->
                parser = put_error(parser, {error_meta, "missing closing parentheses for function invocation"})
                {{token, newlines ++ meta, List.wrap(pairs)}, parser}
            end
          end
      end
    end
  end

  defp parse_identifier(%{current_token: {_identifier, _, token}} = parser)
       when token in [:__MODULE__, :__ENV__, :__DIR__, :__CALLER__] do
    trace "parse_identifier (__MODULE__, etc)", trace_meta(parser) do
      parse_lone_identifier(parser)
    end
  end

  defp parse_identifier(%{current_token: {identifier, _, token}} = parser)
       when identifier in [:identifier, :op_identifier] do
    trace "parse_identifier (#{identifier})", trace_meta(parser) do
      if identifier == :identifier && MapSet.member?(@peeks, peek_token(parser)) do
        parse_lone_identifier(parser)
      else
        meta = current_meta(parser)
        parser = next_token(parser)

        parser = push_nesting(parser)

        # In maps, cap precedence at assoc_op for arguments to prevent => from being consumed
        rest_precedence = if Map.get(parser, :in_map, false), do: {:left, 18}, else: @lowest
        {first_arg, parser} = parse_expression(parser, rest_precedence, false, false, false)

        front = first_arg

        {args, parser} =
          while2 peek_token(parser) == :"," and not stab_state_set?(parser) <- parser do
            parser = parser |> next_token() |> next_token()
            {arg, parser} = parse_expression(parser, rest_precedence, false, false, false)
            {arg, parser}
          end

        args = [front | args]
        parser = pop_nesting(parser)

        {ast, parser} =
          cond do
            parser.nesting == 0 && current_token(parser) == :do ->
              parse_do_block(parser, {token, meta, args})

            parser.nesting == 0 && peek_token(parser) == :do ->
              parse_do_block(next_token(parser), {token, meta, args})

            true ->
              meta =
                if identifier == :op_identifier && length(args) == 1 do
                  [{:ambiguous_op, nil} | meta]
                else
                  meta
                end

              {{token, meta, args}, parser}
          end

        parser = maybe_widen_stab_state(parser, ast)
        {ast, parser}
      end
    end
  end

  defp parse_do_identifier(%{current_token: {:do_identifier, _, token}} = parser) do
    trace "parse_do_identifier - nesting[#{parser.nesting}]", trace_meta(parser) do
      meta = current_meta(parser)
      parser = next_token(parser)

      # if nesting is 0, that means we are not currently an argument for a function call
      # and can assume we are a "lone do_identifier" and parse the block
      # foo do
      #   :ok
      # end

      if parser.nesting == 0 do
        parse_do_block(parser, {token, meta, []})
      else
        {{token, meta, nil}, parser}
      end
    end
  end

  defp parse_call_expression(%{current_token: {:"(", _}} = parser, lhs) do
    trace "parse_call_expression", trace_meta(parser) do
      # this might be wrong, but its how Code.string_to_quoted works
      {_, meta, _} = lhs

      newlines = get_newlines(parser)

      cond do
        peek_token(parser) == :eof ->
          parser = put_error(parser, {meta, "missing closing parentheses for function invocation"})
          closing = current_meta(parser)
          {{lhs, newlines ++ [{:closing, closing} | meta], []}, parser}

        peek_token(parser) == :")" ->
          parser = next_token(parser)
          closing = current_meta(parser)
          {{lhs, newlines ++ [{:closing, closing} | meta], []}, parser}

        true ->
          {pairs, parser} =
            parser
            |> next_token()
            |> eat_eoe()
            |> parse_comma_list()

          parser = eat_eoe_at(parser, 1)

          parser =
            case peek_token(parser) do
              :")" ->
                next_token(parser)

              _ ->
                put_error(parser, {meta, "missing closing parentheses for function invocation"})
            end

          closing = current_meta(parser)

          {{lhs, newlines ++ [{:closing, closing} | meta], List.wrap(pairs)}, parser}
      end
    end
  end

  defp parse_lone_identifier(%{current_token: {_type, token_meta, token}} = parser) do
    trace "parse_lone_identifier", trace_meta(parser) do
      meta =
        parser
        |> current_meta()
        |> push_delimiter(token_meta)

      {{token, meta, nil}, parser}
    end
  end

  defp parse_lone_module_attr(%{current_token: {:at_op, _, token}} = parser) do
    trace "parse_lone_module_attr", trace_meta(parser) do
      meta = current_meta(parser)
      parser = parser |> next_token() |> eat_eoe()

      {rhs, parser} =
        case current_token_type(parser) do
          :"[" ->
            parse_list_literal(parser)

          :int ->
            parse_int(parser)

          :flt ->
            parse_float(parser)

          :char ->
            parse_char(parser)

          :list_string ->
            parse_string(parser)

          :bin_string ->
            parse_string(parser)

          :capture_op ->
            parse_struct_type_prefix(parser)

          :capture_int ->
            parse_capture_int(parser)

          :dual_op ->
            parse_struct_type_prefix(parser)

          :unary_op ->
            parse_struct_type_prefix(parser)

          :atom ->
            parse_atom(parser)

          :alias ->
            parse_alias(parser)

          :at_op ->
            parse_lone_module_attr(parser)

          _ ->
            parse_lone_identifier(parser)
        end

      {{token, meta, [rhs]}, parser}
    end
  end

  defp tokenize(code, opts) do
    opts =
      opts
      |> Keyword.put_new(:cursor_completion, false)
      |> Keyword.put_new(:check_terminators, false)

    tokens =
      case code
           |> String.to_charlist()
           |> :spitfire_tokenizer.tokenize(opts[:line] || 1, opts[:column] || 1, opts) do
        {:ok, _, _, _, tokens, []} ->
          Enum.reverse(tokens)

        {:ok, line, column, _, rev_tokens, rev_terminators} ->
          # vendored from elixir-lang/elixir, license: Apache2
          {rev_tokens, rev_terminators} =
            with [close, open, {_, _, :__cursor__} = cursor | rev_tokens] <- rev_tokens,
                 {_, [_ | after_fn]} <- Enum.split_while(rev_terminators, &(elem(&1, 0) != :fn)),
                 true <- maybe_missing_stab?(rev_tokens, false),
                 [_ | rev_tokens] <- Enum.drop_while(rev_tokens, &(elem(&1, 0) != :fn)) do
              {[close, open, cursor | rev_tokens], after_fn}
            else
              _ -> {rev_tokens, rev_terminators}
            end

          reverse_tokens(line, column, rev_tokens, rev_terminators)

        {:error, _, _, _, tokens} ->
          Enum.reverse(tokens)
      end

    tokens ++ [:eof]
  end

  defp parse_interpolation(parser, tokens) do
    trace "parse_interpolation", trace_meta(parser) do
      args =
        for token <- tokens do
          case token do
            token when is_binary(token) ->
              token

            {{line, col, _}, {cline, ccol, _}, tokens} ->
              meta = [line: line, column: col]

              # construct a new parser
              ast =
                if tokens == [] do
                  {:__block__, [], []}
                else
                  parser =
                    %{
                      tokens: tokens ++ [:eof],
                      current_token: nil,
                      errors: [],
                      peek_token: nil,
                      nesting: 0,
                      fuel: 150,
                      literal_encoder: parser.literal_encoder
                    }
                    |> next_token()
                    |> next_token()

                  initial_meta = current_meta(parser)
                  parser = eat_eoe(parser)

                  if current_token(parser) == :eof do
                    {:__block__, initial_meta, []}
                  else
                    {exprs, _parser} =
                      while2 current_token(parser) != :eof <- parser do
                        {ast, parser} = parse_expression(parser, @lowest, false, false, true)

                        parser =
                          if peek_token(parser) in [:eol, :";", :eof] do
                            next_token(parser)
                          else
                            parser
                          end

                        ast = push_eoe(ast, current_eoe(parser))
                        {ast, eat_eoe(parser)}
                      end

                    build_block_nr(exprs)
                  end
                end

              {:"::", meta,
               [
                 {{:., meta, [Kernel, :to_string]},
                  [from_interpolation: true, closing: [line: cline, column: ccol]] ++ meta, [ast]},
                 {:binary, meta, nil}
               ]}
          end
        end

      {args, parser}
    end
  end

  defp new(code, opts) do
    %{
      tokens: tokenize(code, opts),
      fuel: 150,
      current_token: nil,
      peek_token: nil,
      nesting: 0,
      literal_encoder: Keyword.get(opts, :literal_encoder),
      errors: []
    }
  end

  defp next_token(%{tokens: :eot, current_token: nil, peek_token: nil} = parser) do
    parser
  end

  defp next_token(%{tokens: :eot, current_token: :eof, peek_token: nil} = parser) do
    %{parser | tokens: :eot, current_token: nil, fuel: 150}
  end

  defp next_token(%{tokens: [], current_token: nil, peek_token: nil} = parser) do
    %{parser | tokens: :eot, fuel: 150}
  end

  defp next_token(%{tokens: [], peek_token: nil} = parser) do
    %{parser | tokens: :eot, current_token: nil, fuel: 150}
  end

  defp next_token(%{tokens: []} = parser) do
    %{
      parser
      | current_token: parser.peek_token,
        peek_token: nil,
        tokens: :eot,
        fuel: 150
    }
  end

  defp next_token(%{tokens: [token | tokens]} = parser) do
    %{
      parser
      | tokens: tokens,
        current_token: parser.peek_token,
        peek_token: token,
        fuel: 150
    }
  end

  defp consume_fuel(parser) do
    parser = Map.update!(parser, :fuel, &(&1 - 1))

    if parser.fuel < 1 do
      raise Spitfire.NoFuelRemaining
    end

    parser
  end

  defp eat(edibles, %{tokens: [], current_token: {edible, _}, peek_token: nil} = parser)
       when is_map(edibles) and is_map_key(edibles, edible) do
    %{
      parser
      | tokens: :eot,
        current_token: nil,
        peek_token: nil
    }
  end

  defp eat(edibles, %{tokens: [], current_token: {edible, _}, peek_token: peek} = parser)
       when is_map(edibles) and is_map_key(edibles, edible) do
    %{
      parser
      | tokens: :eot,
        current_token: peek,
        peek_token: nil
    }
  end

  defp eat(edibles, %{tokens: [token | tokens], current_token: {edible, _}} = parser)
       when is_map(edibles) and is_map_key(edibles, edible) do
    %{
      parser
      | tokens: tokens,
        current_token: parser.peek_token,
        peek_token: token
    }
  end

  defp eat(_edibles, parser) do
    parser
  end

  defp eat_eoe(parser) do
    case current_token(parser) do
      type when type in [:eol, :";"] -> eat_eoe(next_token(parser))
      _ -> parser
    end
  end

  defp eat_eol(parser) do
    eat(%{:eol => true}, parser)
  end

  defp eat_eoe_at(%{tokens: [next | rest]} = parser, 1) do
    case peek_token(parser) do
      type when type in [:eol, :";"] ->
        eat_eoe_at(%{parser | peek_token: next, tokens: rest}, 1)

      _ ->
        parser
    end
  end

  defp eat_eoe_at(%{tokens: []} = parser, 1), do: parser
  defp eat_eoe_at(%{tokens: :eot} = parser, 1), do: parser

  defp eat_eol_at(parser, idx) do
    eat_at(parser, [:eol], idx)
  end

  defp eat_at(parser, tokens, idx) when is_list(tokens) do
    eat_at(parser, Map.new(tokens, &{&1, true}), idx)
  end

  defp eat_at(%{tokens: [next | rest]} = parser, tokens, 1) do
    if tokens[peek_token_type(parser)] do
      %{parser | peek_token: next, tokens: rest}
    else
      parser
    end
  end

  defp eat_at(%{tokens: []} = parser, _tokens, 1) do
    parser
  end

  defp eat_at(%{tokens: :eot} = parser, _token, _idx) do
    parser
  end

  defp peek_token(%{peek_token: {:stab_op, _, token}}) do
    token
  end

  defp peek_token(%{peek_token: {type, _, _, _}}) when type in [:list_heredoc, :bin_heredoc] do
    type
  end

  defp peek_token(%{peek_token: {token, _, _}}) do
    token
  end

  defp peek_token(%{peek_token: {token, _}}) do
    token
  end

  defp peek_token(%{peek_token: {token, _, _, _, _, _, _}}) do
    token
  end

  defp peek_token(%{peek_token: :eof}) do
    :eof
  end

  defp peek_token(%{tokens: :eot}) do
    :eof
  end

  defp peek_token_eat_eoe(%{peek_token: {:eol, _token}} = parser) do
    peek_token_eat_eoe(next_token(parser))
  end

  defp peek_token_eat_eoe(%{peek_token: {:";", _token}} = parser) do
    peek_token_eat_eoe(next_token(parser))
  end

  defp peek_token_eat_eoe(%{peek_token: {:stab_op, _, token}}) do
    token
  end

  defp peek_token_eat_eoe(%{peek_token: {type, _, _, _}}) when type in [:list_heredoc, :bin_heredoc] do
    type
  end

  defp peek_token_eat_eoe(%{peek_token: {token, _, _}}) do
    token
  end

  defp peek_token_eat_eoe(%{peek_token: {token, _}}) do
    token
  end

  defp peek_token_eat_eoe(%{peek_token: {token, _, _, _, _, _, _}}) do
    token
  end

  defp peek_token_eat_eoe(%{peek_token: :eof}) do
    :eof
  end

  defp peek_token_eat_eoe(%{tokens: :eot}) do
    :eof
  end

  defp peek_token_skip_eoe(%{peek_token: {:eol, _}} = parser) do
    peek_token_skip_eoe(next_token(parser))
  end

  defp peek_token_skip_eoe(%{peek_token: {:";", _}} = parser) do
    peek_token_skip_eoe(next_token(parser))
  end

  defp peek_token_skip_eoe(parser) do
    peek_token(parser)
  end

  defp peek_token_skip_eol(%{peek_token: {:eol, _}} = parser) do
    peek_token_skip_eol(next_token(parser))
  end

  defp peek_token_skip_eol(parser) do
    peek_token(parser)
  end

  defp current_token_type(%{tokens: :eot}) do
    :eot
  end

  defp current_token_type(%{tokens: :eof}) do
    :eof
  end

  defp current_token_type(%{current_token: {:sigil, _meta, _token, _tokens, _mods, _, _delimiter}}) do
    :sigil
  end

  defp current_token_type(%{current_token: {:bin_heredoc, _meta, _indent, _tokens}}) do
    :bin_heredoc
  end

  defp current_token_type(%{current_token: {:list_heredoc, _meta, _indent, _tokens}}) do
    :list_heredoc
  end

  defp current_token_type(%{current_token: {type, _}}) do
    type
  end

  defp current_token_type(%{current_token: {type, _, _}}) do
    type
  end

  defp peek_token_type(%{peek_token: {type, _}}) do
    type
  end

  defp peek_token_type(%{peek_token: {type, _, _}}) do
    type
  end

  defp peek_token_type(_) do
    :no_peek
  end

  defp current_token(%{current_token: nil}) do
    :eof
  end

  defp current_token(%{current_token: :eof}) do
    :eof
  end

  defp current_token(%{current_token: {:sigil, _meta, token, _tokens, _mods, _indent, _delimiter}}) do
    token
  end

  defp current_token(%{current_token: {:bin_heredoc, _meta, _indent, _tokens}}) do
    :bin_heredoc
  end

  defp current_token(%{current_token: {:list_heredoc, _meta, _indent, _tokens}}) do
    :list_heredoc
  end

  for op <- [
        :arrow_op,
        :pipe_op,
        :when_op,
        :ternary_op,
        :range_op,
        :xor_op,
        :in_match_op,
        :type_op,
        :capture_op,
        :capture_int,
        :block_identifier,
        :in_op,
        :or_op,
        :and_op,
        :comp_op,
        :rel_op,
        :assoc_op,
        :at_op,
        :concat_op,
        :dual_op,
        :mult_op,
        :stab_op,
        :power_op,
        :match_op,
        :unary_op
      ] do
    defp current_token(%{current_token: {unquote(op), _, token}}) do
      token
    end
  end

  defp current_token(%{current_token: {token, _, _}}) do
    token
  end

  defp current_token(%{current_token: {token, _}}) do
    token
  end

  defp current_meta(%{current_token: {:sigil, {line, col, _}, _token, _tokens, _mods, _, _delimiter}}) do
    [line: line, column: col]
  end

  defp current_meta(%{current_token: {:bin_heredoc, {line, col, _}, _indent, _tokens}}) do
    [line: line, column: col]
  end

  defp current_meta(%{current_token: {:list_heredoc, {line, col, _}, _indent, _tokens}}) do
    [line: line, column: col]
  end

  defp current_meta(%{current_token: {token, _}})
       when token in [:fake_closing_brace, :fake_closing_bracket, :fake_closing_brackets] do
    []
  end

  defp current_meta(%{current_token: {_token, {line, col, _}, _}}) do
    [line: line, column: col]
  end

  defp current_meta(%{current_token: {_token, {line, col, _}}}) do
    [line: line, column: col]
  end

  defp current_meta(_) do
    []
  end

  if @trace? do
    defp trace_meta(parser) do
      [{:token, "'#{current_token(parser)}'"}, {:nesting, parser.nesting} | current_meta(parser)]
    end
  end

  defp current_eoe(%{current_token: {token, {line, col, newlines}}})
       when token in [:eol, :";"] and is_integer(newlines) do
    [newlines: newlines, line: line, column: col]
  end

  defp current_eoe(%{current_token: {token, {line, col, _}, _}}) when token in [:eol, :";"] do
    [line: line, column: col]
  end

  defp current_eoe(%{current_token: {token, {line, col, _}}}) when token in [:eol, :";"] do
    [line: line, column: col]
  end

  defp current_eoe(_) do
    nil
  end

  defp peek_eoe(%{peek_token: {token, {line, col, newlines}}}) when token in [:eol, :";"] and is_integer(newlines) do
    [newlines: newlines, line: line, column: col]
  end

  defp peek_eoe(%{peek_token: {token, {line, col, _}, _}}) when token in [:eol, :";"] do
    [line: line, column: col]
  end

  defp peek_eoe(%{peek_token: {token, {line, col, _}}}) when token in [:eol, :";"] do
    [line: line, column: col]
  end

  defp peek_eoe(_) do
    nil
  end

  @newline_carrying_tokens [
    :eol,
    :assoc_op,
    :ternary_op,
    :power_op,
    :range_op,
    :concat_op,
    :arrow_op,
    :comp_op,
    :rel_op,
    :and_op,
    :or_op,
    :xor_op,
    :in_match_op,
    :type_op,
    :stab_op,
    :mult_op,
    :match_op,
    :pipe_op,
    :when_op,
    :in_op
  ]

  defp current_newlines(%{current_token: {token, {_line, _col, newlines}, _}})
       when token in @newline_carrying_tokens and is_integer(newlines) do
    newlines
  end

  defp current_newlines(%{current_token: {token, {_line, _col, newlines}}})
       when token in @newline_carrying_tokens and is_integer(newlines) do
    newlines
  end

  defp current_newlines(_) do
    nil
  end

  defp peek_newlines(%{peek_token: {:eol, {_line, _col, newlines}}}) when is_integer(newlines) do
    newlines
  end

  defp peek_newlines(_) do
    nil
  end

  defp peek_newlines(%{peek_token: {token, {_line, _col, newlines}}}, token) when is_integer(newlines) do
    newlines
  end

  defp peek_newlines(_, _) do
    nil
  end

  defp current_precedence(parser) do
    Map.get(@precedences, current_token_type(parser), @lowest)
  end

  defp peek_precedence(parser) do
    Map.get(@precedences, peek_token_type(parser), @lowest)
  end

  defp pop_nesting(%{nesting: nesting} = parser) do
    %{parser | nesting: nesting - 1}
  end

  defp push_nesting(%{nesting: nesting} = parser) do
    %{parser | nesting: nesting + 1}
  end

  defp encode_literal(%{literal_encoder: encoder} = parser, literal, {line, col, _}) when is_function(encoder) do
    meta = additional_meta(literal, parser) ++ [line: line, column: col]

    case parser.literal_encoder.(literal, meta) do
      {:ok, ast} ->
        ast

      {:error, reason} ->
        Logger.error(reason)
        literal
    end
  end

  defp encode_literal(_parser, literal, _) do
    literal
  end

  defp additional_meta(_literal, %{current_token: {:list_string, _, _}}) do
    [delimiter: "'"]
  end

  defp additional_meta(_literal, %{current_token: {:kw_identifier, _, _}}) do
    [format: :keyword]
  end

  defp additional_meta(_, %{current_token: {type, _, indent, _token}}) when type in [:list_heredoc] do
    [delimiter: ~s"'''", indentation: indent]
  end

  defp additional_meta(literal, parser) when is_list(literal) do
    parser = next_token(parser)
    closing = current_meta(parser)
    [closing: closing]
  end

  defp additional_meta(literal, parser) when is_tuple(literal) do
    closing = current_meta(parser)
    [closing: closing]
  end

  defp additional_meta(_, %{current_token: {type, _, token}}) when type in [:int, :flt] do
    [token: to_string(token)]
  end

  defp additional_meta(_, %{current_token: {type, _, _token}}) when type in [:bin_string, :atom_quoted] do
    [delimiter: ~s'"']
  end

  defp additional_meta(_, %{current_token: {type, _, indent, _token}}) when type in [:bin_heredoc] do
    [delimiter: ~s'"""', indentation: indent]
  end

  defp additional_meta(_literal, %{current_token: {:char, _, token}}) do
    [token: "?" <> List.to_string([token])]
  end

  defp additional_meta(literal, _) when is_atom(literal) do
    []
  end

  defp additional_meta(_, %{current_token: {type, _, _}}) when type in [:do, :atom, :identifier, :block_identifier] do
    []
  end

  defp additional_meta(_, %{current_token: {type, _}}) when type in [:do, nil] do
    []
  end

  defp put_error(parser, error) do
    update_in(parser.errors, &[error | &1])
  end

  @braces MapSet.new([:")", :"]", :"}", :">>"])
  defp validate_peek(parser, current_type) do
    peek = peek_token_type(parser)

    if not valid_peek?(current_type, peek) && peek != :no_peek do
      parser =
        if MapSet.member?(@braces, peek) do
          parser
        else
          next_token(parser)
        end

      {put_error(parser, {current_meta(parser), "syntax error"}), false}
    else
      {parser, true}
    end
  end

  defp valid_peek?(ctype, _ptype) when ctype in [:identifier, :paren_identifier, :"["] do
    true
  end

  defp valid_peek?(_ctype, :"[") do
    true
  end

  defp valid_peek?(:")", :"(") do
    true
  end

  defp valid_peek?(:")", :"{") do
    true
  end

  @valid_peek_after_brace MapSet.put(@peeks, :"[")
  defp valid_peek?(:"}", ptype) do
    MapSet.member?(@valid_peek_after_brace, ptype)
  end

  defp valid_peek?(:alias, ptype) when ptype in [:"{"] do
    true
  end

  defp valid_peek?(:do, _ptype) do
    true
  end

  defp valid_peek?(_ctype, ptype) do
    MapSet.member?(@peeks, ptype)
  end

  # metadata describing how many newlines are present following the start of an expression
  # eg: foo(
  #       arg
  #     )
  # will have 1 newline due to the newline after the opening paren
  defp get_newlines(parser) do
    case peek_newlines(parser) do
      nil -> []
      nl -> [newlines: nl]
    end
  end

  defp push_eoe(ast, eoe) do
    case ast do
      {t, meta, a} when not is_nil(eoe) and t != :-> ->
        {t, [{:end_of_expression, eoe} | meta], a}

      literal ->
        literal
    end
  end

  defp build_block_nr(exprs) do
    case exprs do
      {:->, _, _} ->
        [exprs]

      [{:->, _, _} | _] ->
        exprs

      [{:unquote_splicing, _, [_]}] ->
        {:__block__, [], exprs}

      [expr] ->
        expr

      _ ->
        {:__block__, [], exprs}
    end
  end

  # Code taken from Code.string_to_quoted_with_comments in Elixir core
  # Check it out here: https://github.com/elixir-lang/elixir/blob/12f62e49ca2399a15976d2051a2d7743dae48449/lib/elixir/lib/code.ex#L1327
  # Consult Elixir's license here: https://github.com/elixir-lang/elixir/blob/main/LICENSE
  defp preserve_comments(line, column, tokens, comment, rest) do
    comments = Process.get(:code_formatter_comments)

    comment = %{
      line: line,
      column: column,
      previous_eol_count: previous_eol_count(tokens),
      next_eol_count: next_eol_count(rest, 0),
      text: List.to_string(comment)
    }

    Process.put(:code_formatter_comments, [comment | comments])
  end

  defp next_eol_count([?\s | rest], count), do: next_eol_count(rest, count)
  defp next_eol_count([?\t | rest], count), do: next_eol_count(rest, count)
  defp next_eol_count([?\n | rest], count), do: next_eol_count(rest, count + 1)
  defp next_eol_count([?\r, ?\n | rest], count), do: next_eol_count(rest, count + 1)
  defp next_eol_count(_, count), do: count

  defp previous_eol_count([{token, {_, _, count}} | _]) when token in [:eol, :",", :";"] and count > 0 do
    count
  end

  defp previous_eol_count([]), do: 1
  defp previous_eol_count(_), do: 0

  # vendored from elixir-lang/elixir, license: Apache2
  defp maybe_missing_stab?([{:after, _} | _], _stab_choice?), do: true
  defp maybe_missing_stab?([{:do, _} | _], _stab_choice?), do: true
  defp maybe_missing_stab?([{:fn, _} | _], _stab_choice?), do: true
  defp maybe_missing_stab?([{:else, _} | _], _stab_choice?), do: true
  defp maybe_missing_stab?([{:catch, _} | _], _stab_choice?), do: true
  defp maybe_missing_stab?([{:rescue, _} | _], _stab_choice?), do: true
  defp maybe_missing_stab?([{:stab_op, _, :->} | _], stab_choice?), do: stab_choice?
  defp maybe_missing_stab?([_ | tail], stab_choice?), do: maybe_missing_stab?(tail, stab_choice?)
  defp maybe_missing_stab?([], _stab_choice?), do: false

  # vendored from elixir-lang/elixir, license: Apache2
  defp reverse_tokens(line, column, tokens, terminators) do
    {terminators, _} =
      Enum.map_reduce(terminators, column, fn {start, _, _}, column ->
        atom = :spitfire_tokenizer.terminator(start)

        {{atom, {line, column, nil}}, column + length(Atom.to_charlist(atom))}
      end)

    Enum.reverse(tokens, terminators)
  end

  defp push_delimiter(meta, {_, _, delimiter}) when is_integer(delimiter) do
    [{:delimiter, "#{[delimiter]}"} | meta]
  end

  defp push_delimiter(meta, _token_meta) do
    meta
  end
end
