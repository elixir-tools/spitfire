defmodule Spitfire do
  @moduledoc false
  import Spitfire.While

  require Logger

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
  @range_op {:right, 46}
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

  def parse(code, opts \\ []) do
    parser = code |> new(opts) |> next_token() |> next_token()

    # eat all the beginning eol tokens in case the file starts with a comment
    parser =
      while current_token(parser) == :eol <- parser do
        next_token(parser)
      end

    case parse_program(parser) do
      {ast, %{errors: []}} ->
        {:ok, ast}

      {ast, %{errors: errors}} ->
        {:error, ast, Enum.reverse(errors)}
    end
  end

  def parse!(code, opts \\ []) do
    case parse(code, opts) do
      {:ok, ast} ->
        ast

      {:error, _ast, _errors} ->
        raise "Failed to parse!"
    end
  end

  defp parse_program(parser) do
    exprs = []

    {exprs, parser} =
      while current_token(parser) != :eof <- {exprs, parser} do
        {ast, parser} = parse_expression(parser, top: true)

        parser =
          if peek_token(parser) in [:eol, :eof] and parser.tokens != :eot do
            next_token(parser)
          else
            parser
          end

        ast = push_eoe(ast, current_eoe(parser))

        ast = remove_depth_meta(ast)

        {[ast | exprs], eat_eol(parser)}
      end

    exprs = exprs |> pop_eoe() |> build_block()

    {exprs, parser}
  end

  defp parse_expression(parser, opts \\ []) do
    {associativity, precedence} = Keyword.get(opts, :precedence, @lowest)
    is_list = Keyword.get(opts, :is_list, false)
    is_map = Keyword.get(opts, :is_map, false)
    # NOTE: the root of an expression list is the only place where a comma is treated like an infix operator
    is_top = Keyword.get(opts, :top, false)

    prefix =
      case current_token_type(parser) do
        :identifier -> &parse_identifier/1
        :do_identifier -> &parse_identifier/1
        :paren_identifier -> &parse_identifier/1
        :bracket_identifier -> &parse_identifier/1
        :op_identifier -> &parse_identifier/1
        :alias -> &parse_alias/1
        :"<<" -> &parse_bitstring/1
        :kw_identifier when is_list or is_map -> &parse_kw_identifier/1
        :kw_identifier_unsafe when is_list or is_map -> &parse_kw_identifier/1
        :kw_identifier when not is_list and not is_map -> &parse_bracketless_kw_list/1
        :kw_identifier_unsafe when not is_list and not is_map -> &parse_bracketless_kw_list/1
        :int -> &parse_int/1
        :flt -> &parse_float/1
        :atom -> &parse_atom/1
        :atom_quoted -> &parse_atom/1
        :atom_unsafe -> &parse_atom/1
        true -> &parse_boolean/1
        false -> &parse_boolean/1
        :bin_string -> &parse_string/1
        :bin_heredoc -> &parse_string/1
        :list_string -> &parse_string/1
        :list_heredoc -> &parse_string/1
        :char -> &parse_char/1
        :sigil -> &parse_sigil/1
        :fn -> &parse_anon_function/1
        :at_op -> &parse_prefix_expression/1
        :unary_op -> &parse_prefix_expression/1
        :capture_op -> &parse_prefix_expression/1
        :dual_op -> &parse_prefix_expression/1
        :capture_int -> &parse_capture_int/1
        :stab_op -> &parse_stab_expression/1
        :range_op -> &parse_range_expression/1
        :"[" -> &parse_list_literal/1
        :"(" -> &parse_grouped_expression/1
        :"{" -> &parse_tuple_literal/1
        :%{} -> &parse_map_literal/1
        :% -> &parse_struct_literal/1
        nil -> &parse_nil_literal/1
        _ -> nil
      end

    if prefix == nil do
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

      {{:__error__, meta, ["unknown token: #{ctype}"]}, parser}
    else
      {left, parser} = prefix.(parser)

      calc_prec = fn parser ->
        {_associativity, power} = peek_precedence(parser)

        precedence =
          case associativity do
            :left -> precedence
            :right -> precedence - 1
          end

        precedence < power
      end

      terminals = [:eol, :eof, :"}", :")", :"]", :">>"]

      terminals =
        if is_top do
          terminals
        else
          [:"," | terminals]
        end

      {parser, is_valid} = validate_peek(parser, current_token_type(parser))

      if is_valid do
        while peek_token(parser) not in terminals && calc_prec.(parser) <- {left, parser} do
          infix =
            case peek_token_type(parser) do
              :match_op -> &parse_infix_expression/2
              :when_op -> &parse_infix_expression/2
              :pipe_op when is_map -> &parse_pipe_op/2
              :pipe_op -> &parse_infix_expression/2
              :type_op -> &parse_infix_expression/2
              :dual_op -> &parse_infix_expression/2
              :mult_op -> &parse_infix_expression/2
              :power_op -> &parse_infix_expression/2
              :"[" -> &parse_access_expression/2
              :concat_op -> &parse_infix_expression/2
              :assoc_op -> &parse_assoc_op/2
              :arrow_op -> &parse_infix_expression/2
              :ternary_op -> &parse_infix_expression/2
              :or_op -> &parse_infix_expression/2
              :and_op -> &parse_infix_expression/2
              :comp_op -> &parse_infix_expression/2
              :rel_op -> &parse_infix_expression/2
              :in_op -> &parse_infix_expression/2
              :xor_op -> &parse_infix_expression/2
              :in_match_op -> &parse_infix_expression/2
              :range_op -> &parse_range_expression/2
              :stab_op -> &parse_stab_expression/2
              :do -> &parse_do_block/2
              :dot_call_op -> &parse_dot_call_expression/2
              :"(" -> &parse_call_expression/2
              :. -> &parse_dot_expression/2
              :"," when is_top -> &parse_comma/2
              _ -> nil
            end

          do_block = &parse_do_block/2

          case infix do
            nil ->
              {left, parser}

            ^do_block when parser.nestings != [] ->
              {left, next_token(parser)}

            _ ->
              infix.(next_token(parser), left)
          end
        end
      else
        {left, parser}
      end
    end
  end

  defp parse_grouped_expression(parser) do
    orig_meta = current_meta(parser)

    if peek_token(parser) == :")" do
      parser = parser |> next_token() |> eat_eol()
      {{:__block__, [], []}, parser}
    else
      parser = parser |> next_token() |> eat_eol()
      old_nestings = parser.nestings

      parser =
        parser.nestings
        |> put_in([])
        |> inc_stab_depth()

      {expression, parser} = parse_expression(parser, top: true)

      exprs = [push_eoe(expression, peek_eoe(parser))]

      cond do
        # if the next token is the closing paren or if the next token is a newline and the next next token is the closing paren
        peek_token(parser) == :")" or (peek_token(parser) == :eol and peek_token(next_token(parser)) == :")") ->
          parser =
            parser.nestings
            |> put_in(old_nestings)
            |> next_token()
            |> eat_eol()
            |> dec_stab_depth()

          ast =
            case expression do
              # unquote splicing is special cased, if it has one expression as an arg, its wrapped in a block
              {:unquote_splicing, _, [_]} ->
                {:__block__, [closing: current_meta(parser)] ++ orig_meta, [expression]}

              # not and ! are special cased, if it has one expression as an arg, its wrapped in a block
              {op, _, [_]} when op in [:not, :!] ->
                {:__block__, [], [expression]}

              _ ->
                expression
            end

          {ast, parser}

        # if the next token is a new line, but the next next token is not the closing paren (implied from previous clause)
        peek_token(parser) == :eol ->
          # second conditon checks of the next next token is a closing paren or another expression
          {exprs, parser} =
            while peek_token(parser) == :eol and parser |> next_token() |> peek_token() != :")" <- {exprs, parser} do
              parser = parser |> next_token() |> eat_eol()
              {expression, parser} = parse_expression(parser, top: true)

              {[push_eoe(expression, peek_eoe(parser)) | exprs], parser}
            end

          # handles if the closing paren is on a new line or the same line
          parser =
            if peek_token(parser) == :eol do
              next_token(parser)
            else
              parser
            end

          if peek_token(parser) == :")" do
            parser = parser |> dec_stab_depth() |> put_in([:nestings], old_nestings) |> next_token()

            exprs = exprs |> pop_eoe() |> Enum.reverse()

            {{:__block__, [closing: current_meta(parser)] ++ orig_meta, exprs}, parser}
          else
            meta = current_meta(parser)

            parser =
              parser
              |> put_error({meta, "missing closing parentheses"})
              |> dec_stab_depth()
              |> put_in([:nestings], old_nestings)

            {{:__error__, meta, ["missing closing parentheses"]}, next_token(parser)}
          end

        true ->
          meta = current_meta(parser)

          parser =
            parser
            |> put_error({meta, "missing closing parentheses"})
            |> dec_stab_depth()
            |> put_in([:nestings], old_nestings)

          {{:__error__, meta, ["missing closing parentheses"]}, next_token(parser)}
      end
    end
  end

  defp parse_nil_literal(%{current_token: {nil, meta}} = parser) do
    ast = encode_literal(parser, nil, meta)
    {ast, parser}
  end

  defp parse_kw_identifier(%{current_token: {:kw_identifier, meta, token}} = parser) do
    parser = parser |> next_token() |> eat_eol()

    {expr, parser} = parse_expression(parser, precedence: @kw_identifier)

    token = encode_literal(parser, token, meta)
    {{token, expr}, parser}
  end

  defp parse_kw_identifier(%{current_token: {:kw_identifier_unsafe, meta, tokens}} = parser) do
    {atom, parser} = parse_atom(%{parser | current_token: {:atom_unsafe, meta, tokens}})
    parser = parser |> next_token() |> eat_eol()

    {expr, parser} = parse_expression(parser, precedence: @kw_identifier)

    atom =
      case atom do
        {t, meta, args} ->
          meta = meta |> Keyword.delete(:delimiter) |> Keyword.put(:format, :keyword)
          {t, meta, args}
      end

    {{atom, expr}, parser}
  end

  defp parse_bracketless_kw_list(%{current_token: {:kw_identifier, meta, token}} = parser) do
    parser = parser |> next_token() |> eat_eol()

    {value, parser} = parse_expression(parser, precedence: @kw_identifier)
    token = encode_literal(parser, token, meta)
    kvs = [{token, value}]

    {kvs, parser} =
      while peek_token(parser) == :"," <- {kvs, parser} do
        parser = parser |> next_token() |> next_token()
        {pair, parser} = parse_kw_identifier(parser)

        {[pair | kvs], parser}
      end

    {Enum.reverse(kvs), parser}
  end

  defp parse_bracketless_kw_list(%{current_token: {:kw_identifier_unsafe, meta, tokens}} = parser) do
    {atom, parser} = parse_atom(%{parser | current_token: {:atom_unsafe, meta, tokens}})
    parser = parser |> next_token() |> eat_eol()

    atom =
      case atom do
        {t, meta, args} ->
          meta = meta |> Keyword.delete(:delimiter) |> Keyword.put(:format, :keyword)
          {t, meta, args}
      end

    {value, parser} = parse_expression(parser, precedence: @kw_identifier)
    kvs = [{atom, value}]

    {kvs, parser} =
      while peek_token(parser) == :"," <- {kvs, parser} do
        parser = parser |> next_token() |> next_token()
        {pair, parser} = parse_kw_identifier(parser)

        {[pair | kvs], parser}
      end

    {Enum.reverse(kvs), parser}
  end

  defp parse_assoc_op(%{current_token: {:assoc_op, _, _token}} = parser, key) do
    parser = parser |> next_token() |> eat_eol()
    {value, parser} = parse_expression(parser, precedence: @assoc_op)

    {{key, value}, parser}
  end

  defp parse_comma_list(parser, opts \\ []) do
    opts = Keyword.put_new(opts, :precedence, @list_comma)
    {expr, parser} = parse_expression(parser, opts)
    # we zip together the expression and parser state so that we can potentially 
    # backtrack later
    items = [{expr, parser}]

    {items, parser} =
      while peek_token(parser) == :"," <- {items, parser} do
        parser = next_token(parser)

        case peek_token(parser) do
          delimiter when delimiter in [:"]", :"}"] ->
            {items, parser}

          _ ->
            parser = next_token(parser)
            {item, parser} = parse_expression(parser, opts)

            {[{item, parser} | items], parser}
        end
      end

    {Enum.reverse(items), parser}
  end

  defp parse_prefix_expression(parser) do
    token = current_token(parser)
    meta = current_meta(parser)

    precedence =
      if current_token_type(parser) == :dual_op do
        # dual ops are treated as unary ops when being used as a prefix operator
        @unary_op
      else
        current_precedence(parser)
      end

    parser = next_token(parser)
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    ast = {token, meta, [rhs]}

    {ast, eat_eol(parser)}
  end

  defp parse_prefix_lone_identifer(parser) do
    token = current_token(parser)
    meta = current_meta(parser)

    parser = next_token(parser)
    {rhs, parser} = parse_lone_identifier(parser)

    ast = {token, meta, [rhs]}

    {ast, eat_eol(parser)}
  end

  defp parse_capture_int(parser) do
    token = current_token(parser)
    meta = current_meta(parser)
    parser = next_token(parser)
    {rhs, parser} = parse_int(parser)

    ast = {token, meta, [rhs]}

    {ast, eat_eol(parser)}
  end

  # """
  # A stab expression without a lhs is only possible as the argument to an anonymous function and in the typespect of an anon function

  # ```elixir
  # fn -> :ok end
  # @spec start_link((-> term), GenServer.options()) :: on_start
  # ```
  # """

  defp parse_stab_expression(parser) do
    token = current_token(parser)
    meta = current_meta(parser)
    newlines = get_newlines(parser)

    parser = eat_at(parser, :eol, 1)
    exprs = []

    {exprs, parser} =
      while peek_token(parser) not in [:eof, :end, :")"] <- {exprs, parser} do
        parser = next_token(parser)
        {ast, parser} = parse_expression(parser, top: true)
        eoe = peek_eoe(parser)

        parser = eat_at(parser, :eol, 1)

        ast = push_eoe(ast, eoe)

        {[ast | exprs], eat_eol(parser)}
      end

    rhs = exprs |> pop_eoe() |> build_block()

    ast =
      [{token, newlines ++ meta, [[], rhs]}]

    {ast, eat_eol(parser)}
  end

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
    case current_token(parser) do
      :<- ->
        parse_infix_expression(parser, lhs)

      :-> ->
        token = current_token(parser)
        meta = current_meta(parser)
        newlines = get_newlines(parser)

        current_sd = parser.stab_depth
        parser = eat_at(parser, :eol, 1)
        exprs = []

        old_nestings = parser.nestings
        parser = put_in(parser.nestings, [])

        {exprs, parser} =
          while peek_token(parser) not in [:eof, :end, :")", :block_identifier] <- {exprs, parser} do
            parser = next_token(parser)
            {ast, parser} = parse_expression(parser, top: true)

            eoe = peek_eoe(parser)

            parser = eat_at(parser, :eol, 1)

            ast = push_eoe(ast, eoe)

            {[ast | exprs], eat_eol(parser)}
          end

        rhs = exprs |> pop_eoe() |> build_block()

        {rhs, stabs} =
          Macro.traverse(
            rhs,
            [],
            fn node, acc ->
              case node do
                {:->, meta, _args} ->
                  if meta[:depth] == current_sd do
                    {:__remove_me__, [node | acc]}
                  else
                    {node, acc}
                  end

                _ ->
                  {node, acc}
              end
            end,
            fn
              {node, meta, args}, acc when is_list(args) ->
                args = Enum.reject(args, &(is_list(&1) && Enum.member?(&1, :__remove_me__)))
                {{node, meta, args}, acc}

              node, acc ->
                {node, acc}
            end
          )

        rhs =
          case rhs do
            {:__block__, _, [{:unquote_splicing, _, [_]}]} ->
              rhs

            {:__block__, _, [ast]} ->
              ast

            block ->
              block
          end

        lhs =
          case lhs do
            {:__block__, [], []} -> []
            {:comma, _, lhs} -> lhs
            lhs -> [lhs]
          end

        meta = newlines ++ [depth: parser.stab_depth] ++ meta

        ast =
          [{token, meta, [lhs, rhs]}] ++ Enum.reverse(stabs)

        parser = put_in(parser.nestings, old_nestings)
        {ast, eat_eol(parser)}
    end
  end

  defp parse_comma(parser, lhs) do
    parser = parser |> next_token() |> eat_eol()
    {exprs, parser} = parse_comma_list(parser, precedence: @comma)
    {exprs, _} = Enum.unzip(exprs)

    {{:comma, [], [lhs | exprs]}, eat_eol(parser)}
  end

  defp parse_infix_expression(parser, lhs) do
    token = current_token(parser)
    meta = current_meta(parser)
    precedence = current_precedence(parser)
    # we save this in case the next expression is an error
    pre_parser = parser

    newlines =
      case current_newlines(parser) || peek_newlines(parser, :eol) do
        nil -> []
        nl -> [newlines: nl]
      end

    parser = next_token(parser)

    parser = eat_eol(parser)

    {rhs, parser} = parse_expression(parser, precedence: precedence)

    {rhs, parser} =
      case rhs do
        {:__error__, _, ["unknown token:" <> _]} ->
          parser = put_error(pre_parser, {meta, "malformed right-hand side of #{token} operator"})
          {{:__error__, meta, ["malformed right-hand side of #{token} operator"]}, parser}

        _ ->
          {rhs, parser}
      end

    ast =
      case token do
        :"not in" ->
          {:not, meta, [{:in, meta, [lhs, rhs]}]}

        :when ->
          lhs =
            case lhs do
              {:comma, _, lhs} -> lhs
              lhs -> [lhs]
            end

          {token, newlines ++ meta, lhs ++ [rhs]}

        _ ->
          {token, newlines ++ meta, [lhs, rhs]}
      end

    {ast, eat_eol(parser)}
  end

  defp parse_pipe_op(parser, lhs) do
    token = current_token(parser)
    meta = current_meta(parser)

    newlines =
      case current_newlines(parser) || peek_newlines(parser, :eol) do
        nil -> []
        nl -> [newlines: nl]
      end

    parser = next_token(parser)

    parser = eat_eol(parser)

    {pairs, parser} = parse_comma_list(parser, is_map: true)
    {pairs, _} = Enum.unzip(pairs)

    ast = {token, newlines ++ meta, [lhs, pairs]}

    {ast, eat_eol(parser)}
  end

  defp parse_access_expression(parser, lhs) do
    meta = current_meta(parser)
    parser = parser |> next_token() |> eat_eol()
    {rhs, parser} = parse_expression(parser)

    extra_meta = [from_brackets: true]

    newlines =
      case peek_newlines(parser, :eol) do
        nil -> []
        nl -> [newlines: nl]
      end

    parser = parser |> next_token() |> eat_eol()
    closing = current_meta(parser)
    meta = extra_meta ++ newlines ++ [{:closing, closing} | meta]

    ast = {{:., meta, [Access, :get]}, meta, [lhs, rhs]}

    {ast, parser}
  end

  defp parse_range_expression(parser) do
    token = current_token(parser)
    meta = current_meta(parser)
    {{token, meta, []}, parser}
  end

  defp parse_range_expression(parser, lhs) do
    token = current_token(parser)
    meta = current_meta(parser)
    precedence = current_precedence(parser)
    parser = next_token(parser)
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    if peek_token(parser) == :ternary_op do
      parser = parser |> next_token() |> next_token()
      {rrhs, parser} = parse_expression(parser, precedence: precedence)

      {{:"..//", meta, [lhs, rhs, rrhs]}, eat_eol(parser)}
    else
      {{token, meta, [lhs, rhs]}, eat_eol(parser)}
    end
  end

  defp parse_do_block(%{current_token: {:do, meta}} = parser, lhs) do
    do_meta = current_meta(parser)
    exprs = [{encode_literal(parser, :do, meta), []}]
    parser = parser |> next_token() |> eat_eol()

    parser = inc_stab_depth(parser)

    old_nestings = parser.nestings
    parser = put_in(parser.nestings, [])

    {exprs, parser} =
      while current_token(parser) not in [:end, :eof] <- {exprs, parser} do
        [{type, current_exprs} | rest] = exprs

        {exprs, parser} =
          while current_token(parser) not in [:end, :block_identifier, :eof] <- {current_exprs, parser} do
            {ast, parser} = parse_expression(parser, top: true)

            parser = next_token(parser)
            eoe = current_eoe(parser)

            parser =
              if current_token(parser) in [:end, :block_identifier] do
                parser
              else
                parser |> next_token() |> eat_eol()
              end

            ast = push_eoe(ast, eoe)

            {[ast | current_exprs], parser}
          end

        exprs = pop_eoe(exprs)

        case parser do
          %{current_token: {:block_identifier, meta, token}} ->
            {[{encode_literal(parser, token, meta), []}, {type, exprs} | rest], parser |> next_token() |> eat_eol()}

          _ ->
            {[{type, exprs} | rest], parser}
        end
      end

    {parser, end_meta} =
      case current_token(parser) do
        :end ->
          {parser, current_meta(parser)}

        _ ->
          {put_error(parser, {do_meta, "missing `end` for do block"}), do_meta}
      end

    exprs =
      for {type, expr} <- Enum.reverse(exprs) do
        {type, build_block(expr)}
      end

    ast =
      case lhs do
        {token, meta, nil} ->
          {token, [do: do_meta, end: end_meta] ++ meta, [exprs]}

        {token, meta, args} when is_list(args) ->
          {token, [do: do_meta, end: end_meta] ++ meta, args ++ [exprs]}
      end

    parser = parser |> dec_stab_depth() |> put_in([:nestings], old_nestings)
    {ast, parser}
  end

  defp parse_dot_expression(parser, lhs) do
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

        parser = parser |> next_token() |> eat_eol()
        {items, parser} = parse_comma_list(parser)
        parser = parser |> next_token() |> eat_eol()
        {multis, _} = Enum.unzip(items)

        multis =
          {{:., dot_meta, [lhs, :{}]}, newlines ++ [{:closing, current_meta(parser)} | dot_meta], multis}

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
        ident_meta = current_meta(parser)

        %{current_token: {:bracket_identifier, _, rhs}} = parser

        rhs = {{token, meta, [lhs, rhs]}, [no_parens: true] ++ ident_meta, []}

        parser = next_token(parser)
        {ast, parser} = parse_access_expression(parser, rhs)

        {ast, eat_eol(parser)}

      type when type in [:identifier, :paren_identifier, :do_identifier] ->
        parser = next_token(parser)

        {{rhs, next_meta, args}, parser} = parse_expression(parser, precedence: precedence)

        args =
          if args == nil do
            []
          else
            args
          end

        extra =
          if type in [:identifier, :do_identifier] and args == [] do
            [no_parens: true]
          else
            []
          end

        ast = {{token, meta, [lhs, rhs]}, extra ++ next_meta, args}

        {ast, eat_eol(parser)}

      _ ->
        parser = next_token(parser)
        next_meta = current_meta(parser)
        {rhs, parser} = parse_expression(parser)
        ast = {{token, meta, [lhs, rhs]}, next_meta, []}

        {ast, eat_eol(parser)}
    end
  end

  defp parse_anon_function(%{current_token: {:fn, _}} = parser) do
    meta = current_meta(parser)

    newlines = get_newlines(parser)
    parser = parser |> next_token() |> eat_eol()

    parser = inc_stab_depth(parser)

    {ast, parser} = parse_expression(parser, top: true)

    parser = dec_stab_depth(parser)

    parser =
      case peek_token(parser) do
        :end ->
          parser |> next_token() |> eat_eol()

        _ ->
          put_error(parser, {meta, "missing closing end for anonymous function"})
      end

    meta = [{:closing, current_meta(parser)} | meta]

    {{:fn, newlines ++ meta, ast}, parser}
  end

  defp parse_dot_call_expression(parser, lhs) do
    meta = current_meta(parser)
    parser = next_token(parser)
    newlines = get_newlines(parser)

    parser = eat_eol(parser)

    if peek_token(parser) == :")" do
      parser = next_token(parser)
      closing = [closing: current_meta(parser)]
      ast = {{:., meta, [lhs]}, newlines ++ closing ++ meta, []}
      {ast, parser}
    else
      {pairs, parser} = parse_comma_list(parser |> next_token() |> eat_eol())
      {pairs, _} = Enum.unzip(pairs)
      parser = parser |> next_token() |> eat_eol()
      closing = [closing: current_meta(parser)]
      ast = {{:., meta, [lhs]}, newlines ++ closing ++ meta, pairs}

      {ast, parser}
    end
  end

  defp parse_atom(%{current_token: {:atom, meta, atom}} = parser) do
    atom = encode_literal(parser, atom, meta)
    {atom, parser}
  end

  defp parse_atom(%{current_token: {:atom_quoted, meta, atom}} = parser) do
    atom = encode_literal(parser, atom, meta)
    {atom, parser}
  end

  defp parse_atom(%{current_token: {:atom_unsafe, _, tokens}} = parser) do
    meta = current_meta(parser)
    {args, parser} = parse_interpolation(parser, tokens)
    {{{:., meta, [:erlang, :binary_to_atom]}, [{:delimiter, ~S'"'} | meta], [{:<<>>, meta, args}, :utf8]}, parser}
  end

  defp parse_boolean(%{current_token: {bool, meta}} = parser) do
    bool = encode_literal(parser, bool, meta)

    {bool, parser}
  end

  defp parse_int(%{current_token: {:int, {_, _, int} = meta, _}} = parser) do
    int = encode_literal(parser, int, meta)
    {int, parser}
  end

  defp parse_float(%{current_token: {:flt, {_, _, float} = meta, _}} = parser) do
    float = encode_literal(parser, float, meta)
    {float, parser}
  end

  defp parse_string(%{current_token: {:bin_heredoc, meta, _indent, [string]}} = parser) do
    string = encode_literal(parser, string, meta)
    {string, parser}
  end

  defp parse_string(%{current_token: {:list_heredoc, meta, _indent, [string]}} = parser) do
    string = encode_literal(parser, String.to_charlist(string), meta)
    {string, parser}
  end

  defp parse_string(%{current_token: {:bin_heredoc, _meta, indentation, tokens}} = parser) do
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

  defp parse_string(%{current_token: {:list_heredoc, _meta, indentation, tokens}} = parser) do
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
                    nestings: [],
                    errors: [],
                    literal_encoder: parser.literal_encoder,
                    stab_depth: 0
                  }
                  |> next_token()
                  |> next_token()
                  |> eat_eol()

                {ast, _parser} = parse_expression(parser)
                ast
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

  defp parse_string(%{current_token: {:bin_string, meta, [string]}} = parser) when is_binary(string) do
    string = encode_literal(parser, string, meta)
    {string, parser}
  end

  defp parse_string(%{current_token: {:bin_string, _, tokens}} = parser) do
    meta = current_meta(parser)

    {args, parser} = parse_interpolation(parser, tokens)

    {{:<<>>, [{:delimiter, "\""} | meta], args}, parser}
  end

  defp parse_string(%{current_token: {:list_string, meta, [string]}} = parser) do
    string = encode_literal(parser, String.to_charlist(string), meta)
    {string, parser}
  end

  defp parse_string(%{current_token: {:list_string, _, tokens}} = parser) do
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
                    nestings: [],
                    errors: [],
                    literal_encoder: parser.literal_encoder,
                    stab_depth: 0
                  }
                  |> next_token()
                  |> next_token()
                  |> eat_eol()

                {ast, _parser} = parse_expression(parser)
                ast
              end

            {{:., meta, [Kernel, :to_string]}, [from_interpolation: true, closing: [line: cline, column: ccol]] ++ meta,
             [ast]}
        end
      end

    {{{:., meta, [List, :to_charlist]}, [{:delimiter, "'"} | meta], [args]}, parser}
  end

  defp parse_char(%{current_token: {:char, {_, _, _token} = meta, num}} = parser) do
    char = encode_literal(parser, num, meta)
    {char, parser}
  end

  defp parse_sigil(%{current_token: {:sigil, _meta, token, tokens, mods, indentation, delimiter}} = parser) do
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

  defp parse_alias(%{current_token: {:alias, _, alias}} = parser) do
    meta = current_meta(parser)
    aliases = [{alias, meta}]

    {[{_alias, last} | _rest] = aliases, parser} =
      while peek_token(parser) == :. and peek_token(next_token(parser)) == :alias <- {aliases, parser} do
        parser = next_token(parser)

        case parser.peek_token do
          {:alias, _, alias} ->
            parser = next_token(parser)
            {[{alias, current_meta(parser)} | aliases], parser}
        end
      end

    aliases = aliases |> Enum.reverse() |> Enum.unzip() |> elem(0)

    {{:__aliases__, [{:last, last} | meta], aliases}, parser}
  end

  defp parse_bitstring(%{current_token: {:"<<", _}} = parser) do
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
        {pairs, parser} = parse_comma_list(parser, is_list: true)

        parser = eat_at(parser, :eol, 1)

        case peek_token(parser) do
          :">>" ->
            pairs = pairs |> Enum.unzip() |> elem(0)
            parser = next_token(parser)
            {{:<<>>, newlines ++ [{:closing, current_meta(parser)} | meta], pairs}, eat_eol(parser)}

          _ ->
            [{potential_error, parser}, {item, parser_for_errors} | rest] = all_pairs = Enum.reverse(pairs)

            # if the last item is an unknown token error, that means that it parsed past the
            # recovery point and we need to insert a fake closing bracket, and backtrack
            # the errors from the previous item.
            {pairs, parser} =
              case potential_error do
                {:__error__, _, ["unknown token: " <> _]} ->
                  {[{item, parser} | rest],
                   parser
                   |> put_in([:current_token], {:fake_closing_brackets, nil})
                   |> put_in([:peek_token], parser.current_token)
                   |> put_in([:errors], parser_for_errors.errors)
                   |> update_in([:tokens], &[parser.peek_token | &1])}

                _ ->
                  {all_pairs, parser}
              end

            parser = put_error(parser, {meta, "missing closing brackets for bitstring"})

            {pairs, _} = pairs |> Enum.reverse() |> Enum.unzip()

            {{:<<>>, newlines ++ [{:closing, current_meta(parser)} | meta], List.wrap(pairs)}, parser}
        end
    end
  end

  defp parse_map_literal(%{current_token: {:%{}, _}} = parser) do
    meta = current_meta(parser)
    parser = next_token(parser)
    # we use a then to create lexical scoping to 
    # hide manipulating incrementing the parser
    newlines = peek_newlines(parser)

    parser = parser |> next_token() |> eat_eol()
    old_nestings = parser.nestings
    parser = put_in(parser.nestings, [])

    if current_token(parser) == :"}" do
      closing = current_meta(parser)
      parser = put_in(parser.nestings, old_nestings)

      extra =
        if newlines do
          [{:newlines, newlines}, {:closing, closing}]
        else
          [{:closing, closing}]
        end

      {{:%{}, extra ++ meta, []}, parser}
    else
      {pairs, parser} = parse_comma_list(parser, is_map: true)
      {pairs, _} = Enum.unzip(pairs)

      parser = eat_at(parser, :eol, 1)

      parser =
        case peek_token(parser) do
          :"}" ->
            next_token(parser)

          _ ->
            put_error(parser, {current_meta(parser), "missing closing brace for map"})
        end

      closing = current_meta(parser)
      parser = put_in(parser.nestings, old_nestings)

      extra =
        if newlines do
          [{:newlines, newlines}, {:closing, closing}]
        else
          [{:closing, closing}]
        end

      {{:%{}, extra ++ meta, pairs}, parser}
    end
  end

  defp parse_struct_type(parser) do
    # structs can only have certain expressions to denote the type,
    # so we special case them here rather than parse an arbitrary expression

    {associativity, precedence} = @lowest

    prefix =
      case current_token_type(parser) do
        :identifier -> &parse_lone_identifier/1
        :paren_identifier -> &parse_identifier/1
        :alias -> &parse_alias/1
        :at_op -> &parse_lone_module_attr/1
        :unary_op -> &parse_prefix_lone_identifer/1
        _ -> nil
      end

    if prefix == nil do
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

      {{:__error__, meta, ["unknown token: #{ctype}"]}, parser}
    else
      {left, parser} = prefix.(parser)

      calc_prec = fn parser ->
        {_associativity, power} = peek_precedence(parser)

        precedence =
          case associativity do
            :left -> precedence
            :right -> precedence - 1
          end

        precedence < power
      end

      terminals = [:eol, :eof, :"}", :")", :"]", :">>"]

      {parser, is_valid} = validate_peek(parser, current_token_type(parser))

      if is_valid do
        while peek_token(parser) not in terminals && calc_prec.(parser) <- {left, parser} do
          infix =
            case peek_token_type(parser) do
              :. -> &parse_dot_expression/2
              _ -> nil
            end

          case infix do
            nil ->
              {left, parser}

            _ ->
              infix.(next_token(parser), left)
          end
        end
      else
        {left, parser}
      end
    end
  end

  defp parse_struct_literal(%{current_token: {:%, _}} = parser) do
    meta = current_meta(parser)
    parser = next_token(parser)
    {type, parser} = parse_struct_type(parser)

    parser = next_token(parser)

    brace_meta = current_meta(parser)
    parser = next_token(parser)

    newlines =
      case current_newlines(parser) do
        nil -> []
        nl -> [newlines: nl]
      end

    parser = eat_eol(parser)

    old_nestings = parser.nestings
    parser = put_in(parser.nestings, [])

    if current_token(parser) == :"}" do
      closing = current_meta(parser)
      ast = {:%, meta, [type, {:%{}, newlines ++ [{:closing, closing} | brace_meta], []}]}
      parser = put_in(parser.nestings, old_nestings)
      {ast, parser}
    else
      {pairs, parser} = parse_comma_list(parser, is_map: true)
      {pairs, _} = Enum.unzip(pairs)

      parser = eat_at(parser, :eol, 1)

      parser =
        case peek_token(parser) do
          :"}" ->
            next_token(parser)

          _ ->
            put_error(parser, {current_meta(parser), "missing closing brace for struct"})
        end

      closing = current_meta(parser)
      ast = {:%, meta, [type, {:%{}, newlines ++ [{:closing, closing} | brace_meta], pairs}]}
      parser = put_in(parser.nestings, old_nestings)
      {ast, parser}
    end
  end

  defp parse_tuple_literal(%{current_token: {:"{", orig_meta}} = parser) do
    meta = current_meta(parser)
    orig_parser = parser
    newlines = peek_newlines(parser)

    parser = parser |> next_token() |> eat_eol()
    old_nestings = parser.nestings
    parser = put_in(parser.nestings, [])

    cond do
      current_token(parser) == :"}" ->
        closing = current_meta(parser)
        parser = put_in(parser.nestings, old_nestings)

        extra =
          if newlines do
            [{:newlines, newlines}, {:closing, closing}]
          else
            [{:closing, closing}]
          end

        {{:{}, extra ++ meta, []}, parser}

      current_token(parser) in [:end, :"]", :")", :">>"] ->
        # if the current token is the wrong kind of ending delimiter, we revert to the previous parser
        # state, put an error, and inject a closing brace to simulate a completed tuple
        parser = put_error(orig_parser, {meta, "missing closing brace for tuple"})

        parser = next_token(parser)

        parser =
          parser
          |> put_in([:current_token], {:fake_closing_brace, nil})
          |> put_in([:peek_token], parser.current_token)
          |> update_in([:tokens], &[parser.peek_token | &1])

        parser = put_in(parser.nestings, old_nestings)
        {{:{}, meta, []}, parser}

      true ->
        {pairs, parser} = parse_comma_list(parser)

        parser = eat_at(parser, :eol, 1)

        {pairs, parser} =
          case peek_token(parser) do
            :"}" ->
              pairs = pairs |> Enum.unzip() |> elem(0)
              {pairs, parser |> next_token() |> eat_eol()}

            _ ->
              [{potential_error, parser}, {item, parser_for_errors} | rest] = all_pairs = Enum.reverse(pairs)

              # if the last item is an unknown token error, that means that it parsed past the
              # recovery point and we need to insert a fake closing brace, and backtrack
              # the errors from the previous item.
              {pairs, parser} =
                case potential_error do
                  {:__error__, _, ["unknown token: " <> _]} ->
                    {[{item, parser} | rest],
                     parser
                     |> put_in([:current_token], {:fake_closing_brace, nil})
                     |> put_in([:peek_token], parser.current_token)
                     |> put_in([:errors], parser_for_errors.errors)
                     |> update_in([:tokens], &[parser.peek_token | &1])}

                  _ ->
                    {all_pairs, parser}
                end

              parser = put_error(parser, {meta, "missing closing brace for tuple"})

              {pairs, _} = pairs |> Enum.reverse() |> Enum.unzip()

              {pairs, parser}
          end

        if length(pairs) == 2 do
          tuple = encode_literal(parser, pairs |> List.wrap() |> List.to_tuple(), orig_meta)
          parser = put_in(parser.nestings, old_nestings)
          {tuple, parser}
        else
          closing = current_meta(parser)
          parser = put_in(parser.nestings, old_nestings)

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

  defp parse_list_literal(%{current_token: {:"[", orig_meta}} = parser) do
    meta = current_meta(parser)
    orig_parser = parser
    parser = parser |> next_token() |> eat_eol()
    old_nestings = parser.nestings
    parser = put_in(parser.nestings, [])

    cond do
      current_token(parser) == :"]" ->
        parser = put_in(parser.nestings, old_nestings)
        {encode_literal(parser, [], orig_meta), parser}

      current_token(parser) in [:end, :"}", :")", :">>"] ->
        # if the current token is the wrong kind of ending delimiter, we revert to the previous parser
        # state, put an error, and inject a closing bracket to simulate a completed list
        parser = put_error(orig_parser, {meta, "missing closing bracket for list"})

        parser = next_token(parser)

        parser =
          parser
          |> put_in([:current_token], {:fake_closing_bracket, nil})
          |> put_in([:peek_token], parser.current_token)
          |> update_in([:tokens], &[parser.peek_token | &1])

        parser = put_in(parser.nestings, old_nestings)
        {encode_literal(parser, [], orig_meta), parser}

      true ->
        {pairs, parser} = parse_comma_list(parser, is_list: true)

        parser = eat_at(parser, :eol, 1)

        case peek_token(parser) do
          :"]" ->
            pairs = pairs |> Enum.unzip() |> elem(0)
            parser = put_in(parser.nestings, old_nestings)
            {encode_literal(parser, pairs, orig_meta), parser |> next_token() |> eat_eol()}

          _ ->
            [{potential_error, parser}, {item, parser_for_errors} | rest] = all_pairs = Enum.reverse(pairs)

            # if the last item is an unknown token error, that means that it parsed past the
            # recovery point and we need to insert a fake closing bracket, and backtrack
            # the errors from the previous item.
            {pairs, parser} =
              case potential_error do
                {:__error__, _, ["unknown token: " <> _]} ->
                  {[{item, parser} | rest],
                   parser
                   |> put_in([:current_token], {:fake_closing_bracket, nil})
                   |> put_in([:peek_token], parser.current_token)
                   |> put_in([:errors], parser_for_errors.errors)
                   |> update_in([:tokens], &[parser.peek_token | &1])}

                _ ->
                  {all_pairs, parser}
              end

            parser = put_error(parser, {meta, "missing closing bracket for list"})

            {pairs, _} = pairs |> Enum.reverse() |> Enum.unzip()

            parser = put_in(parser.nestings, old_nestings)
            {encode_literal(parser, List.wrap(pairs), orig_meta), parser}
        end
    end
  end

  defp parse_identifier(%{current_token: {:paren_identifier, _, token}} = parser) do
    meta = current_meta(parser)
    parser = next_token(parser)
    newlines = get_newlines(parser)
    error_meta = current_meta(parser)

    if peek_token(parser) == :")" do
      parser = next_token(parser)
      closing = current_meta(parser)
      {{token, newlines ++ [{:closing, closing} | meta], []}, parser}
    else
      old_nestings = parser.nestings
      parser = put_in(parser.nestings, [])

      {pairs, parser} =
        parser
        |> next_token()
        |> eat_eol()
        |> parse_comma_list(pre_parse: fn parser -> inc_nesting(parser) end)

      parser = put_in(parser, [:nestings], old_nestings)
      {pairs, _} = Enum.unzip(pairs)

      parser = eat_at(parser, :eol, 1)

      case peek_token(parser) do
        :")" ->
          parser = next_token(parser)
          closing = current_meta(parser)

          {{token, newlines ++ [{:closing, closing} | meta], List.wrap(pairs)}, parser}

        _ ->
          parser = put_error(parser, {error_meta, "missing closing parentheses for function invocation"})
          {{token, newlines ++ meta, List.wrap(pairs)}, parser}
      end
    end
  end

  defp parse_identifier(%{current_token: {:bracket_identifier, _, _token}} = parser) do
    parse_lone_identifier(parser)
  end

  @operators [
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
    :**,
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

  defp parse_identifier(%{current_token: {identifier, _, token}} = parser)
       when identifier in [:identifier, :op_identifier] do
    meta = current_meta(parser)

    if token in [:__MODULE__, :__ENV__, :__DIR__, :__CALLER__] or
         (identifier == :identifier and
            peek_token(parser) in ([:";", :eol, :eof, :end, :",", :")", :do, :., :"}", :"]", :">>"] ++ @operators)) do
      parse_lone_identifier(parser)
    else
      parser = next_token(parser)

      parser = push_nesting(parser, 1)
      {first_arg, parser} = parse_expression(parser)

      args = [first_arg]

      {args, parser} =
        while peek_token(parser) == :"," <- {args, parser} do
          parser = parser |> next_token() |> next_token()
          parser = inc_nesting(parser)
          {arg, parser} = parse_expression(parser)

          {[arg | args], parser}
        end

      parser = pop_nesting(parser)

      if parser.nestings == [] && current_token(parser) == :do do
        parse_do_block(parser, {token, meta, Enum.reverse(args)})
      else
        meta =
          if identifier == :op_identifier and length(args) == 1 do
            [{:ambiguous_op, nil} | meta]
          else
            meta
          end

        {{token, meta, Enum.reverse(args)}, parser}
      end
    end
  end

  defp parse_identifier(%{current_token: {:do_identifier, _, token}} = parser) do
    meta = current_meta(parser)

    parser = next_token(parser)

    # if nesting is empty, that means we are not currently an argument for a function call
    # and can assume we are a "lone do_identifier" and parse the block
    # foo do
    #   :ok
    # end
    if parser.nestings == [] do
      parse_do_block(parser, {token, meta, []})
    else
      {{token, meta, nil}, parser}
    end
  end

  defp parse_call_expression(%{current_token: {:"(", _}} = parser, lhs) do
    # this might be wrong, but its how Code.string_to_quoted works
    {_, meta, _} = lhs

    newlines = get_newlines(parser)

    if peek_token(parser) == :")" do
      parser = next_token(parser)
      closing = current_meta(parser)
      {{lhs, newlines ++ [{:closing, closing} | meta], []}, parser}
    else
      {pairs, parser} =
        parser
        |> next_token()
        |> eat_eol()
        |> parse_comma_list()

      {pairs, _} = Enum.unzip(pairs)

      parser = eat_at(parser, :eol, 1)

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

  defp parse_lone_identifier(%{current_token: {_type, _, token}} = parser) do
    meta = current_meta(parser)
    {{token, meta, nil}, parser}
  end

  defp parse_lone_module_attr(%{current_token: {:at_op, _, token}} = parser) do
    meta = current_meta(parser)
    parser = next_token(parser)
    {ident, parser} = parse_lone_identifier(parser)
    {{token, meta, [ident]}, parser}
  end

  def tokenize(code, opts) do
    tokens =
      case code
           |> String.to_charlist()
           |> :elixir_tokenizer.tokenize(
             opts[:line] || 1,
             opts[:column] || 1,
             opts |> Keyword.put(:check_terminators, false) |> Keyword.put(:cursor_completion, false)
           ) do
        {:ok, _, _, _, tokens} ->
          tokens

        {:error, _, _, [], tokens} ->
          Enum.reverse(tokens)
      end

    tokens ++ [:eof]
  end

  def parse_interpolation(parser, tokens) do
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
                parser = %{
                  tokens: tokens ++ [:eof],
                  current_token: nil,
                  peek_token: nil,
                  nestings: [],
                  errors: [],
                  literal_encoder: parser.literal_encoder,
                  stab_depth: 0
                }

                {ast, _parser} = parse_expression(parser |> next_token() |> next_token() |> eat_eol())
                ast
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

  def new(code, opts) do
    %{
      tokens: tokenize(code, opts),
      current_token: nil,
      peek_token: nil,
      nestings: [],
      stab_depth: 0,
      literal_encoder: Keyword.get(opts, :literal_encoder, fn literal, _meta -> {:ok, literal} end),
      errors: []
    }
  end

  def next_token(%{tokens: :eot, current_token: nil, peek_token: nil} = parser) do
    parser
  end

  def next_token(%{tokens: :eot, current_token: :eof, peek_token: nil} = parser) do
    %{parser | tokens: :eot, current_token: nil}
  end

  def next_token(%{tokens: [], current_token: nil, peek_token: nil} = parser) do
    %{parser | tokens: :eot}
  end

  def next_token(%{tokens: [], peek_token: nil} = parser) do
    %{parser | tokens: :eot, current_token: nil}
  end

  def next_token(%{tokens: []} = parser) do
    %{
      parser
      | current_token: parser.peek_token,
        peek_token: nil,
        tokens: :eot
    }
  end

  def next_token(%{tokens: [token | tokens]} = parser) do
    %{
      parser
      | tokens: tokens,
        current_token: parser.peek_token,
        peek_token: token
    }
  end

  def eat(edible, %{tokens: [], current_token: {edible, _}, peek_token: nil} = parser) do
    %{
      parser
      | tokens: :eot,
        current_token: nil,
        peek_token: nil
    }
  end

  def eat(edible, %{tokens: [], current_token: {edible, _}, peek_token: peek} = parser) do
    %{
      parser
      | tokens: :eot,
        current_token: peek,
        peek_token: nil
    }
  end

  def eat(edible, %{tokens: [token | tokens], current_token: {edible, _}} = parser) do
    %{
      parser
      | tokens: tokens,
        current_token: parser.peek_token,
        peek_token: token
    }
  end

  def eat(_edible, parser) do
    parser
  end

  def eat_eol(parser) do
    eat(:eol, parser)
  end

  def eat_at(parser, token, 0) do
    eat(token, parser)
  end

  def eat_at(%{peek_token: peek_token, tokens: [next | rest]} = parser, token, 1) do
    case peek_token do
      {^token, _} ->
        %{parser | peek_token: next, tokens: rest}

      {^token, _, _} ->
        %{parser | peek_token: next, tokens: rest}

      _ ->
        parser
    end
  end

  def eat_at(%{tokens: tokens} = parser, token, idx) when is_list(tokens) do
    tokens =
      case Enum.at(tokens, idx) do
        {^token, _, _} ->
          List.delete_at(tokens, idx)

        {^token, _} ->
          List.delete_at(tokens, idx)

        _ ->
          tokens
      end

    %{parser | tokens: tokens}
  end

  def eat_at(%{tokens: :eot} = parser, _token, _idx) do
    parser
  end

  def peek_token(%{peek_token: {:stab_op, _, token}}) do
    token
  end

  def peek_token(%{peek_token: {type, _, _, _}}) when type in [:list_heredoc, :bin_heredoc] do
    type
  end

  def peek_token(%{peek_token: {token, _, _}}) do
    token
  end

  def peek_token(%{peek_token: {token, _}}) do
    token
  end

  def peek_token(%{peek_token: {token, _, _, _, _, _, _}}) do
    token
  end

  def peek_token(%{peek_token: :eof}) do
    :eof
  end

  def peek_token(%{tokens: :eot}) do
    :eof
  end

  def current_token_type(%{tokens: :eot}) do
    :eot
  end

  def current_token_type(%{tokens: :eof}) do
    :eof
  end

  def current_token_type(%{current_token: {:sigil, _meta, _token, _tokens, _mods, _, _delimiter}}) do
    :sigil
  end

  def current_token_type(%{current_token: {:bin_heredoc, _meta, _indent, _tokens}}) do
    :bin_heredoc
  end

  def current_token_type(%{current_token: {:list_heredoc, _meta, _indent, _tokens}}) do
    :list_heredoc
  end

  def current_token_type(%{current_token: {type, _}}) do
    type
  end

  def current_token_type(%{current_token: {type, _, _}}) do
    type
  end

  def peek_token_type(%{peek_token: {type, _}}) do
    type
  end

  def peek_token_type(%{peek_token: {type, _, _}}) do
    type
  end

  def peek_token_type(_) do
    :no_peek
  end

  def current_token(%{current_token: nil}) do
    :eof
  end

  def current_token(%{current_token: :eof}) do
    :eof
  end

  def current_token(%{current_token: {:sigil, _meta, token, _tokens, _mods, _indent, _delimiter}}) do
    token
  end

  def current_token(%{current_token: {:bin_heredoc, _meta, _indent, _tokens}}) do
    :bin_heredoc
  end

  def current_token(%{current_token: {:list_heredoc, _meta, _indent, _tokens}}) do
    :list_heredoc
  end

  def current_token(%{current_token: {op, _, token}})
      when op in [
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
    token
  end

  def current_token(%{current_token: {token, _, _}}) do
    token
  end

  def current_token(%{current_token: {token, _}}) do
    token
  end

  def current_meta(%{current_token: {:sigil, {line, col, _}, _token, _tokens, _mods, _, _delimiter}}) do
    [line: line, column: col]
  end

  def current_meta(%{current_token: {:bin_heredoc, {line, col, _}, _indent, _tokens}}) do
    [line: line, column: col]
  end

  def current_meta(%{current_token: {:list_heredoc, {line, col, _}, _indent, _tokens}}) do
    [line: line, column: col]
  end

  def current_meta(%{current_token: {token, _}})
      when token in [:fake_closing_brace, :fake_closing_bracket, :fake_closing_brackets] do
    []
  end

  def current_meta(%{current_token: {_token, {line, col, _}, _}}) do
    [line: line, column: col]
  end

  def current_meta(%{current_token: {_token, {line, col, _}}}) do
    [line: line, column: col]
  end

  def current_eoe(%{current_token: {:eol, {line, col, newlines}}}) do
    [newlines: newlines, line: line, column: col]
  end

  def current_eoe(%{current_token: {_, {line, col, _}, _}}) do
    [line: line, column: col]
  end

  def current_eoe(%{current_token: {_, {line, col, _}}}) do
    [line: line, column: col]
  end

  def current_eoe(_) do
    nil
  end

  def peek_eoe(%{peek_token: {:eol, {line, col, newlines}}}) do
    [newlines: newlines, line: line, column: col]
  end

  def peek_eoe(%{peek_token: {_, {line, col, _}, _}}) do
    [line: line, column: col]
  end

  def peek_eoe(%{peek_token: {_, {line, col, _}}}) do
    [line: line, column: col]
  end

  def peek_eoe(_) do
    nil
  end

  def current_newlines(%{current_token: {_token, {_line, _col, newlines}, _}}) when is_integer(newlines) do
    newlines
  end

  def current_newlines(%{current_token: {_token, {_line, _col, newlines}}}) when is_integer(newlines) do
    newlines
  end

  def current_newlines(_) do
    nil
  end

  def peek_newlines(%{peek_token: {:eol, {_line, _col, newlines}}}) when is_integer(newlines) do
    newlines
  end

  def peek_newlines(_) do
    nil
  end

  def peek_newlines(%{peek_token: {token, {_line, _col, newlines}}}, token) when is_integer(newlines) do
    newlines
  end

  def peek_newlines(_, _) do
    nil
  end

  def token_loc({_, {row, col, _}, _}) do
    {row, col}
  end

  def token_loc({_, {row, col, _}}) do
    {row, col}
  end

  defp current_precedence(parser) do
    Map.get(@precedences, current_token_type(parser), @lowest)
  end

  defp peek_precedence(parser) do
    Map.get(@precedences, peek_token_type(parser), @lowest)
  end

  defp inc_nesting(%{nestings: [top | rest]} = parser) do
    %{parser | nestings: [top + 1 | rest]}
  end

  defp pop_nesting(%{nestings: [_top | rest]} = parser) do
    %{parser | nestings: rest}
  end

  defp push_nesting(%{nestings: nestings} = parser, nesting) do
    %{parser | nestings: [nesting | nestings]}
  end

  defp inc_stab_depth(%{stab_depth: depth} = parser) do
    %{parser | stab_depth: depth + 1}
  end

  defp dec_stab_depth(%{stab_depth: depth} = parser) do
    %{parser | stab_depth: depth - 1}
  end

  defp encode_literal(parser, literal, {line, col, _}) do
    meta = additional_meta(literal, parser) ++ [line: line, column: col]

    case parser.literal_encoder.(literal, meta) do
      {:ok, ast} ->
        ast

      {:error, reason} ->
        Logger.error(reason)
        literal
    end
  end

  defp additional_meta(_literal, %{current_token: {:list_string, _, _}}) do
    [delimiter: "'"]
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
    [token: "?#{List.to_string([token])}"]
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

  defp validate_peek(parser, current_type) do
    peek = peek_token_type(parser)

    if peek != :no_peek and not valid_peek?(current_type, peek) do
      parser =
        if peek in [:")", :"]", :"}", :">>"] do
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

  defp valid_peek?(:"}", ptype) do
    ptype in (@operators ++ [:"[", :";", :eol, :eof, :",", :")", :do, :., :"}", :"]", :">>", :end])
  end

  defp valid_peek?(:alias, ptype) when ptype in [:"{"] do
    true
  end

  defp valid_peek?(_ctype, ptype) do
    ptype in (@operators ++ [:";", :eol, :eof, :",", :")", :do, :., :"}", :"]", :">>", :end])
  end

  defp remove_depth_meta(ast) do
    Macro.prewalk(ast, fn
      {:->, meta, args} -> {:->, Keyword.delete(meta, :depth), args}
      node -> node
    end)
  end

  defp delete_meta({t, m, a}, key) do
    {t, Keyword.delete(m, key), a}
  end

  defp delete_meta(ast, _meta) do
    ast
  end

  # metadata describing how mnay newlines are present following the start of an expression
  # eg: foo(
  #       arg
  #     )
  # will have 1 newling due to the newline after the opening paren
  defp get_newlines(parser, kind \\ :peek)

  defp get_newlines(parser, :peek) do
    case peek_newlines(parser) do
      nil -> []
      nl -> [newlines: nl]
    end
  end

  defp get_newlines(parser, :current) do
    case current_newlines(parser) do
      nil -> []
      nl -> [newlines: nl]
    end
  end

  # delete the eoe metadata from the first expression in the list
  defp pop_eoe([ast | rest]) do
    [delete_meta(ast, :end_of_expression) | rest]
  end

  defp pop_eoe(exprs) do
    exprs
  end

  defp push_eoe(ast, eoe) do
    case ast do
      {t, meta, a} ->
        {t, [{:end_of_expression, eoe} | meta], a}

      literal ->
        literal
    end
  end

  defp build_block(exprs) do
    case exprs do
      [{:unquote_splicing, _, [_]}] ->
        {:__block__, [], exprs}

      [_] ->
        List.first(exprs)

      _ ->
        {:__block__, [], Enum.reverse(exprs)}
    end
  end
end
