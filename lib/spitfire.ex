defmodule Spitfire do
  @moduledoc false
  import Spitfire.While

  require Logger

  @lowest {:left, 2}
  @doo {:left, 4}
  @stab_op {:right, 6}
  @comma {:left, 8}
  @whenn {:right, 8}
  @kw_identifier {:left, 10}
  @in_match_op {:left, 12}
  @type_op {:right, 16}
  @pipe_op {:right, 18}
  @assoc_op {:right, 20}
  @capture_op {:left, 22}
  @match_op {:right, 24}
  @or_op {:left, 26}
  @and_op {:left, 28}
  @comp_op {:left, 30}
  @rel_op {:left, 32}
  @arrow_op {:left, 34}
  @in_op {:left, 36}
  @xor_op {:left, 38}
  @ternary_op {:right, 40}
  @concat_op {:right, 42}
  @range_op {:right, 44}
  @dual_op {:left, 46}
  @mult_op {:left, 48}
  @power_op {:left, 50}
  @left_paren {:left, 52}
  @left_bracket {:left, 54}
  @unary_op {:left, 56}
  @dot_call_op {:left, 58}
  @dot_op {:left, 60}
  @at_op {:left, 62}

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

    case parse_program(parser) do
      {ast, %{errors: []}} ->
        {:ok, ast}

      {ast, %{errors: errors}} ->
        {:error, ast, errors}
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

        {ast, exprs} =
          case ast do
            {:",", _meta, [arg]} ->
              [last | rest] = exprs
              {[last, arg], rest}

            _ ->
              {ast, exprs}
          end

        {[ast | exprs], eat_eol(parser)}
      end

    {
      case exprs do
        [ast] -> ast
        exprs -> {:__block__, [], Enum.reverse(exprs)}
      end,
      parser
    }
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
        :alias -> &parse_alias/1
        :kw_identifier when is_list or is_map -> &parse_kw_identifier/1
        :kw_identifier when not is_list and not is_map -> &parse_bracketless_kw_list/1
        :int -> &parse_int/1
        :atom -> &parse_atom/1
        :atom_quoted -> &parse_atom/1
        :atom_unsafe -> &parse_atom/1
        true -> &parse_boolean/1
        false -> &parse_boolean/1
        :bin_string -> &parse_string/1
        :fn -> &parse_anon_function/1
        :at_op -> &parse_prefix_expression/1
        :unary_op -> &parse_prefix_expression/1
        :capture_op -> &parse_prefix_expression/1
        :capture_int -> &parse_prefix_expression/1
        :stab_op -> &parse_stab_expression/1
        :"[" -> &parse_list_literal/1
        :"(" -> &parse_grouped_expression/1
        :"{" -> &parse_tuple_literal/1
        :%{} -> &parse_map_literal/1
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

      terminals = [:eol, :eof, :"}", :")", :"]"]

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
              :pipe_op -> &parse_infix_expression/2
              :type_op -> &parse_infix_expression/2
              :dual_op -> &parse_infix_expression/2
              :mult_op -> &parse_infix_expression/2
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
    parser = next_token(parser)

    {expression, parser} = parse_expression(parser)

    if peek_token(parser) == :")" do
      {expression, next_token(parser)}
    else
      meta = current_meta(parser)
      parser = put_error(parser, {meta, "missing closing parentheses"})

      {{:__error__, meta, ["missing closing parentheses"]}, next_token(parser)}
    end
  end

  defp parse_nil_literal(%{current_token: {nil, meta}} = parser) do
    ast = encode_literal(parser, nil, meta)
    {ast, parser}
  end

  defp parse_kw_identifier(%{current_token: {:kw_identifier, meta, token}} = parser) do
    parser = next_token(parser)
    {expr, parser} = parse_expression(parser, precedence: @kw_identifier)

    token = encode_literal(parser, token, meta)
    pair = {token, expr}
    {pair, parser}
  end

  defp parse_bracketless_kw_list(%{current_token: {:kw_identifier, meta, token}} = parser) do
    parser = next_token(parser)

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

  defp parse_assoc_op(%{current_token: {:assoc_op, _, _token}} = parser, key) do
    parser = next_token(parser)
    {expr, parser} = parse_expression(parser, precedence: @assoc_op)

    pair = {key, expr}
    {pair, parser}
  end

  defp parse_comma_list(parser, opts \\ []) do
    opts = Keyword.put(opts, :precedence, @comma)
    {expr, parser} = parse_expression(parser, opts)
    items = [expr]

    {items, parser} =
      while peek_token(parser) == :"," <- {items, parser} do
        parser = parser |> next_token() |> next_token()

        {item, parser} = parse_expression(parser, opts)

        {[item | items], parser}
      end

    {Enum.reverse(items), parser}
  end

  defp parse_prefix_expression(parser) do
    token = current_token(parser)
    meta = current_meta(parser)
    precedence = current_precedence(parser)
    parser = next_token(parser)
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    ast = {token, meta, [rhs]}

    {ast, eat_eol(parser)}
  end

  # """
  # A stab expression without a lhs is only possible as the argument to an anonymous function

  # ```elixir
  # fn -> :ok end
  # ```
  # """

  defp parse_stab_expression(parser) do
    token = current_token(parser)
    meta = current_meta(parser)
    parser = eat_at(parser, :eol, 1)
    exprs = []

    {exprs, parser} =
      while peek_token(parser) not in [:eof, :end] <- {exprs, parser} do
        parser = next_token(parser)
        {ast, parser} = parse_expression(parser, top: true)

        parser = eat_at(parser, :eol, 1)

        {[ast | exprs], eat_eol(parser)}
      end

    rhs =
      case exprs do
        [ast] -> ast
        exprs -> {:__block__, [], Enum.reverse(exprs)}
      end

    ast =
      [{token, meta, [[], rhs]}]

    {ast, eat_eol(parser)}
  end

  # """
  # A stab expression with a lhs is present in case, cond, and try blocks, as well as macros.

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
        current_sd = parser.stab_depth
        parser = eat_at(parser, :eol, 1)
        exprs = []

        {exprs, parser} =
          while peek_token(parser) not in [:eof, :end] <- {exprs, parser} do
            parser = next_token(parser)
            {ast, parser} = parse_expression(parser, top: true)

            parser = eat_at(parser, :eol, 1)

            {[ast | exprs], eat_eol(parser)}
          end

        rhs =
          case exprs do
            [ast] -> ast
            exprs -> {:__block__, [], Enum.reverse(exprs)}
          end

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
            {:__block__, _, [ast]} -> ast
            [ast] -> ast
            block -> block
          end

        lhs =
          case lhs do
            {:comma, _, lhs} -> lhs
            lhs -> [lhs]
          end

        ast =
          [{token, [depth: parser.stab_depth] ++ meta, [lhs, rhs]}] ++ Enum.reverse(stabs)

        {ast, eat_eol(parser)}
    end
  end

  defp parse_comma(parser, lhs) do
    parser = parser |> next_token() |> eat_eol()
    {exprs, parser} = parse_comma_list(parser)

    {{:comma, [], [lhs | exprs]}, eat_eol(parser)}
  end

  defp parse_infix_expression(parser, lhs) do
    token = current_token(parser)
    meta = current_meta(parser)
    precedence = current_precedence(parser)
    # we save this in case the next expression is an error
    pre_parser = parser
    parser = parser |> next_token() |> eat_eol()
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
          {:not, meta, [{:in, [], [lhs, rhs]}]}

        :when ->
          lhs =
            case lhs do
              {:comma, _, lhs} -> lhs
              lhs -> [lhs]
            end

          {token, meta, lhs ++ [rhs]}

        _ ->
          {token, meta, [lhs, rhs]}
      end

    {ast, eat_eol(parser)}
  end

  defp parse_access_expression(parser, lhs) do
    precedence = current_precedence(parser)
    parser = parser |> next_token() |> eat_eol()
    meta = current_meta(parser)
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    ast = {{:., meta, [Access, :get]}, meta ++ [from_brackets: true], [lhs, rhs]}

    {ast, eat_eol(next_token(parser))}
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

  defp parse_do_block(%{current_token: {:do, _}} = parser, lhs) do
    do_meta = current_meta(parser)
    parser = parser |> next_token() |> eat_eol()
    exprs = [do: []]

    parser = inc_stab_depth(parser)

    {exprs, parser} =
      while current_token(parser) not in [:end, :eof] <- {exprs, parser} do
        [{type, current_exprs} | rest] = exprs

        {exprs, parser} =
          while current_token(parser) not in [:end, :block_identifier, :eof] <- {current_exprs, parser} do
            {ast, parser} = parse_expression(parser, top: true)

            parser = next_token(parser)

            parser =
              if current_token(parser) == :end do
                parser
              else
                parser |> next_token() |> eat_eol()
              end

            {[ast | current_exprs], parser}
          end

        case parser do
          %{current_token: {:block_identifier, _, token}} ->
            {[{token, []}, {type, exprs} | rest], parser |> next_token() |> eat_eol()}

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
        expr =
          if length(expr) == 1 do
            List.first(expr)
          else
            {:__block__, [], Enum.reverse(expr)}
          end

        {type, expr}
      end

    ast =
      case lhs do
        {token, meta, Elixir} ->
          {token, [do: do_meta, end: end_meta] ++ meta, [exprs]}

        {token, meta, args} when is_list(args) ->
          {token, [do: do_meta, end: end_meta] ++ meta, args ++ [exprs]}
      end

    parser = dec_stab_depth(parser)
    {ast, parser}
  end

  defp parse_dot_expression(parser, lhs) do
    token = current_token(parser)
    precedence = current_precedence(parser)

    case peek_token_type(parser) do
      type when type in [:identifier, :paren_identifier] ->
        parser = next_token(parser)

        {{rhs, _, args}, parser} = parse_expression(parser, precedence: precedence)

        args =
          if args == Elixir do
            []
          else
            args
          end

        ast = {{token, [], [lhs, rhs]}, [], args}

        {ast, eat_eol(parser)}

      _ ->
        parser = next_token(parser)
        {rhs, parser} = parse_expression(parser)
        ast = {{token, [], [lhs, rhs]}, [], []}

        {ast, eat_eol(parser)}
    end
  end

  defp parse_anon_function(%{current_token: {:fn, _}} = parser) do
    meta = current_meta(parser)
    parser = parser |> next_token() |> eat_eol()

    parser = inc_stab_depth(parser)

    {ast, parser} = parse_expression(parser, top: true)

    parser = dec_stab_depth(parser)

    dbg(parser)

    parser =
      case peek_token(parser) do
        :end ->
          next_token(parser)

        _ ->
          put_error(parser, {meta, "missing closing end for anonymous function"})
      end

    {{:fn, meta, ast}, parser}
  end

  defp parse_dot_call_expression(parser, lhs) do
    parser = parser |> next_token() |> eat_eol()

    if peek_token(parser) == :")" do
      ast = {{:., [], [lhs]}, [], []}
      {ast, next_token(parser)}
    else
      {pairs, parser} = parse_comma_list(parser |> next_token() |> eat_eol())
      ast = {{:., [], [lhs]}, [], pairs}

      {ast, parser |> next_token() |> eat_eol()}
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

    args =
      for token <- tokens do
        case token do
          token when is_binary(token) ->
            token

          {_start, _stop, tokens} ->
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
                  stab_depth: 0
                }

                {ast, _parser} = parse_expression(parser |> next_token() |> next_token())
                ast
              end

            {:"::", meta,
             [{{:., meta, [Kernel, :to_string]}, meta ++ [from_interpolation: true], [ast]}, {:binary, [], Elixir}]}
        end
      end

    {{{:., meta, [:erlang, :binary_to_atom]}, meta, [{:<<>>, meta, args}, :utf8]}, parser}
  end

  defp parse_boolean(%{current_token: {bool, meta}} = parser) do
    bool = encode_literal(parser, bool, meta)

    {bool, parser}
  end

  defp parse_int(%{current_token: {:int, {_, _, int} = meta, _}} = parser) do
    int = encode_literal(parser, int, meta)
    {int, parser}
  end

  defp parse_string(%{current_token: {:bin_string, meta, [string]}} = parser) do
    string = encode_literal(parser, string, meta)
    {string, parser}
  end

  defp parse_string(%{current_token: {:bin_string, _, tokens}} = parser) do
    meta = current_meta(parser)

    args =
      for token <- tokens do
        case token do
          token when is_binary(token) ->
            token

          {_start, _stop, tokens} ->
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
                  stab_depth: 0
                }

                {ast, _parser} = parse_expression(parser |> next_token() |> next_token())
                ast
              end

            {:"::", meta,
             [{{:., meta, [Kernel, :to_string]}, meta ++ [from_interpolation: true], [ast]}, {:binary, [], Elixir}]}
        end
      end

    {{:<<>>, meta, args}, parser}
  end

  defp parse_alias(%{current_token: {:alias, _, alias}} = parser) do
    meta = current_meta(parser)
    aliases = [alias]

    {aliases, parser} =
      while peek_token(parser) == :. and peek_token(next_token(parser)) == :alias <-
              {aliases, parser} do
        parser = parser |> next_token() |> next_token()
        %{current_token: {:alias, _, alias}} = parser

        {[alias | aliases], parser}
      end

    {{:__aliases__, meta, Enum.reverse(aliases)}, parser}
  end

  defp parse_map_literal(%{current_token: {:%{}, _}} = parser) do
    meta = current_meta(parser)
    parser = parser |> next_token() |> next_token() |> eat_eol()

    if current_token(parser) == :"}" do
      {{:%{}, meta, []}, parser}
    else
      {pairs, parser} = parse_comma_list(parser, is_map: true)

      parser = eat_at(parser, :eol, 1)

      parser =
        case peek_token(parser) do
          :"}" ->
            next_token(parser)

          _ ->
            put_error(parser, {current_meta(parser), "missing closing brace for map"})
        end

      {{:%{}, meta, pairs}, parser}
    end
  end

  defp parse_tuple_literal(%{current_token: {:"{", _}} = parser) do
    meta = current_meta(parser)
    parser = parser |> next_token() |> eat_eol()

    if current_token(parser) == :"}" do
      {{:{}, meta, []}, parser}
    else
      {pairs, parser} = parse_comma_list(parser)

      parser = eat_at(parser, :eol, 1)

      parser =
        case peek_token(parser) do
          :"}" ->
            parser |> next_token() |> eat_eol()

          _ ->
            put_error(parser, {current_meta(parser), "missing closing brace for tuple"})
        end

      if length(pairs) == 2 do
        {pairs |> List.wrap() |> List.to_tuple(), parser}
      else
        {{:{}, meta, List.wrap(pairs)}, parser}
      end
    end
  end

  defp parse_list_literal(%{current_token: {:"[", meta}} = parser) do
    parser = parser |> next_token() |> eat_eol()

    if current_token(parser) == :"]" do
      {[], parser}
    else
      {pairs, parser} = parse_comma_list(parser, is_list: true)

      parser = eat_at(parser, :eol, 1)

      parser =
        case peek_token(parser) do
          :"]" ->
            next_token(parser)

          _ ->
            put_error(parser, {current_meta(parser), "missing closing bracket for list"})
        end

      list = encode_literal(parser, List.wrap(pairs), meta)
      {list, parser}
    end
  end

  defp parse_identifier(%{current_token: {:paren_identifier, _, token}} = parser) do
    meta = current_meta(parser)

    parser =
      parser
      |> next_token()
      |> eat_eol()

    error_meta = current_meta(parser)

    if peek_token(parser) == :")" do
      {{token, meta, []}, next_token(parser)}
    else
      {pairs, parser} =
        parser
        |> next_token()
        |> eat_eol()
        |> parse_comma_list()

      parser = eat_at(parser, :eol, 1)

      parser =
        case peek_token(parser) do
          :")" ->
            next_token(parser)

          _ ->
            put_error(parser, {error_meta, "missing closing parentheses for function invocation"})
        end

      {{token, meta, List.wrap(pairs)}, parser}
    end
  end

  defp parse_identifier(%{current_token: {:bracket_identifier, _, token}} = parser) do
    ident_meta = current_meta(parser)
    parser = parser |> next_token() |> eat_eol()

    meta = current_meta(parser)
    {key, parser} = parse_expression(parser |> next_token() |> eat_eol())
    ast = {{:., meta, [Access, :get]}, meta ++ [from_brackets: true], [{token, ident_meta, Elixir}, key]}
    {ast, parser |> next_token() |> eat_eol()}
  end

  @operators [
    :"=>",
    :->,
    :+,
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
  defp parse_identifier(%{current_token: {type, _, token}} = parser) when type in [:identifier, :do_identifier] do
    meta = current_meta(parser)

    if peek_token(parser) in ([:";", :eol, :eof, :",", :")", :do, :., :"}", :"]"] ++ @operators) do
      {{token, meta, Elixir}, parser}
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
        {{token, meta, Enum.reverse(args)}, parser}
      end
    end
  end

  def tokenize(code, opts) do
    tokens =
      case code |> String.to_charlist() |> :elixir_tokenizer.tokenize(1, Keyword.put(opts, :check_terminators, false)) do
        {:ok, _, _, _, tokens} ->
          tokens

        {:error, _, _, [], tokens} ->
          Enum.reverse(tokens)
      end

    tokens ++ [:eof]
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
    dbg()

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

  def peek_token(%{peek_token: {token, _, _}}) do
    token
  end

  def peek_token(%{peek_token: {token, _}}) do
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

  def current_meta(%{current_token: {_token, {line, col, _}, _}}) do
    [line: line, column: col]
  end

  def current_meta(%{current_token: {_token, {line, col, _}}}) do
    [line: line, column: col]
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
    meta = [line: line, column: col]

    case parser.literal_encoder.(literal, meta) do
      {:ok, ast} ->
        ast

      {:error, reason} ->
        Logger.error(reason)
        literal
    end
  end

  defp put_error(parser, error) do
    update_in(parser.errors, &[error | &1])
  end

  defp validate_peek(parser, current_type) do
    peek = peek_token_type(parser)

    if peek != :no_peek and not valid_peek?(current_type, peek) do
      parser =
        if peek in [:")", :"]", :"}"] do
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

  defp valid_peek?(:"}", ptype) do
    ptype in (@operators ++ [:"[", :";", :eol, :eof, :",", :")", :do, :., :"}", :"]", :end])
  end

  defp valid_peek?(_ctype, ptype) do
    ptype in (@operators ++ [:";", :eol, :eof, :",", :")", :do, :., :"}", :"]", :end])
  end
end
