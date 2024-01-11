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
  @capture_op {:unassoc, 22}
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
  @unary_op {:unassoc, 54}
  @dot_call_op {:left, 56}
  @dot_op {:left, 58}
  @at_op {:unassoc, 60}
  @dot_identifier {:unassoc, 62}
  # Left       5 do.
  # Right     10 stab_op_eol.     %% ->
  # Left      20 ','.
  # Left      40 in_match_op_eol. %% <-, \\ (allowed in matches along =)
  # Right     50 when_op_eol.     %% when
  # Right     60 type_op_eol.     %% ::
  # Right     70 pipe_op_eol.     %% |
  # Right     80 assoc_op_eol.    %% =>
  # Nonassoc  90 capture_op_eol.  %% &
  # Right    100 match_op_eol.    %% =
  # Left     120 or_op_eol.       %% ||, |||, or
  # Left     130 and_op_eol.      %% &&, &&&, and
  # Left     140 comp_op_eol.     %% ==, !=, =~, ===, !==
  # Left     150 rel_op_eol.      %% <, >, <=, >=
  # Left     160 arrow_op_eol.    %% |>, <<<, >>>, <<~, ~>>, <~, ~>, <~>, <|>
  # Left     170 in_op_eol.       %% in, not in
  # Left     180 xor_op_eol.      %% ^^^
  # Right    190 ternary_op_eol.  %% //
  # Right    200 concat_op_eol.   %% ++, --, +++, ---, <>
  # Right    200 range_op_eol.    %% ..
  # Left     210 dual_op_eol.     %% +, -
  # Left     220 mult_op_eol.     %% *, /
  # Left     230 power_op_eol.    %% **
  # Nonassoc 300 unary_op_eol.    %% +, -, !, ^, not, ~~~
  # Left     310 dot_call_op.
  # Left     310 dot_op.          %% .
  # Nonassoc 320 at_op_eol.       %% @
  # Nonassoc 330 dot_identifier.

  @precedences %{
    :"," => @comma,
    :. => @dot_call_op,
    :"(" => @left_paren,
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
    at_op: @at_op,
    dot_identifier: @dot_identifier
  }

  def parse(code) do
    parser = code |> new() |> next_token() |> next_token()

    parse_program(parser)
  end

  defp parse_program(parser) do
    exprs = []

    {exprs, _parser} =
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

    case exprs do
      [ast] -> ast
      exprs -> {:__block__, [], Enum.reverse(exprs)}
    end
  end

  defp parse_grouped_expression(parser) do
    parser = next_token(parser)

    {expression, parser} = parse_expression(parser)

    if peek_token(parser) == :")" do
      {expression, next_token(parser)}
    else
      {:error, parser}
    end
  end

  defp parse_nil_literal(parser) do
    {nil, parser}
  end

  defp parse_kw_identifier(%{current_token: {:kw_identifier, _, token}} = parser) do
    parser = next_token(parser)
    {expr, parser} = parse_expression(parser, precedence: @kw_identifier)

    pair = {token, expr}
    {pair, parser}
  end

  defp parse_assoc_op(%{current_token: {:assoc_op, _, _token}} = parser, key) do
    parser = next_token(parser)
    {expr, parser} = parse_expression(parser, precedence: @assoc_op)

    pair = {key, expr}
    {pair, parser}
  end

  defp parse_expression(parser, opts \\ []) do
    {associativity, precedence} = Keyword.get(opts, :precedence, @lowest)
    # NOTE: the root of an expression list is the only place where a comma is treated like an infix operator
    is_top = Keyword.get(opts, :top, false)

    prefix =
      case current_token_type(parser) do
        :identifier -> &parse_identifier/1
        :do_identifier -> &parse_identifier/1
        :paren_identifier -> &parse_identifier/1
        :alias -> &parse_alias/1
        :kw_identifier -> &parse_kw_identifier/1
        :int -> &parse_int/1
        :atom -> &parse_atom/1
        true -> &parse_boolean/1
        false -> &parse_boolean/1
        :atom_quoted -> &parse_atom/1
        :bin_string -> &parse_string/1
        :fn -> &parse_anon_function/1
        :at_op -> &parse_prefix_expression/1
        :unary_op -> &parse_prefix_expression/1
        :capture_op -> &parse_prefix_expression/1
        :stab_op -> &parse_stab_expression/1
        :"[" -> &parse_list_literal/1
        :"(" -> &parse_grouped_expression/1
        :"{" -> &parse_tuple_literal/1
        :%{} -> &parse_map_literal/1
        nil -> &parse_nil_literal/1
        _ -> nil
      end

    if prefix == nil do
      {row, col} = token_loc(parser.current_token)

      IO.puts(
        IO.ANSI.red() <>
          "#{row}:#{col}: unknown prefix: #{current_token_type(parser)}" <> IO.ANSI.reset()
      )

      {:error, next_token(parser)}
    else
      {left, parser} = prefix.(parser)

      calc_prec = fn parser ->
        {_associativity, power} = peek_precedence(parser)

        precedence =
          case associativity do
            :left -> precedence
            :unassoc -> 0
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

      while parser.last_parsed not in [:unary_op, :at_op] && peek_token(parser) not in terminals && calc_prec.(parser) <-
              {left, parser} do
        infix =
          case peek_token_type(parser) do
            :match_op -> &parse_infix_expression/2
            :when_op -> &parse_infix_expression/2
            :pipe_op -> &parse_infix_expression/2
            :dual_op -> &parse_infix_expression/2
            :mult_op -> &parse_infix_expression/2
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
    end
  end

  defp parse_comma_list(parser) do
    {expr, parser} = parse_expression(parser, precedence: @comma)
    items = [expr]

    {items, parser} =
      while peek_token(parser) == :"," <- {items, parser} do
        parser = parser |> next_token() |> next_token()

        {item, parser} = parse_expression(parser, precedence: @comma)

        {[item | items], parser}
      end

    {Enum.reverse(items), parser}
  end

  defp parse_prefix_expression(parser) do
    token = current_token(parser)
    precedence = current_precedence(parser)
    parser = Map.put(parser, :last_parsed, current_token_type(parser))
    parser = next_token(parser)
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    parser = Map.put(parser, :last_parsed, nil)

    ast = {token, [], [rhs]}

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
      [{token, [], [[], rhs]}]

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

        ast =
          [{token, [depth: parser.stab_depth], [wrap(lhs), rhs]}] ++ Enum.reverse(stabs)

        {ast, eat_eol(parser)}
    end
  end

  defp wrap(nil) do
    [nil]
  end

  defp wrap(other) do
    List.wrap(other)
  end

  defp parse_comma(parser, lhs) do
    parser = parser |> next_token() |> eat_eol()
    {exprs, parser} = parse_comma_list(parser)

    {[lhs | exprs], eat_eol(parser)}
  end

  defp parse_infix_expression(parser, lhs) do
    token = current_token(parser)
    precedence = current_precedence(parser)
    parser = parser |> next_token() |> eat_eol()
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    ast =
      case token do
        :"not in" ->
          {:not, [], [{:in, [], [lhs, rhs]}]}

        :when ->
          {token, [], List.wrap(lhs) ++ [rhs]}

        _ ->
          {token, [], [lhs, rhs]}
      end

    {ast, eat_eol(parser)}
  end

  defp parse_range_expression(parser, lhs) do
    token = current_token(parser)
    precedence = current_precedence(parser)
    parser = next_token(parser)
    {rhs, parser} = parse_expression(parser, precedence: precedence)

    if peek_token(parser) == :ternary_op do
      parser = parser |> next_token() |> next_token()
      {rrhs, parser} = parse_expression(parser, precedence: precedence)

      {{:"..//", [], [lhs, rhs, rrhs]}, eat_eol(parser)}
    else
      {{token, [], [lhs, rhs]}, eat_eol(parser)}
    end
  end

  defp parse_do_block(%{current_token: {:do, _}} = parser, lhs) do
    parser = parser |> next_token() |> eat_eol()
    exprs = [do: []]

    {exprs, parser} =
      while current_token(parser) != :end <- {exprs, parser} do
        [{type, current_exprs} | rest] = exprs

        {exprs, parser} =
          while current_token(parser) not in [:end, :block_identifier] <- {current_exprs, parser} do
            {ast, parser} = parse_expression(inc_stab_depth(parser), top: true)

            parser = next_token(parser)

            parser =
              if current_token(parser) == :end do
                parser
              else
                parser |> next_token() |> eat_eol()
              end

            {[ast | current_exprs], parser}
          end

        parser = dec_stab_depth(parser)

        case parser do
          %{current_token: {:block_identifier, _, token}} ->
            {[{token, []}, {type, exprs} | rest], parser |> next_token() |> eat_eol()}

          _ ->
            {[{type, exprs} | rest], parser}
        end
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
          {token, meta, [exprs]}

        # {token, meta, [[{:<-, _, _} | _] = stabs]} ->
        #   {token, meta, stabs ++ [exprs]}

        {token, meta, args} when is_list(args) ->
          {token, meta, args ++ [exprs]}
      end

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
    parser = next_token(parser)

    {ast, parser} = parse_expression(parser, top: true)

    parser = next_token(parser)

    {{:fn, [], ast}, parser}
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

  defp parse_atom(%{current_token: {:atom, _, atom}} = parser) do
    {atom, parser}
  end

  defp parse_atom(%{current_token: {:atom_quoted, _, atom}} = parser) do
    {atom, parser}
  end

  defp parse_boolean(%{current_token: {bool, _}} = parser) do
    {bool, parser}
  end

  defp parse_int(%{current_token: {:int, {_, _, int}, _}} = parser) do
    {int, parser}
  end

  defp parse_string(%{current_token: {:bin_string, _, [string]}} = parser) do
    {string, parser}
  end

  defp parse_string(%{current_token: {:bin_string, _, tokens}} = parser) do
    args =
      for token <- tokens do
        case token do
          token when is_binary(token) ->
            token

          {_start, _stop, tokens} ->
            # construct a new parser
            parser = %{
              tokens: tokens ++ [:eof],
              current_token: nil,
              peek_token: nil,
              nestings: [],
              stab_depth: 0,
              last_parsed: nil
            }

            {ast, _parser} = parse_expression(parser |> next_token() |> next_token())

            {:"::", [], [{{:., [], [Kernel, :to_string]}, [], [ast]}, {:binary, [], Elixir}]}
        end
      end

    {{:<<>>, [], args}, parser}
  end

  defp parse_alias(%{current_token: {:alias, _, alias}} = parser) do
    aliases = [alias]

    {aliases, parser} =
      while peek_token(parser) == :. and peek_token(next_token(parser)) == :alias <-
              {aliases, parser} do
        parser = parser |> next_token() |> next_token()
        %{current_token: {:alias, _, alias}} = parser

        {[alias | aliases], parser}
      end

    {{:__aliases__, [], Enum.reverse(aliases)}, parser}
  end

  defp parse_map_literal(%{current_token: {:%{}, _}} = parser) do
    parser = next_token(parser)
    parser = parser |> next_token() |> eat_eol()

    if current_token(parser) == :"}" do
      {{:%{}, [], []}, parser}
    else
      {pairs, parser} = parse_comma_list(parser)
      {{:%{}, [], pairs}, parser |> next_token() |> eat_eol()}
    end
  end

  defp parse_tuple_literal(%{current_token: {:"{", _}} = parser) do
    parser = parser |> next_token() |> eat_eol()

    if current_token(parser) == :"}" do
      {{:{}, [], []}, parser}
    else
      {pairs, parser} = parse_comma_list(parser)

      if length(pairs) == 2 do
        {pairs |> List.wrap() |> List.to_tuple(), parser |> next_token() |> eat_eol()}
      else
        {{:{}, [], List.wrap(pairs)}, parser |> next_token() |> eat_eol()}
      end
    end
  end

  defp parse_list_literal(%{current_token: {:"[", _}} = parser) do
    parser = parser |> next_token() |> eat_eol()

    if current_token(parser) == :"]" do
      {[], parser}
    else
      {pairs, parser} = parse_comma_list(parser)

      {List.wrap(pairs), parser |> next_token() |> eat_eol()}
    end
  end

  defp parse_identifier(%{current_token: {:paren_identifier, _, token}} = parser) do
    parser = parser |> next_token() |> eat_eol()

    if peek_token(parser) == :")" do
      {{token, [], []}, next_token(parser)}
    else
      {pairs, parser} = parse_comma_list(parser |> next_token() |> eat_eol())

      {{token, [], List.wrap(pairs)}, parser |> next_token() |> eat_eol()}
    end
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
    :rel_op,
    :and_op,
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
    :dot_call_op,
    :when_op
  ]
  defp parse_identifier(%{current_token: {type, _, token}} = parser) when type in [:identifier, :do_identifier] do
    if peek_token(parser) in ([:";", :eol, :eof, :",", :")", :do, :., :"}", :"]"] ++ @operators) do
      {{token, [], Elixir}, parser}
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
        parse_do_block(parser, {token, [], Enum.reverse(args)})
      else
        {{token, [], Enum.reverse(args)}, parser}
      end
    end
  end

  def tokenize(code) do
    tokens =
      case :elixir_tokenizer.tokenize(String.to_charlist(code), 1, []) do
        {:ok, _, _, _, tokens} ->
          tokens

        {:error, _, _, [], tokens} ->
          Enum.reverse(tokens)
      end

    tokens ++ [:eof]
  end

  def new(code) do
    %{
      tokens: tokenize(code),
      current_token: nil,
      peek_token: nil,
      nestings: [],
      stab_depth: 0,
      last_parsed: nil
    }
  end

  def next_token(%{tokens: :eot, current_token: nil, peek_token: nil} = parser) do
    parser
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
        peek_token: nil
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

  def eat_at(%{tokens: tokens} = parser, token, idx) do
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
             :capture_op,
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
end
