defmodule Spitfire do
  require Logger
  import Spitfire.While

  @lowest {:left, 1}
  @doo {:left, 5}
  @stab_op {:right, 10}
  @comma {:left, 20}
  @leftstab_op {:left, 50}
  @whenn {:right, 50}
  @type_op {:right, 60}
  @pipe_op {:right, 70}
  @assoc_op {:right, 80}
  @capture_op {:nonassoc, 90}
  @match_op {:right, 100}
  @or_op {:left, 120}
  @and_op {:left, 130}
  @comp_op {:left, 140}
  @rel_op {:left, 150}
  @arrow_op {:left, 160}
  @in_op {:left, 170}
  @xor_op {:left, 180}
  @ternary_op {:right, 190}
  @concat_op {:right, 200}
  @range_op {:right, 200}
  @dual_op {:left, 210}
  @mult_op {:left, 220}
  @power_op {:left, 230}
  @unary_op {:unassoc, 300}
  @dot_call_op {:left, 310}
  @dot_op {:left, 310}
  @at_op {:unassoc, 320}
  @dot_identifier {:unassoc, 330}
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
    do: @doo,
    stab_op: @stab_op,
    leftstab_op: @leftstab_op,
    when: @whenn,
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
    parser = new(code) |> next_token() |> next_token()

    parse_program(parser)
  end

  defp parse_program(parser) do
    exprs = []

    {exprs, _parser} =
      while current_token(parser) != :eof <- {exprs, parser} do
        {ast, parser} = parse_expression(parser)

        parser =
          if peek_token(parser) == :eol do
            next_token(parser)
          else
            parser
          end

        {[ast | exprs], eat_eol(parser)}
      end

    case exprs do
      [ast] -> ast
      exprs -> {:__block__, [], Enum.reverse(exprs)}
    end
  end

  defp parse_nil_literal(parser) do
    {nil, parser}
  end

  defp parse_kw_identifier(%{current_token: {:kw_identifier, _, token}} = parser) do
    {token, parser}
  end

  defp parse_expression(parser, {associativity, precedence} \\ @lowest) do
    prefix =
      case current_token_type(parser) do
        :identifier -> &parse_identifier/1
        :do_identifier -> &parse_identifier/1
        :paren_identifier -> &parse_identifier/1
        :alias -> &parse_alias/1
        :kw_identifier -> &parse_kw_identifier/1
        :int -> &parse_int/1
        :atom -> &parse_atom/1
        :bin_string -> &parse_string/1
        :fn -> &parse_anon_function/1
        :at_op -> &parse_prefix_expression/1
        :unary_op -> &parse_prefix_expression/1
        :"[" -> &parse_list_literal/1
        :%{} -> &parse_map_literal/1
        nil -> &parse_nil_literal/1
        _ -> nil
      end

    cond do
      prefix == nil ->
        IO.puts(
          IO.ANSI.red() <>
            "#{__ENV__.line}: unknown prefix: #{current_token_type(parser)}" <> IO.ANSI.reset()
        )

        {:error, next_token(parser)}

      true ->
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

        while peek_token(parser) not in [:",", :assoc_op, :eol, :eof, :->] && calc_prec.(parser) <-
                {left, parser} do
          infix =
            case peek_token_type(parser) do
              :dual_op -> &parse_infix_expression/2
              :mult_op -> &parse_infix_expression/2
              :concat_op -> &parse_infix_expression/2
              :arrow_op -> &parse_infix_expression/2
              :ternary_op -> &parse_infix_expression/2
              :or_op -> &parse_infix_expression/2
              :and_op -> &parse_infix_expression/2
              :comp_op -> &parse_infix_expression/2
              :rel_op -> &parse_infix_expression/2
              :in_op -> &parse_infix_expression/2
              :xor_op -> &parse_infix_expression/2
              :range_op -> &parse_range_expression/2
              :do -> &parse_do_block/2
              :. -> &parse_dot_expression/2
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

  defp parse_prefix_expression(parser) do
    token = current_token(parser)
    precedence = current_precedence(parser)
    parser = parser |> next_token()
    {rhs, parser} = parse_expression(parser, precedence)
    ast = {token, [], [rhs]}

    {ast, eat_eol(parser)}
  end

  defp parse_infix_expression(parser, lhs) do
    token = current_token(parser)
    precedence = current_precedence(parser)
    parser = parser |> next_token()
    {rhs, parser} = parse_expression(parser, precedence)

    ast =
      case token do
        :"not in" ->
          {:not, [], [{:in, [], [lhs, rhs]}]}

        _ ->
          {token, [], [lhs, rhs]}
      end

    {ast, eat_eol(parser)}
  end

  defp parse_range_expression(parser, lhs) do
    token = current_token(parser)
    precedence = current_precedence(parser)
    parser = parser |> next_token()
    {rhs, parser} = parse_expression(parser, precedence)

    if peek_token(parser) == :ternary_op do
      parser = next_token(parser) |> next_token()
      {rrhs, parser} = parse_expression(parser, precedence)

      {{:"..//", [], [lhs, rhs, rrhs]}, eat_eol(parser)}
    else
      {{token, [], [lhs, rhs]}, eat_eol(parser)}
    end
  end

  defp parse_do_block(%{current_token: {:do, _}} = parser, lhs) do
    parser = next_token(parser) |> eat_eol()
    exprs = []

    {exprs, parser} =
      while current_token(parser) != :end <- {exprs, parser} do
        {ast, parser} = parse_expression(parser)

        parser = next_token(parser)

        parser =
          if current_token(parser) == :end do
            parser
          else
            parser |> next_token() |> eat_eol()
          end

        {[ast | exprs], parser}
      end

    exprs = Enum.reverse(exprs)

    ast =
      case lhs do
        {token, meta, Elixir} ->
          {token, meta, [[do: {:__block__, [], exprs}]]}

        {token, meta, args} when is_list(args) ->
          {token, meta, args ++ [[do: {:__block__, [], exprs}]]}
      end

    {ast, parser}
  end

  defp parse_dot_expression(parser, lhs) do
    token = current_token(parser)
    # parser = parser |> next_token()

    case peek_token_type(parser) do
      type when type in [:identifier, :paren_identifier] ->
        parser = next_token(parser)
        {{rhs, _, args}, parser} = parse_expression(parser)

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
    parameters = []

    parser =
      if peek_token(parser) == :"(" do
        next_token(parser)
      else
        parser
      end

    {parameters, parser} =
      while peek_token(parser) != :-> && peek_token(parser) != :")" <- {parameters, parser} do
        parser = next_token(parser)
        {parameter, parser} = parse_expression(parser)

        {List.wrap(parameter) ++ parameters, parser}
      end

    parser =
      cond do
        current_token(parser) == :fn and peek_token(parser) == :-> ->
          parser |> next_token() |> next_token() |> eat_eol()

        peek_token(parser) == :")" and peek_token(next_token(parser)) == :-> ->
          parser |> next_token() |> next_token() |> next_token() |> eat_eol()

        peek_token(parser) == :-> ->
          parser |> next_token() |> next_token() |> eat_eol()

        true ->
          raise "boom"
      end

    asts = []

    {asts, parser} =
      while current_token(parser) != :end <- {asts, parser} do
        {ast, parser} = parse_expression(parser)

        {[ast | asts], eat_eol(next_token(parser))}
      end

    parser = next_token(parser)

    ast =
      {:fn, [], [{:->, [], [Enum.reverse(parameters), {:__block__, [], Enum.reverse(asts)}]}]}

    {ast, parser}
  end

  defp parse_atom(%{current_token: {:atom, _, atom}} = parser) do
    {atom, parser}
  end

  defp parse_int(%{current_token: {:int, {_, _, int}, _}} = parser) do
    {int, parser}
  end

  defp parse_string(%{current_token: {:bin_string, _, [string]}} = parser) do
    {string, parser}
  end

  defp parse_alias(%{current_token: {:alias, _, alias}} = parser) do
    # parser = next_token(parser)

    aliases = [alias]

    {aliases, parser} =
      while peek_token(parser) == :. and peek_token(next_token(parser)) == :alias <-
              {aliases, parser} do
        parser = next_token(parser) |> next_token()
        %{current_token: {:alias, _, alias}} = parser

        {[alias | aliases], parser}
      end

    {{:__aliases__, [], Enum.reverse(aliases)}, parser}
  end

  defp parse_map_literal(%{current_token: {:%{}, _}} = parser) do
    parser = next_token(parser)
    pairs = []

    {pairs, parser} =
      while peek_token(parser) != :"}" <- {pairs, parser} do
        parser = next_token(parser)
        {key, parser} = parse_expression(parser)
        key_token_type = current_token_type(parser)

        cond do
          key_token_type == :kw_identifier ->
            {value, parser} = parse_expression(parser |> next_token())
            {[{key, value} | pairs], parser}

          peek_token(parser) == :assoc_op ->
            parser = parser |> next_token() |> next_token()
            {value, parser} = parse_expression(parser)

            case {not (peek_token(parser) == :"}"), peek_token(parser) == :","} do
              {true, true} ->
                {[{key, value} | pairs], next_token(parser)}

              {false, false} ->
                {[{key, value} | pairs], parser}

              {_, true} ->
                {[{key, value} | pairs], parser}
            end

          true ->
            {[:error | pairs], parser}
        end
      end

    {{:%{}, [], Enum.reverse(pairs)}, next_token(parser)}
  end

  defp parse_list_literal(%{current_token: {:"[", _}} = parser) do
    if peek_token(parser) == :"]" do
      {[], next_token(parser)}
    else
      parser = next_token(parser)

      {first_expr, parser} = parse_expression(parser)

      exprs = [first_expr]

      {exprs, parser} =
        while peek_token(parser) == :"," <- {exprs, parser} do
          parser = parser |> next_token() |> next_token()
          {expr, parser} = parse_expression(parser)

          {[expr | exprs], parser}
        end

      {Enum.reverse(exprs), next_token(parser)}
    end
  end

  # defp parse_identifier(%{current_token: {:do_identifier, _, token}} = parser) do
  #   parser = next_token(parser)

  #   # if token is do, then eat the do token and a possible eol token
  #   parser = parser |> next_token() |> eat_eol()

  #   asts = []

  #   # parse each expression in do block
  #   {asts, parser} =
  #     while current_token(parser) not in [:end, :eot] <- {asts, parser} do
  #       {ast, parser} = parse_expression(parser)
  #       {[ast | asts], eat_eol(parser)}
  #     end

  #   # do blocks are a separate keyword list argument, so args are `list, kw`
  #   args = [[do: {:__block__, [], Enum.reverse(asts)}]]
  #   parser = next_token(parser) |> eat_eol()

  #   {{token, [], args}, parser}
  # end

  defp parse_identifier(%{current_token: {:paren_identifier, _, token}} = parser) do
    args = []

    parser = parser |> next_token()

    {args, parser} =
      while peek_token(parser) != :")" <- {args, parser} do
        parser = parser |> next_token() |> then(&eat(:",", &1))
        # parse arguments
        {arg, parser} = parse_expression(parser)

        # eat comma
        {[arg | args], parser}
      end

    parser = next_token(parser)

    {{token, [], Enum.reverse(args)}, parser}
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
    :assoc_op,
    :concat_op,
    :dual_op,
    :ternary_op,
    :in_op
  ]
  defp parse_identifier(%{current_token: {type, _, token}} = parser)
       when type in [:identifier, :do_identifier] do
    cond do
      peek_token(parser) in ([:";", :eol, :eof, :",", :")", :do, :.] ++ @operators) ->
        {{token, [], Elixir}, parser}

      true ->
        parser = next_token(parser)

        parser = push_nesting(parser, 1)
        {first_arg, parser} = parse_expression(parser, @comma)

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
      nestings: []
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
             :ternary_op,
             :range_op,
             :xor_op,
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
end
