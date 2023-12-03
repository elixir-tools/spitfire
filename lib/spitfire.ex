defmodule Spitfire do
  require Logger
  import Spitfire.While

  @lowest 1
  @equals 2
  @lessgreater 3
  @sum 4
  @product 5
  @prefix 6
  @call 7
  @index 8

  @precedences %{
    :== => @equals,
    :!= => @equals,
    :< => @lessgreater,
    :> => @lessgreater,
    :+ => @sum,
    :- => @sum,
    :/ => @product,
    :* => @product,
    :"(" => @call,
    :"[" => @index
  }

  def tokenize(code) do
    case :elixir_tokenizer.tokenize(String.to_charlist(code), 1, []) do
      {:ok, _, _, [], tokens} ->
        tokens

      {:error, _, _, [], tokens} ->
        Enum.reverse(tokens)
    end
  end

  def new(code) do
    %{
      tokens: tokenize(code),
      current_token: nil,
      peek_token: nil
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

  def current_token(%{current_token: nil}) do
    :eot
  end

  def current_token(%{current_token: {op, _, token}})
      when op in [
             :stab_op,
             :dual_op,
             :mult_op,
             :assoc_op,
             :concat_op
           ] do
    token
  end

  def current_token(%{current_token: {token, _, _}}) do
    token
  end

  def current_token(%{current_token: {token, _}}) do
    token
  end

  def parse(code) do
    parser = new(code) |> next_token() |> next_token()

    asts = []

    {asts, _parser} =
      while parser.tokens != :eot <- {asts, parser} do
        {ast, parser} = parse_expression(parser)
        {[ast | asts], eat_eol(parser)}
      end

    case asts do
      [ast] -> ast
      asts -> {:__block__, [], Enum.reverse(asts)}
    end
  end

  defp parse_nil_literal(parser) do
    {nil, next_token(parser)}
  end

  defp parse_kw_identifier(%{current_token: {:kw_identifier, _, token}} = parser) do
    {token, next_token(parser)}
  end

  defp parse_expression(parser) do
    dbg(parser)

    prefix =
      case current_token_type(parser) do
        :identifier -> &parse_identifier/1
        :paren_identifier -> &parse_identifier/1
        :alias -> &parse_alias/1
        :kw_identifier -> &parse_kw_identifier/1
        :int -> &parse_int/1
        :atom -> &parse_atom/1
        :bin_string -> &parse_string/1
        :fn -> &parse_anon_function/1
        :"[" -> &parse_list_literal/1
        :%{} -> &parse_map_literal/1
        nil -> &parse_nil_literal/1
        :eol -> :eol
        :end -> :end
        _ -> nil
      end

    dbg(prefix)
    dbg(parser)

    cond do
      prefix == nil ->
        Logger.error("unknown prefix: #{current_token_type(parser)}")
        {:error, next_token(parser)}

      prefix in [:eol, :end] ->
        {nil, next_token(parser)}

      true ->
        {left, parser} = prefix.(parser)

        infix =
          case current_token_type(parser) do
            :dual_op -> &parse_infix_expression/2
            :mult_op -> &parse_infix_expression/2
            :concat_op -> &parse_infix_expression/2
            :. -> &parse_dot_expression/2
            _ -> nil
          end

        if infix == nil do
          {left, parser}
        else
          infix.(parser, left)
        end
    end
  end

  defp parse_infix_expression(parser, lhs) do
    token = current_token(parser)
    parser = parser |> next_token()
    {rhs, parser} = parse_expression(parser)
    ast = {token, [], [lhs, rhs]}

    {ast, eat_eol(parser)}
  end

  defp parse_dot_expression(parser, lhs) do
    dbg(parser)
    token = current_token(parser)
    parser = parser |> next_token()

    case current_token_type(parser) do
      type when type in [:identifier, :paren_identifier] ->
        {{rhs, _, args}, parser} = parse_expression(parser)
        ast = {{token, [], [lhs, rhs]}, [], args}

        {ast, eat_eol(parser)}

      _ ->
        {rhs, parser} = parse_expression(parser)
        ast = {{token, [], [lhs, rhs]}, [], []}

        {ast, eat_eol(parser)}
    end
  end

  defp parse_anon_function(%{current_token: {:fn, _}} = parser) do
    parameters = []

    parser = next_token(parser)

    {parameters, parser} =
      while current_token(parser) not in [:->] <- {parameters, parser} do
        {parameter, parser} = parse_argument(parser)

        {List.wrap(parameter) ++ parameters, parser}
      end

    parser = parser |> next_token() |> eat_eol()

    asts = []

    {asts, parser} =
      while current_token(parser) not in [:end] <- {asts, parser} do
        {ast, parser} = parse_expression(parser)
        {[ast | asts], eat_eol(parser)}
      end

    parser = next_token(parser)

    ast =
      {:fn, [], [{:->, [], [Enum.reverse(parameters), {:__block__, [], Enum.reverse(asts)}]}]}

    {ast, next_token(parser)}
  end

  defp parse_atom(%{current_token: {:atom, _, atom}} = parser) do
    {atom, next_token(parser)}
  end

  defp parse_int(%{current_token: {:int, {_, _, int}, _}} = parser) do
    {int, next_token(parser)}
  end

  defp parse_string(%{current_token: {:bin_string, _, [string]}} = parser) do
    {string, next_token(parser)}
  end

  defp parse_map_literal(%{current_token: {:%{}, _}} = parser) do
    parser = next_token(parser) |> next_token()
    pairs = []

    {pairs, parser} =
      while current_token(parser) != :"}" <- {pairs, parser} do
        key_token_type = current_token_type(parser)
        {key, parser} = parse_expression(parser)

        {value, parser} =
          cond do
            key_token_type == :kw_identifier ->
              parse_expression(parser)

            current_token(parser) == :"=>" ->
              parser = next_token(parser)
              parse_expression(parser)

            true ->
              {:error, parser}
          end

        {[{key, value} | pairs], eat(:",", parser)}
      end

    {{:%{}, [], Enum.reverse(pairs)}, next_token(parser)}
  end

  defp parse_list_literal(%{current_token: {:"[", _}} = parser) do
    expressions = []
    parser = next_token(parser)

    {expressions, parser} =
      while current_token(parser) not in [:"]"] <- {expressions, parser} do
        {expression, parser} = parse_expression(parser)

        {[expression | expressions], eat(:",", parser)}
      end

    {Enum.reverse(expressions), next_token(parser)}
  end

  def(foo(:ok, :done)) do
    :ok
  end

  defp parse_identifier(%{current_token: {type, _, token}} = parser)
       when type in [:identifier, :paren_identifier] do
    if token == :def do
      dbg(parser)
    end

    if peek_token(parser) in [:identifier, :paren_identifier, :"(", :do, :alias] do
      # currently on an identifier, might be a variable or a function/macro
      # if it is paren_identifier, definitely a function/macro
      args = []

      {parser, terminals} =
        if peek_token(parser) == :"(" do
          # if next token is paren, then parse until closing paren
          {next_token(parser) |> next_token(), [:")"]}
        else
          # if next token is eol or a do, then parse until that
          {next_token(parser), [:eol, :do]}
        end

      {args, parser} =
        while current_token(parser) not in terminals <- {args, parser} do
          # parse arguments
          {arg, parser} = parse_expression(parser)

          # eat comma
          {List.wrap(arg) ++ args, eat(:",", parser)}
        end

      parser =
        if terminals == [:")"] do
          # if we were parsing until closing paren, then progress to the next token
          next_token(parser)
        else
          # else, we are at eol or do, so keep it
          parser
        end

      {ast, parser} =
        if current_token(parser) == :do do
          # if token is do, then eat the do token and a possible eol token
          parser = parser |> next_token() |> eat_eol()

          dbg(parser)
          asts = []

          # parse each expression in do block
          {asts, parser} =
            while current_token(parser) not in [:end, :eot] <- {asts, parser} do
              {ast, parser} = parse_expression(parser)
              {[ast | asts], eat_eol(parser)} |> dbg()
            end

          # do blocks are a separate keyword list argument, so args are `list, kw`
          {Enum.reverse(args) ++ [[do: {:__block__, [], Enum.reverse(asts)}]], next_token(parser)}
          |> dbg
        else
          {Enum.reverse(args), parser}
        end

      parser = parser |> eat_eol()

      {{token, [], ast}, parser} |> dbg
    else
      {{token, [], []}, next_token(parser)}
    end
  end

  defp parse_alias(%{current_token: {:alias, _, _token}} = parser) do
    aliases = []

    {aliases, parser} =
      while current_token(parser) == :alias <- {aliases, parser} do
        %{current_token: {:alias, _, alias}} = parser

        parser = next_token(parser)

        aliases = [alias | aliases]

        parser =
          if current_token(parser) == :. and peek_token(parser) == :alias do
            next_token(parser)
          else
            parser
          end

        dbg(parser)

        {aliases, parser}
      end

    {{:__aliases__, [], Enum.reverse(aliases)}, parser}
  end

  defp parse_argument(%{current_token: {:alias, _, _token}} = parser) do
    parse_alias(parser)
  end

  defp parse_argument(%{current_token: {type, _, token}} = parser)
       when type in [:identifier] do
    {{token, [], []}, next_token(parser)}
  end

  defp parse_argument(%{current_token: {type, _, token}} = parser)
       when type in [:paren_identifier] do
    {parser, terminals} =
      if peek_token(parser) == :"(" do
        {next_token(parser) |> next_token(), [:")"]}
      end

    args = []

    {args, parser} =
      while current_token(parser) not in terminals <- {args, parser} do
        {arg, parser} = parse_argument(parser)

        {List.wrap(arg) ++ args, parser}
      end

    parser = next_token(parser)

    {{token, [], args}, parser}
  end

  def current_precedence(parser) do
    Map.get(@precedences, parser.current_token, @lowest)
  end
end
