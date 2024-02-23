#!/usr/bin/env elixir

Mix.install([
  :benchee,
  :benchee_html,
  {:spitfire, path: "."}
])

defmodule Runner do
  @moduledoc false
  defmodule While do
    @moduledoc false
    import Spitfire.While

    def while(input) do
      items = []
      token = %{items: input}

      while token.items != [] <- {items, token} do
        {item, token} = get_and_update_in(token.items, fn [item | rest] -> {item, rest} end)
        {[item | items], token}
      end
    end
  end

  defmodule While2 do
    @moduledoc false
    import Spitfire.While2

    def while(input) do
      token = %{items: input}

      while2 token.items != [] <- token do
        get_and_update_in(token.items, fn [item | rest] -> {item, rest} end)
      end
    end
  end

  def new(input) do
    token = %{items: input}

    items =
      recurse(token, &(&1.items != []), fn token ->
        get_and_update_in(token.items, fn [item | rest] -> {item, rest} end)
      end)

    token = Process.get(:while_token)
    Process.delete(:while_token)

    {items, token}
  end

  defp recurse(token, pred, callback) do
    if pred.(token) do
      {item, token} = callback.(token)

      [item | recurse(token, pred, callback)]
    else
      Process.put(:while_token, token)
      []
    end
  end
end

defmodule Bench do
  @moduledoc false
  def run do
    Benchee.run(
      %{
        "Spitfire.parse!" => fn input -> Spitfire.parse!(input) end,
        "Code.string_to_quoted!" => fn input -> Code.string_to_quoted!(input, columns: true, token_metadata: true) end,
        # "while" => &Runner.While.while/1,
        # "while2" => &Runner.While2.while/1,
        # "new" => &Runner.new/1
      },
      # time: 30,
      # memory_time: 5,
      # reduction_time: 5,
      formatters: [
        # Benchee.Formatters.HTML,
        Benchee.Formatters.Console
      ],
      inputs: %{
        # "50" => Enum.to_list(1..50),
        # "100" => Enum.to_list(1..100),
        # "1000" => Enum.to_list(1..1000),
        # "5000" => Enum.to_list(1..5000)
        "small file" => File.read!("mix.exs"),
        "medium file" => File.read!("lib/spitfire.ex"),
        "big file" => File.read!("/home/mitchell/src/elixir/lib/elixir/lib/enum.ex")
        # "test file" => File.read!("test.ex"),
        # "int" => "1",
        # "multi expression" => Enum.map_join(0..10_000, "\n", &to_string/1)
        # "string" => ~S'"foobar"',
        # "identifier" => ~S'foobar',
        # "function call parens" => ~S'foobar(a, b, c)',
        # "function call no parens" => ~S'foobar a, b, c ',
        # "if" => ~S'''
        # if is_true do
        #   :ok
        # else
        #   :error
        # end
        # ''',
        # "case" => ~S'''
        # case foo do
        #   :bar -> :ok
        # end
        # ''',
        # "case 2 prong" => ~S'''
        # case foo do
        #   :bar -> :ok
        #   :baz -> :ok
        # end
        # ''',
        # "fn" => ~S'''
        # fn foo ->
        #   :ok
        # end
        # '''
      }
    )
  end

  def thing(_one, _two, _three, _four) do
    :ok
  end

  def run2 do
    Benchee.run(
      %{
        "tokenize" => fn input ->
          input ++ [:eof]
        end
        # "++" => fn input ->
        #   input ++ [:eof]
        # end,
        # "reverse" => fn input ->
        #   Enum.reverse([:eof | Enum.reverse(input)])
        # end
      },
      inputs: %{
        "lib/spitfire.ex" =>
          "lib/spitfire.ex"
          |> File.read!()
          |> String.to_charlist()
          |> :spitfire_tokenizer.tokenize(1, 1, check_terminators: false, cursor_completion: false)
          |> elem(4)
      }
    )
  end

  def mapvlist do
    Benchee.run(
      %{
        "map access" => fn input ->
          map = %{foo: true, boo: false}
          map[input]
        end,
        "list access" => fn input ->
          list = [foo: true, boo: false]
          list[input]
        end,
        "no access" => fn _input ->
          thing(true, false, true, false)
        end
      },
      inputs: %{
        ":foo" => :foo,
        ":bar" => :bar,
        ":baz" => :baz,
        ":boo" => :boo
      }
    )
  end
end

Bench.run()
