#!/usr/bin/env elixir

Mix.install([
  :benchee,
  :benchee_html,
  {:spitfire, path: "."}
])

defmodule Bench do
  @moduledoc false
  def run do
    Benchee.run(
      %{
        "Spitfire.parse!" => fn input -> Spitfire.parse!(input) end,
        "Code.string_to_quoted!" => fn input -> Code.string_to_quoted!(input, columns: true, token_metadata: true) end
      },
      formatters: [
        # Benchee.Formatters.HTML,
        Benchee.Formatters.Console
      ],
      inputs: %{
        # "small file" => File.read!("mix.exs"),
        # "medium file" => File.read!("lib/spitfire.ex"),
        # "big file" => File.read!("/home/mitchell/src/elixir/lib/elixir/lib/enum.ex"),
        # "test file" => File.read!("test.ex"),
        # "int" => "1",
        "multi expression" => ~S'''
        1
        2
        3
        4
        '''
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
