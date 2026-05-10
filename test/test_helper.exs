defmodule Spitfire.Assertions do
  @moduledoc """
  Reusable assertion macros and check functions used in Spitfire tests.
  """

  @doc """
  A wrapper over `Spitfire.parse/1` that normalizes returned `:error` tuple.
  """
  def spitfire_parse(code) do
    case Spitfire.parse(code) do
      {:ok, ast} -> {:ok, ast}
      {:error, _ast, _errors} -> {:error, :parse_error}
      {:error, :no_fuel_remaining} -> {:error, :no_fuel_remaining}
    end
  end

  @doc """
  Converts a string of Elixir code into a quoted expression in a format
  usable for comparison with Spitfire output.
  """
  def s2q(code, opts \\ []) do
    Code.string_to_quoted(
      code,
      Keyword.merge([columns: true, token_metadata: true, emit_warnings: false], opts)
    )
  end

  @doc """
  Asserts that Spitfire output conforms to Elixir parser output for a code snippet.
  """
  defmacro assert_conforms(code) do
    quote location: :keep do
      spitfire = unquote(__MODULE__).spitfire_parse(unquote(code))
      elixir = unquote(__MODULE__).s2q(unquote(code))

      case elixir do
        {:ok, _} -> assert spitfire == elixir
        {:error, _} -> assert {:error, _} = spitfire
      end
    end
  end

  @doc """
  Asserts that both Elixir parser and Spitfire parser return errors for a code snippet.
  """
  defmacro assert_errors(code) do
    quote location: :keep do
      assert {:error, _} = unquote(__MODULE__).s2q(unquote(code))
      assert {:error, _} = unquote(__MODULE__).spitfire_parse(unquote(code))
    end
  end
end

ExUnit.start(exclude: [:skip])
