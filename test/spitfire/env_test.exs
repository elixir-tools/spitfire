defmodule Spitfire.EnvTest.Functions do
  @moduledoc false
  def foo, do: :ok
  def bar, do: :ok
  def baz, do: :ok
end

defmodule Spitfire.EnvTest.Macros do
  @moduledoc false
  defmacro with_functions(name, block) do
    quote do
      def unquote(name)() do
        import Spitfire.EnvTest.Functions

        unquote(block)
      end
    end
  end
end

defmodule Spitfire.EnvTest do
  use ExUnit.Case, async: true

  alias Spitfire.Env

  test "returns aliases at the given position" do
    {:ok, code} =
      Spitfire.container_cursor_to_quoted("""
      defmodule Foo do
        alias Foo.Bar
        alias Foo.Bar.Baz

        alias Alice, as: Bob

        import Bitwise
        import Spitfire.EnvTest.Macros

        @yolo "hi"

        with_functions :run do
          foo = 1
          
          bar = 2
      """)

    assert {
             _ast,
             _state,
             _env,
             cursor_env
           } = Env.expand(code, "foo.ex")

    # variables in scope
    assert cursor_env.variables == [:foo, :bar]

    # current aliases
    assert cursor_env.aliases == [{Bar, Foo.Bar}, {Baz, Foo.Bar.Baz}, {Bob, Alice}]
    # imported macros
    assert {:defp, 2} in (cursor_env.macros |> List.keyfind(Kernel, 0) |> elem(1))

    # imported functions
    assert {:&&&, 2} in (cursor_env.functions |> List.keyfind(Bitwise, 0) |> elem(1))

    # imported functions that were imported inside a macro
    env_test_functions = cursor_env.functions |> List.keyfind(Spitfire.EnvTest.Functions, 0) |> elem(1)
    assert {:foo, 0} in env_test_functions
    assert {:bar, 0} in env_test_functions
    assert {:baz, 0} in env_test_functions

    # module attributes
    assert "yolo" in cursor_env.attrs
  end
end
