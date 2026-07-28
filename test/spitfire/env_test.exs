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

defmodule Spitfire.EnvTest.Imported do
  @moduledoc false
  def imported_function, do: :ok
  defmacro imported_macro, do: :ok
end

defmodule Spitfire.EnvTest.Using do
  @moduledoc false

  defmacro __using__(_opts) do
    quote do
      import Spitfire.EnvTest.Imported
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

  test "expand/2 preserves the environment at an untagged cursor" do
    code =
      quote do
        import Spitfire.EnvTest.Imported

        __cursor__()
      end

    assert {ast, state, final_env, cursor_env} = Env.expand(code, "foo.ex")

    assert {^ast, ^state, ^final_env, %{default: ^cursor_env}} =
             Env.expand_with_cursor_envs(code, "foo.ex")

    assert imported?(cursor_env.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
    assert imported?(cursor_env.macros, Spitfire.EnvTest.Imported, {:imported_macro, 0})
  end

  test "expand/2 returns the last cursor environment" do
    code =
      tagged_code("""
      __cursor__(:first)
      import Spitfire.EnvTest.Imported
      __cursor__(:last)
      """)

    assert {_, _, _, cursor_env} = Env.expand(code, "foo.ex")
    assert imported?(cursor_env.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
  end

  test "returns independent environments for multiple tagged cursors" do
    code =
      tagged_code("""
      __cursor__(:before_import)
      import Spitfire.EnvTest.Imported
      __cursor__(:after_import)
      """)

    assert {_, _, _, %{before_import: before_import, after_import: after_import}} =
             Env.expand_with_cursor_envs(code, "foo.ex")

    refute imported?(before_import.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
    assert imported?(after_import.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
  end

  test "captures functions and macros imported through use" do
    code =
      tagged_code("""
      __cursor__(:before_use)
      use Spitfire.EnvTest.Using
      __cursor__(:after_use)
      """)

    assert {_, _, _, %{before_use: before_use, after_use: after_use}} =
             Env.expand_with_cursor_envs(code, "foo.ex")

    refute imported?(before_use.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
    refute imported?(before_use.macros, Spitfire.EnvTest.Imported, {:imported_macro, 0})
    assert imported?(after_use.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
    assert imported?(after_use.macros, Spitfire.EnvTest.Imported, {:imported_macro, 0})
  end

  test "keeps nested and sibling module imports lexically isolated" do
    code =
      tagged_code("""
      defmodule Parent do
        __cursor__(:parent)

        defmodule ImportedChild do
          import Spitfire.EnvTest.Imported
          __cursor__(:imported_child)
        end

        defmodule Sibling do
          __cursor__(:sibling)
        end
      end
      """)

    assert {_, _, _, cursor_envs} = Env.expand_with_cursor_envs(code, "foo.ex")

    refute imported?(cursor_envs.parent.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
    assert imported?(cursor_envs.imported_child.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
    refute imported?(cursor_envs.sibling.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
  end

  test "duplicate IDs use the later cursor" do
    code =
      tagged_code("""
      __cursor__(:same)
      import Spitfire.EnvTest.Imported
      __cursor__(:same)
      """)

    assert {_, _, _, %{same: cursor_env}} = Env.expand_with_cursor_envs(code, "foo.ex")
    assert imported?(cursor_env.functions, Spitfire.EnvTest.Imported, {:imported_function, 0})
  end

  test "uses :default for untagged cursors and handles attribute-wrapped cursors" do
    code = {:__block__, [], [{:@, [], [cursor(:tagged)]}, {:__cursor__, [], []}]}

    assert {_, _, _, %{tagged: _, default: _}} = Env.expand_with_cursor_envs(code, "foo.ex")
  end

  test "returns no cursor environments and does not leak cursor state between expansions" do
    tagged = cursor(:first_expansion)

    assert {_, state, _, %{first_expansion: _}} = Env.expand_with_cursor_envs(tagged, "first.ex")
    assert state |> Map.keys() |> Enum.sort() == [:attrs, :functions, :macros]

    assert {_, next_state, _, %{}} = Env.expand_with_cursor_envs(quote(do: :ok), "second.ex")
    assert next_state |> Map.keys() |> Enum.sort() == [:attrs, :functions, :macros]
  end

  defp imported?(imports, module, function) do
    case List.keyfind(imports, module, 0) do
      {^module, functions} -> function in functions
      nil -> false
    end
  end

  defp cursor(id), do: {:__cursor__, [cursor_id: id], []}

  defp tagged_code(source) do
    source
    |> Code.string_to_quoted!()
    |> Macro.postwalk(fn
      {:__cursor__, meta, [id]} -> {:__cursor__, Keyword.put(meta, :cursor_id, id), []}
      node -> node
    end)
  end
end
