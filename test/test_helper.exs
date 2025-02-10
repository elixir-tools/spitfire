defmodule Spitfire.TestHelpers do
  @moduledoc false
  defmacro lhs == rhs do
    quote do
      lhs =
        Macro.prewalk(unquote(lhs), fn
          {t, meta, a} ->
            {t, [], a}

          ast ->
            ast
        end)

      rhs =
        Macro.prewalk(unquote(rhs), fn
          {t, meta, a} ->
            {t, [], a}

          ast ->
            ast
        end)

      if true do
        import Kernel
        import unquote(__MODULE__), except: [==: 2]

        assert lhs == rhs
      end
    end
  end
end

ExUnit.start(exclude: [:skip])
