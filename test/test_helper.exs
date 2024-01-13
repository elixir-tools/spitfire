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

      Kernel.==(lhs, rhs)
    end
  end
end

ExUnit.start()
