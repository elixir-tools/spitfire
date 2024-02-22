defmodule Spitfire.Env do
  @moduledoc false
  defstruct [
    :ast,
    :store
  ]

  defmodule Store do
    @moduledoc false
    defstruct vars: [],
              aliases: [],
              imports: [],
              outer: nil
  end

  defmacrop continue?(meta, pos, do: block) do
    quote do
      if before?(unquote_splicing([meta, pos])) do
        unquote(block)
      else
        var!(store)
      end
    end
  end

  def new(code) do
    %__MODULE__{ast: code, store: %Spitfire.Env.Store{}}
  end

  def at(%__MODULE__{ast: ast, store: store}, pos) do
    walk(ast, store, pos)
  end

  defp walk([{{:__literal__, meta, [:do]}, {_, _meta, _exprs} = ast}], store, pos) do
    continue? meta, pos do
      walk(ast, store, pos)
    end
  end

  defp walk({:__block__, _, exprs}, store, pos) do
    store = %Spitfire.Env.Store{outer: store}
    iterate(exprs, store, pos)
  end

  defp walk({:->, _, exprs}, store, pos) do
    store = %Spitfire.Env.Store{outer: store}
    iterate(exprs, store, pos)
  end

  defp walk({:=, meta, [{name, _, nil} | _]}, store, pos) do
    continue? meta, pos do
      %{store | vars: [name | store.vars]}
    end
  end

  defp walk({:alias, meta, [alias]}, store, pos) do
    continue? meta, pos do
      module = Macro.to_string(alias)
      {:__aliases__, _, parts} = alias
      name = List.last(parts)
      %{store | aliases: [%{name: name, module: module, raw: alias} | store.aliases]}
    end
  end

  defp walk({_token, meta, exprs}, store, pos) do
    continue? meta, pos do
      iterate(exprs, store, pos)
    end
  end

  defp walk(_, store, _pos) do
    store
  end

  defp iterate(nil, store, _pos) do
    store
  end

  defp iterate([], store, _pos) do
    store
  end

  defp iterate([expr | rest], store, pos) do
    store = walk(expr, store, pos)
    iterate(rest, store, pos)
  end

  defp before?(meta, {cline, ccol}) do
    line = meta[:line]
    col = meta[:col]

    line < cline || (line == cline and col < ccol)
  end
end
