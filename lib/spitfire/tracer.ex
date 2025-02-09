defmodule Spitfire.Tracer do
  @moduledoc false

  @trace? Application.compile_env(:spitfire, :trace, false)

  @indent :spitfire_tracer_indent
  defmacro trace(name, attrs \\ Macro.escape(%{}), do: block) do
    traced =
      quote do
        Spitfire.Tracer.start_trace(unquote(name), unquote(attrs))
        result = unquote(block)
        Spitfire.Tracer.stop_trace(unquote(name))
        result
      end

    if @trace? do
      traced
    else
      block
    end
  end

  def start_trace(name, attrs) do
    indent = increment_indent()
    IO.puts("#{String.duplicate(" ", indent * 2)}BEGIN[#{a2s(attrs)}]: #{name}")
    :ok
  end

  def stop_trace(name) do
    IO.puts("#{String.duplicate(" ", indent() * 2)}END: #{name}")
    decrement_indent()
    :ok
  end

  defp indent do
    Process.get(@indent, 0)
  end

  defp increment_indent do
    indent = Process.get(@indent, 0) + 1
    Process.put(@indent, indent)
    indent
  end

  defp decrement_indent do
    indent = Process.get(@indent, 1) - 1
    Process.put(@indent, indent)
    indent
  end

  defp a2s(attrs) do
    Enum.map_join(attrs, ",", fn {k, v} ->
      tag = k |> to_string() |> String.first() |> String.upcase()
      "#{tag}#{v}"
    end)
  end
end
