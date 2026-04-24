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

  def strip_range_metadata(term) do
    Macro.postwalk(term, fn
      {token, meta, args} when is_list(meta) ->
        {token, Keyword.delete(meta, :range), args}

      other ->
        other
    end)
  end

  def strip_parse_result_range_metadata({:ok, ast, comments}) do
    {:ok, strip_range_metadata(ast), comments}
  end

  def strip_parse_result_range_metadata({:error, ast, errors}) do
    {:error, strip_range_metadata(ast), strip_error_metadata(errors)}
  end

  def strip_parse_result_range_metadata({:error, ast, comments, errors}) do
    {:error, strip_range_metadata(ast), comments, strip_error_metadata(errors)}
  end

  def strip_parse_result_range_metadata({status, ast}) when status in [:ok, :error] do
    {status, strip_range_metadata(ast)}
  end

  def strip_parse_result_range_metadata(other), do: other

  defp strip_metadata_range(meta) do
    Keyword.delete(meta, :range)
  end

  defp strip_error_metadata(errors) when is_list(errors) do
    Enum.map(errors, fn
      {meta, message} when is_list(meta) and is_binary(message) ->
        {strip_metadata_range(meta), message}

      other ->
        other
    end)
  end

  defp strip_error_metadata(errors), do: errors
end

ExUnit.start(exclude: [:skip])
