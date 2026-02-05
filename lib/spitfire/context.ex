defmodule Spitfire.Context do
  @moduledoc false

  @doc """
  Temporarily applies parser context updates and restores them after the block.

  This is used to scope parsing contexts (e.g. map entries, stab parsing) so
  context flags do not leak outside the intended region.

  The `updates` argument must be a map.

  Supports two return shapes from `fun`:

    * `{value, parser}` - returns `{value, restored_parser}`
    * `parser` - returns `restored_parser`

  Use the `:capture` option to return a map of selected keys as they were at the
  end of the block, before context is restored.
  """
  def with_context(parser, updates, fun) when is_map(updates) and is_function(fun, 1) do
    with_context(parser, updates, [], fun)
  end

  def with_context(parser, updates, opts, fun) when is_map(updates) and is_list(opts) and is_function(fun, 1) do
    old =
      Enum.reduce(updates, %{}, fn {key, _value}, acc ->
        Map.put(acc, key, {Map.has_key?(parser, key), Map.get(parser, key)})
      end)

    parser = Map.merge(parser, updates)
    result = fun.(parser)

    capture_keys = Keyword.get(opts, :capture, [])

    restore = fn parser ->
      Enum.reduce(old, parser, fn {key, {had_key, value}}, acc ->
        if had_key do
          Map.put(acc, key, value)
        else
          Map.delete(acc, key)
        end
      end)
    end

    capture = fn parser ->
      Enum.reduce(capture_keys, %{}, fn key, acc ->
        Map.put(acc, key, Map.get(parser, key))
      end)
    end

    case result do
      {value, parser} ->
        captured = capture.(parser)
        restored = restore.(parser)

        if capture_keys == [] do
          {value, restored}
        else
          {{value, captured}, restored}
        end

      parser when is_map(parser) ->
        captured = capture.(parser)
        restored = restore.(parser)

        if capture_keys == [] do
          restored
        else
          {{parser, captured}, restored}
        end
    end
  end
end
