defmodule Spitfire.Context do
  @moduledoc false

  @doc """
  Temporarily applies parser state updates and restores them after the block.

  Use the `:capture` option to return a map of selected keys as they were
  at the end of the block, before state is restored.
  """
  defmacro with_state(parser, updates, opts \\ [], do: block) do
    quote location: :keep, generated: true do
      parser = unquote(parser)
      updates = unquote(updates)
      opts = unquote(opts)

      updates =
        if is_map(updates) do
          updates
        else
          Map.new(updates)
        end

      old =
        Enum.reduce(updates, %{}, fn {key, _value}, acc ->
          Map.put(acc, key, {Map.has_key?(parser, key), Map.get(parser, key)})
        end)

      parser = Map.merge(parser, updates)
      result = unquote(block)
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

      case result do
        {value, parser} ->
          captured =
            Enum.reduce(capture_keys, %{}, fn key, acc ->
              Map.put(acc, key, Map.get(parser, key))
            end)

          parser = restore.(parser)

          if capture_keys == [] do
            {value, parser}
          else
            {{value, captured}, parser}
          end

        parser when is_map(parser) ->
          captured =
            Enum.reduce(capture_keys, %{}, fn key, acc ->
              Map.put(acc, key, Map.get(parser, key))
            end)

          parser = restore.(parser)

          if capture_keys == [] do
            parser
          else
            {{parser, captured}, parser}
          end
      end
    end
  end
end
