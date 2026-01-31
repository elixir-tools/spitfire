defmodule Spitfire.ContextTest do
  use ExUnit.Case, async: true

  import Spitfire.Context, only: [with_state: 3, with_state: 4]

  test "restores updated keys after block" do
    parser = %{mode: :original, keep: :ok}

    restored =
      with_state(parser, %{mode: :temp, new_key: :temp}, fn parser ->
        assert parser[:mode] == :temp
        assert parser[:new_key] == :temp
        Map.merge(parser, %{mode: :inner, new_key: :inner})
      end)

    assert restored == %{mode: :original, keep: :ok}
  end

  test "captures state before restore" do
    parser = %{mode: :original}

    {{value, captured}, restored} =
      with_state(parser, %{mode: :temp, flag: false}, [capture: [:mode, :flag]], fn parser ->
        {:ok, Map.merge(parser, %{mode: :final, flag: true})}
      end)

    assert value == :ok
    assert captured == %{mode: :final, flag: true}
    assert restored == %{mode: :original}
  end

  test "nested calls restore to outer then original" do
    parser = %{mode: :base}

    {value, restored} =
      with_state(parser, %{mode: :outer, outer: true}, fn parser ->
        assert parser[:mode] == :outer
        assert parser[:outer]

        {inner_value, parser} =
          with_state(parser, %{mode: :inner, inner: true}, fn parser ->
            assert parser[:mode] == :inner
            assert parser[:outer]
            assert parser[:inner]
            {:inner, Map.put(parser, :mode, :inner_set)}
          end)

        assert inner_value == :inner
        assert parser[:mode] == :outer
        assert parser[:outer]
        refute Map.has_key?(parser, :inner)

        {:outer, parser}
      end)

    assert value == :outer
    assert restored == %{mode: :base}
  end
end
