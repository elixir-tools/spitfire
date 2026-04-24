defmodule Spitfire.RangeMetadataTest do
  use ExUnit.Case, async: true

  test "binary operators span the whole expression" do
    assert {:ok, {:+, meta, [1, {:*, inner_meta, [2, 3]}]}} = Spitfire.parse("1 + 2 * 3")

    assert meta[:range] == %{start: {1, 1}, end: {1, 10}}
    assert inner_meta[:range] == %{start: {1, 5}, end: {1, 10}}
  end

  test "dot identifiers and dot calls keep a single wrapper range" do
    assert {:ok, {{:., dot_meta, [_lhs, :bar]}, outer_meta, []}} = Spitfire.parse("foo.bar()")

    assert [dot_range] = Keyword.get_values(dot_meta, :range)
    assert [outer_range] = Keyword.get_values(outer_meta, :range)
    assert dot_range == %{start: {1, 1}, end: {1, 10}}
    assert outer_range == %{start: {1, 1}, end: {1, 10}}
  end

  test "dot calls used in access match stdlib after removing ranges" do
    for code <- ["foo.bar[0]", ~S(foo."bar"[:key])] do
      assert normalize(Spitfire.parse!(code)) == stdlib_parse!(code)
    end
  end

  test "dot call with do block keeps a single wrapper range" do
    code = ~S'''
    foo.() do
    :ok
    end
    '''

    assert {:ok, {{:., _dot_meta, [_lhs]}, outer_meta, [[do: :ok]]}} = Spitfire.parse(code)

    assert [outer_range] = Keyword.get_values(outer_meta, :range)
    assert outer_range == %{start: {1, 1}, end: {3, 4}}
  end

  test "dot identifier with do block spans the full receiver" do
    code = ~S'''
    foo.bar do
    :ok
    end
    '''

    assert {:ok, {{:., _dot_meta, [_lhs, :bar]}, outer_meta, [[do: :ok]]}} = Spitfire.parse(code)
    assert [outer_range] = Keyword.get_values(outer_meta, :range)
    assert outer_range == %{start: {1, 1}, end: {3, 4}}
    assert normalize(Spitfire.parse!(code)) == stdlib_parse!(code)
  end

  test "empty grouped expressions include the closing paren in range" do
    assert {:ok, {:__block__, meta, []}} = Spitfire.parse("()")

    assert meta[:range] == %{start: {1, 1}, end: {1, 3}}
  end

  test "anonymous functions include the closing end keyword in range" do
    assert {:ok, {:fn, meta, clauses}} = Spitfire.parse("fn :a -> 1; :b -> 2 end")

    assert meta[:range] == %{start: {1, 1}, end: {1, 24}}
    assert [_, _] = clauses
  end

  test "struct literal keeps a narrower range for the inner synthetic map" do
    assert {:ok, {:%, outer_meta, [{:__aliases__, _, [:MyStruct]}, {:%{}, inner_meta, []}]}} =
             Spitfire.parse("%MyStruct{}")

    assert outer_meta[:range] == %{start: {1, 1}, end: {1, 12}}
    assert inner_meta[:range] == %{start: {1, 10}, end: {1, 12}}
  end

  test "range keys in user data are preserved while metadata ranges are removed" do
    for code <- ["[range: 1]", "%{range: 1}", "[nested: %{range: 1}]", ~S|{[range: 1], "x"}|] do
      assert normalize(Spitfire.parse!(code)) == stdlib_parse!(code)
    end
  end

  describe "range corpus" do
    test "comments do not change expression ranges" do
      for {code, range} <- [
            {~S'''
             # Foo
             :bar
             ''', %{start: {2, 1}, end: {2, 5}}},
            {~S'''
             # Foo
             # Bar
             :baz
             ''', %{start: {3, 1}, end: {3, 5}}},
            {":baz # Foo", %{start: {1, 1}, end: {1, 5}}},
            {~S'''
             # Foo
             :baz # Bar
             ''', %{start: {2, 1}, end: {2, 5}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "literal roots use literal_encoder range metadata" do
      for {code, range} <- [
            {~S|1|, %{start: {1, 1}, end: {1, 2}}},
            {~S|100|, %{start: {1, 1}, end: {1, 4}}},
            {~S|1_000|, %{start: {1, 1}, end: {1, 6}}},
            {~S|1.0|, %{start: {1, 1}, end: {1, 4}}},
            {~S|1.00|, %{start: {1, 1}, end: {1, 5}}},
            {~S|1_000.0|, %{start: {1, 1}, end: {1, 8}}},
            {~S|"foo"|, %{start: {1, 1}, end: {1, 6}}},
            {~S'''
             "fo
             o"
             ''', %{start: {1, 1}, end: {2, 3}}},
            {~S|"key: \"value\""|, %{start: {1, 1}, end: {1, 17}}},
            {~S|'foo'|, %{start: {1, 1}, end: {1, 6}}},
            {~S'''
             'fo
             o'
             ''', %{start: {1, 1}, end: {2, 3}}},
            {~S|:foo|, %{start: {1, 1}, end: {1, 5}}},
            {~S|:"foo"|, %{start: {1, 1}, end: {1, 7}}},
            {~S|:'foo'|, %{start: {1, 1}, end: {1, 7}}},
            {~S|:"::"|, %{start: {1, 1}, end: {1, 6}}},
            {~S|foo|, %{start: {1, 1}, end: {1, 4}}},
            {~S|{1, 2, 3}|, %{start: {1, 1}, end: {1, 10}}},
            {~S|[1, 2, 3]|, %{start: {1, 1}, end: {1, 10}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "multiline literal roots use exact closing delimiter ranges" do
      for {code, range} <- [
            {~S'''
             """
             foo

             bar
             """
             ''', %{start: {1, 1}, end: {5, 4}}},
            {~S'''
               """
               foo
               bar
               """
             ''', %{start: {1, 3}, end: {4, 6}}},
            {~S"""
             '''
             foo

             bar
             '''
             """, %{start: {1, 1}, end: {5, 4}}},
            {~S"""
               '''
               foo
               bar
               '''
             """, %{start: {1, 3}, end: {4, 6}}},
            {~S'''
             :"foo

             bar"
             ''', %{start: {1, 1}, end: {3, 5}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "interpolated strings, charlists, atoms, and sigils span delimiters" do
      for {code, range} <- [
            {~S|"foo#{2}bar"|, %{start: {1, 1}, end: {1, 13}}},
            {~S'''
             "foo#{
               2
               }bar"
             ''', %{start: {1, 1}, end: {3, 8}}},
            {~S'''
             "foo#{
               2
               }
               bar"
             ''', %{start: {1, 1}, end: {4, 7}}},
            {~S'''
             "foo#{
               2
               }
               bar
             "
             ''', %{start: {1, 1}, end: {5, 2}}},
            {~S|'foo#{2}bar'|, %{start: {1, 1}, end: {1, 13}}},
            {~S'''
             'foo#{
               2
               }bar'
             ''', %{start: {1, 1}, end: {3, 8}}},
            {~S'''
             'foo#{
               2
               }
               bar'
             ''', %{start: {1, 1}, end: {4, 7}}},
            {~S'''
             'foo#{
               2
               }
               bar
             '
             ''', %{start: {1, 1}, end: {5, 2}}},
            {~S|:"foo#{2}bar"|, %{start: {1, 1}, end: {1, 14}}},
            {~S'''
             :"foo#{
               2
               }bar"
             ''', %{start: {1, 1}, end: {3, 8}}},
            {~S'''
             :"foo#{
               2
               }
             bar"
             ''', %{start: {1, 1}, end: {4, 5}}},
            {~S|~s[foo#{2}bar]|, %{start: {1, 1}, end: {1, 15}}},
            {~S|~s[foo#{2}bar]abc|, %{start: {1, 1}, end: {1, 18}}},
            {~S'''
             ~s"""
             foo#{10
              }
             bar
             """
             ''', %{start: {1, 1}, end: {5, 4}}},
            {~S'''
             ~s"""
             foo#{10
              }bar
             """abc
             ''', %{start: {1, 1}, end: {4, 7}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "interpolation child ranges cover interpolation delimiters" do
      code = ~S'''
      "foo#{
        2
        }bar"
      '''

      assert {:<<>>, _string_meta, ["foo", {:"::", interpolation_meta, _args}, "bar"]} =
               parse_with_literal_ranges(code)

      assert interpolation_meta[:range] == %{start: {1, 5}, end: {3, 4}}

      code = ~S'''
      'foo#{
        2
        }bar'
      '''

      assert {{:., _charlist_dot_meta, [List, :to_charlist]}, _charlist_meta,
              [["foo", {{:., _interpolation_dot_meta, [Kernel, :to_string]}, interpolation_meta, _args}, "bar"]]} =
               parse_with_literal_ranges(code)

      assert interpolation_meta[:range] == %{start: {1, 5}, end: {3, 4}}
    end

    test "containers and grouped blocks span their delimiters" do
      for {code, range} <- [
            {~S'''
             {
               1,
               2,
               3
             }
             ''', %{start: {1, 1}, end: {5, 2}}},
            {~S'''
             {1,
              2,
                3}
             ''', %{start: {1, 1}, end: {3, 6}}},
            {~S'''
             [
               1,
               2,
               3
             ]
             ''', %{start: {1, 1}, end: {5, 2}}},
            {~S'''
             [1,
              2,
                3]
             ''', %{start: {1, 1}, end: {3, 6}}},
            {~S|(1; 2; 3)|, %{start: {1, 1}, end: {1, 10}}},
            {~S'''
             (1;
               2;
               3)
             ''', %{start: {1, 1}, end: {3, 5}}},
            {~S'''
             (1;
               2;
               3
             )
             ''', %{start: {1, 1}, end: {4, 2}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "anonymous functions and stabs span their clauses" do
      for {code, range} <- [
            {~S|fn -> :ok end|, %{start: {1, 1}, end: {1, 14}}},
            {~S'''
             fn -> end
             ''', %{start: {1, 1}, end: {1, 10}}},
            {~S'''
             fn ->
             end
             ''', %{start: {1, 1}, end: {2, 4}}},
            {~S'''
             fn ->
               :ok
             end
             ''', %{start: {1, 1}, end: {3, 4}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end

      assert {:fn, _fn_meta, [{:->, stab_meta, _args}]} = parse_with_literal_ranges(~S|fn -> :ok end|)
      assert stab_meta[:range] == %{start: {1, 4}, end: {1, 10}}

      code = ~S'''
      fn ->
        :ok
      end
      '''

      assert {:fn, _fn_meta, [{:->, stab_meta, _args}]} = parse_with_literal_ranges(code)
      assert stab_meta[:range] == %{start: {1, 4}, end: {2, 6}}

      assert {:fn, _fn_meta, [{:->, stab_meta, _args}]} = parse_with_literal_ranges(~S|fn -> end|)
      assert stab_meta[:range] == %{start: {1, 4}, end: {1, 6}}

      code = ~S'''
      fn a ->
      end
      '''

      assert {:fn, _fn_meta, [{:->, stab_meta, _args}]} = parse_with_literal_ranges(code)
      assert stab_meta[:range] == %{start: {1, 4}, end: {1, 8}}
    end

    test "aliases, calls, and blocks span receivers and terminators" do
      for {code, range} <- [
            {~S|Foo|, %{start: {1, 1}, end: {1, 4}}},
            {~S|Foo.Bar|, %{start: {1, 1}, end: {1, 8}}},
            {~S'''
             Foo.
               Bar
             ''', %{start: {1, 1}, end: {2, 6}}},
            {~S|__MODULE__|, %{start: {1, 1}, end: {1, 11}}},
            {~S|__MODULE__.Bar|, %{start: {1, 1}, end: {1, 15}}},
            {~S|@foo.Bar|, %{start: {1, 1}, end: {1, 9}}},
            {~S|foo().Bar|, %{start: {1, 1}, end: {1, 10}}},
            {~S|foo.bar.().Baz|, %{start: {1, 1}, end: {1, 15}}},
            {~S|Foo.{Bar, Baz}|, %{start: {1, 1}, end: {1, 15}}},
            {~S'''
             Foo.{
               Bar,
               Bar,
               Qux
             }
             ''', %{start: {1, 1}, end: {5, 2}}},
            {~S'''
             Foo.{Bar,
              Baz,
                Qux}
             ''', %{start: {1, 1}, end: {3, 8}}},
            {~S|foo do :ok end|, %{start: {1, 1}, end: {1, 15}}},
            {~S'''
             foo do
               :ok
             end
             ''', %{start: {1, 1}, end: {3, 4}}},
            {~S|foo.bar|, %{start: {1, 1}, end: {1, 8}}},
            {~S|foo.bar()|, %{start: {1, 1}, end: {1, 10}}},
            {~S|foo.()|, %{start: {1, 1}, end: {1, 7}}},
            {~S|foo.bar.()|, %{start: {1, 1}, end: {1, 11}}},
            {~S'''
             foo.bar(
             )
             ''', %{start: {1, 1}, end: {2, 2}}},
            {~S|a.b.c|, %{start: {1, 1}, end: {1, 6}}},
            {~S|foo.bar(baz)|, %{start: {1, 1}, end: {1, 13}}},
            {~S|foo.bar.(baz)|, %{start: {1, 1}, end: {1, 14}}},
            {~S'''
             foo.bar.(
             baz)
             ''', %{start: {1, 1}, end: {2, 5}}},
            {~S|foo.bar("baz#{2}qux")|, %{start: {1, 1}, end: {1, 22}}},
            {~S|foo.bar("baz#{2}qux", [])|, %{start: {1, 1}, end: {1, 26}}},
            {~S|foo."b-a-r"|, %{start: {1, 1}, end: {1, 12}}},
            {~S|foo."b-a-r"()|, %{start: {1, 1}, end: {1, 14}}},
            {~S|foo."b-a-r"(1)|, %{start: {1, 1}, end: {1, 15}}},
            {~S|Mod.unquote(foo)(bar)|, %{start: {1, 1}, end: {1, 22}}},
            {~S|foo.bar baz|, %{start: {1, 1}, end: {1, 12}}},
            {~S|foo.bar baz, qux|, %{start: {1, 1}, end: {1, 17}}},
            {~S|foo."b-a-r" baz|, %{start: {1, 1}, end: {1, 16}}},
            {~S|foo(bar)|, %{start: {1, 1}, end: {1, 9}}},
            {~S'''
             foo(
               bar
               )
             ''', %{start: {1, 1}, end: {3, 4}}},
            {~S|foo bar|, %{start: {1, 1}, end: {1, 8}}},
            {~S|foo bar baz|, %{start: {1, 1}, end: {1, 12}}},
            {~S'''
             foo
               bar
             ''', %{start: {1, 1}, end: {2, 6}}},
            {~S|Foo.bar|, %{start: {1, 1}, end: {1, 8}}},
            {~S'''
             Foo.
               bar
             ''', %{start: {1, 1}, end: {2, 6}}},
            {~S|unquote(foo)()|, %{start: {1, 1}, end: {1, 15}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "operators, ranges, bitstrings, sigils, captures, and access span operands" do
      for {code, range} <- [
            {~S|!foo|, %{start: {1, 1}, end: {1, 5}}},
            {~S|!   foo|, %{start: {1, 1}, end: {1, 8}}},
            {~S|not  foo|, %{start: {1, 1}, end: {1, 9}}},
            {~S|@foo|, %{start: {1, 1}, end: {1, 5}}},
            {~S|@   foo|, %{start: {1, 1}, end: {1, 8}}},
            {~S|1 + 1|, %{start: {1, 1}, end: {1, 6}}},
            {~S|foo when bar|, %{start: {1, 1}, end: {1, 13}}},
            {~S'''
              5 +
                 10
             ''', %{start: {1, 2}, end: {2, 7}}},
            {~S"foo |> bar", %{start: {1, 1}, end: {1, 11}}},
            {~S'''
             foo
             |> bar
             ''', %{start: {1, 1}, end: {2, 7}}},
            {~S'''
             foo
             |>
             bar
             ''', %{start: {1, 1}, end: {3, 4}}},
            {~S|1..2|, %{start: {1, 1}, end: {1, 5}}},
            {~S|1..2//3|, %{start: {1, 1}, end: {1, 8}}},
            {~S|foo..bar//baz|, %{start: {1, 1}, end: {1, 14}}},
            {~S|<<1, 2, foo>>|, %{start: {1, 1}, end: {1, 14}}},
            {~S'''
             <<1, 2,

              foo>>
             ''', %{start: {1, 1}, end: {3, 7}}},
            {~S|~s[foo bar]|, %{start: {1, 1}, end: {1, 12}}},
            {~S'''
             ~s"""
             foo
             bar
             """
             ''', %{start: {1, 1}, end: {4, 4}}},
            {~S|&foo/1|, %{start: {1, 1}, end: {1, 7}}},
            {~S|&Foo.bar/1|, %{start: {1, 1}, end: {1, 11}}},
            {~S|&__MODULE__.Foo.bar/1|, %{start: {1, 1}, end: {1, 22}}},
            {~S|&foo(&1, :bar)|, %{start: {1, 1}, end: {1, 15}}},
            {~S|& &1.foo|, %{start: {1, 1}, end: {1, 9}}},
            {~S|& &1|, %{start: {1, 1}, end: {1, 5}}},
            {~S|foo[bar]|, %{start: {1, 1}, end: {1, 9}}}
          ] do
        assert {_token, meta, _args} = parse_with_literal_ranges(code)
        assert meta[:range] == range
      end
    end

    test "regression ranges" do
      code = ~S'''
      def rpc_call(pid, call = %Call{method: unquote(method_name)}),
        do: GenServer.unquote(genserver_method)(pid, call)
      '''

      assert {_token, meta, _args} = parse_with_literal_ranges(code)
      assert meta[:range] == %{start: {1, 1}, end: {2, 53}}

      code = ~S'''
      defmodule Foo do
        def foo(arg) when (arg.valid? == true) do
          arg
        end
      end
      '''

      assert {:defmodule, _module_meta,
              [
                _module,
                [
                  {{:__literal__, _do_meta, [:do]},
                   {:def, _def_meta, [{:when, _when_meta, [_call, {:==, meta, _args}]}, _body]}}
                ]
              ]} = parse_with_literal_ranges(code)

      assert meta[:range] == %{start: {2, 21}, end: {2, 41}}

      code = ~S'''
                          fn
        1 -> File.read!(arg1)
        arg1 -> File.read!(arg1)
      end
      '''

      assert {:fn, meta, _stabs} = parse_with_literal_ranges(code)
      assert meta[:range] == %{start: {1, 21}, end: {4, 4}}
    end
  end

  defp normalize(ast) do
    Spitfire.TestHelpers.strip_range_metadata(ast)
  end

  defp parse_with_literal_ranges(code) do
    Spitfire.parse!(code, literal_encoder: fn literal, meta -> {:ok, {:__literal__, meta, [literal]}} end)
  end

  defp stdlib_parse!(code) do
    Code.string_to_quoted!(code, columns: true, token_metadata: true, emit_warnings: false)
  end
end
