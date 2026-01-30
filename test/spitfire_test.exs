defmodule SpitfireTest do
  use ExUnit.Case, async: true

  doctest Spitfire

  describe "valid code" do
    test "semicolons" do
      code = "res = Foo.Bar.run(1, 2, 3); IO.inspect(res)"

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      res = Foo.Bar.run(1, 2, 3);
      IO.inspect(res)
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      fn one -> IO.inspect(one); one end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      def foo, do: IO.inspect("bob"); "bob"
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      foo do: IO.inspect("bob"); "bob"
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses valid elixir" do
      code = """
      defmodule Foo do
        use AnotherMod.Nested,
          some: :option

        def run(arg) do
          bar()
          :ok
        end
      end
      """

      assert Spitfire.parse(code) == s2q(code)
    end

    test "access syntax" do
      codes = [
        "foo[:bar]",
        "foo[:bar][:baz]",
        ~S'(meta[:end_of_expression] || meta)[:line]',
        "%{bar: :foo}[:bar]",
        "state.parent_meta[:line]",
        "@preferred_envs[task]",
        "!!meta[:diff]",
        "foo[1]",
        ~S'''
        foo[
          :bar
        ]
        ''',
        ~S'foo[bar["baz"]]'
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "token metadata" do
      code = ~S'''
      foo do
        1 + 1
      end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      foo do
        bar do
          1 + 1
        end
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "type syntax" do
      codes = [
        ~S'''
        @type foo :: String.t()
        ''',
        ~S'''
        @spec foo(one :: String.t(), number) :: :ok | :error
        ''',
        ~S'''
        @type diagnostic(severity) :: %{
          required(:source) => Path.t() | nil,
          required(:file) => Path.t() | nil,
          required(:severity) => severity,
          required(:message) => String.t(),
          required(:position) => position(),
          required(:stacktrace) => Exception.stacktrace(),
          required(:span) => {line :: pos_integer(), column :: pos_integer()} | nil,
          optional(:details) => term(),
          optional(any()) => any()
        }
        ''',
        ~S'''
        @typep versioned_vars :: %{optional(variable) => var_version :: non_neg_integer}
        ''',
        "@spec with(t(), (() -> x)) :: x when x: var"
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses unary operators" do
      code = ~S'''
      ^foo
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      ^
      foo
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses numbers" do
      code = """
      111_111
      """

      assert Spitfire.parse(code) == s2q(code)

      code = """
      1.4
      """

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses strings" do
      code = ~s'''
      "foobar"
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      """
      foobar
      """
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses charlists" do
      code = ~s'''
      'foobar'
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S"""
      '''
      foobar
      '''
      """

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      'foo#{alice}bar'
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      'foo#{
        alice
      }bar'
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S"""
      '''
      foo#{alice}bar
      '''
      """

      assert Spitfire.parse(code) == s2q(code)

      code = ~S"""
      '''
      foo#{
        alice
      }bar
      '''
      """

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses atoms" do
      code = ~s'''
      :foobar
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~s'''
      :","
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      :"foo#{}"
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      :"foo#{bar}"
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses left stab" do
      code = """
      apple <- apples
      """

      assert Spitfire.parse(code) == s2q(code)
    end

    # these tests do not test against Code.string_to_quoted because these code fragments parse as errors
    @tag :skip
    test "parses right stab" do
      # code = """
      # -> bar
      # """

      # assert Spitfire.parse!(code) == [
      #          {:->, [line: 1, column: 1],
      #           [[], {:bar, [{:end_of_expression, [newlines: 1, line: 1, column: 7]}, line: 1, column: 4], nil}]}
      #        ]

      # code = """
      # -> :ok
      # """

      # assert Spitfire.parse!(code) == [{:->, [line: 1, column: 1], [[], :ok]}]

      # code = """
      # foo -> bar
      # """

      # assert Spitfire.parse!(code) == [
      #          {:->, [line: 1, column: 5],
      #           [
      #             [{:foo, [line: 1, column: 1], nil}],
      #             {:bar, [{:end_of_expression, [newlines: 1, line: 1, column: 11]}, line: 1, column: 8], nil}
      #           ]}
      #        ]

      # code = """
      # foo, bar, baz -> bar
      # """

      # assert Spitfire.parse!(code) == [
      #          {:->, [line: 1, column: 15],
      #           [
      #             [
      #               {:foo, [line: 1, column: 1], nil},
      #               {:bar, [line: 1, column: 6], nil},
      #               {:baz, [line: 1, column: 11], nil}
      #             ],
      #             {:bar, [{:end_of_expression, [newlines: 1, line: 1, column: 21]}, line: 1, column: 18], nil}
      #           ]}
      #        ]

      # code = """
      # alice, bob, carol ->
      #   :error
      #   bar
      # """

      # # if we get a prefix comma operator, that means we might need to backtrack and then
      # # parse a comma list. if we hit the operator, it means that we are not actually in an
      # # existing comma list, like a list or a map
      # assert Spitfire.parse!(code) == [
      #          {:->, [newlines: 1, line: 1, column: 19],
      #           [
      #             [
      #               {:alice, [line: 1, column: 1], nil},
      #               {:bob, [line: 1, column: 8], nil},
      #               {:carol, [line: 1, column: 13], nil}
      #             ],
      #             {:__block__, [],
      #              [:error, {:bar, [{:end_of_expression, [newlines: 1, line: 3, column: 6]}, line: 3, column: 3], nil}]}
      #           ]}
      #        ]

      code = """
      foo ->
        :ok
        baz

      alice, bob, carol ->
        :error
        bar
      """

      assert Spitfire.parse!(code) ==
               [
                 {:->, [newlines: 1, line: 1, column: 5],
                  [
                    [{:foo, [line: 1, column: 1], nil}],
                    {:__block__, [],
                     [
                       :ok,
                       {:baz,
                        [
                          end_of_expression: [newlines: 2, line: 3, column: 6],
                          line: 3,
                          column: 3
                        ], nil}
                     ]}
                  ]},
                 {:->, [newlines: 1, line: 5, column: 19],
                  [
                    [
                      {:alice, [line: 5, column: 1], nil},
                      {:bob, [line: 5, column: 8], nil},
                      {:carol, [line: 5, column: 13], nil}
                    ],
                    {:__block__, [],
                     [
                       :error,
                       {:bar,
                        [
                          end_of_expression: [newlines: 1, line: 7, column: 6],
                          line: 7,
                          column: 3
                        ], nil}
                     ]}
                  ]}
               ]

      code = ~S'''
      ^foo ->
        :ok
      '''

      assert Spitfire.parse!(code) == [
               {:->, [newlines: 1, line: 1, column: 6],
                [[{:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}], :ok]}
             ]

      code = ~S'''
      @foo ->
        :ok
      '''

      assert Spitfire.parse!(code) == [
               {:->, [newlines: 1, line: 1, column: 6],
                [[{:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}], :ok]}
             ]
    end

    test "parses grouped expressions" do
      codes = [
        ~s'''
        1 + 2 + 3
        ''',
        ~s'''
        (1 + 2) + 3
        ''',
        ~s'''
        ((1 + 2) + 3)
        ''',
        ~s'''
        1 + (2 + 3)
        ''',
        "(!false)",
        ~S'''
        (
          !false
        )
        ''',
        "(not false)",
        ~S'''
        a do
          d ->
            (b -> c)
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses for comprehension" do
      codes = [
        ~s'''
        for i <- 0..100 do
          i + i
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses with expression" do
      codes = [
        ~s'''
        with {:ok, school} <- State.get_school(id),
             {:ok, teachers} <- School.list_teachers(school),
             {:ok, teacher} <- Teacher.coolest(teachers) do
          Email.send(teacher, "You are the coolest teacher")
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses variable identifiers" do
      code = ~s'''
      foobar
      alice
      bob
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses lists" do
      codes = [
        ~s'''
        []
        ''',
        ~s'''
        [arg]
        ''',
        ~s'''
        [one, :two, "three"]
        ''',
        ~s'''
         [
           one,
           :two,
           "three"
         ]
        ''',
        ~S'''
        case a do
          _ -> [d]
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses keyword lists" do
      codes = [
        ~s'''
        foo(one, two, alice: alice, bob: bob)
        ''',
        ~s'''
        foo alice: alice do
          :ok
        end
        ''',
        ~s'''
        [:one, two: :three]
        ''',
        ~s'''
        @moduledoc deprecated:
           "Use the new child specifications outlined in the Supervisor module instead"
        ''',
        ~S'["#{field}": value]',
        ~S'''
        ["#{field}":
          value]
        ''',
        ~S'''
        ["#{
          field}":
          value]
        ''',
        ~S'foo(a, "#{field}": value)'
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "another thing" do
      code = ~S'''
      case foo do
        :kw_identifier when is_list or is_map -> &parse_kw_identifier/1
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses pattern matching in list" do
      codes = [
        ~s'''
        [one | rest] = my_list
        ''',
        ~s'''
        [one, two | rest] = my_list
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses tuples" do
      codes = [
        ~s'''
        {}
        ''',
        ~s'''
        {one, :two}
        ''',
        ~s'''
        {
          one,
          :two,
          "three"
        }
        ''',
        ~s'''
        {one, :two, "three"}
        ''',
        ~s'''
        {
          one,
          :two,
          "three"
        }
        ''',
        ~s'''
        {
          one,
          :two,
          "three",
        }
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses aliases" do
      codes = [
        ~s'''
        Remote
        ''',
        ~s'''
        Remote.Foo
        ''',
        ~s'''
        Remote.Foo.Bar
        ''',
        "alias Mix.Phoenix.{Context, Schema}",
        ~S'''
        alias Mix.Phoenix.{
          Context,
          Schema
        }
        ''',
        "__MODULE__.{Config, Default}",
        ~S'''
        __MODULE__.{
          Config,
          Default
        }
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses maps" do
      codes = [
        ~s'''
        %{}
        ''',
        ~s'''
        %{
          foo: "bar",
          alice: "bob"
         }
        ''',
        ~s'''
        %{
          "foo" =>
            "bar",
          "alice" => "bob"
         }
        ''',
        ~s'''
        %{"foo" => "bar", 1 => 2, :three => :four, [] => [1], %{} => nil, bing => bong, foo: :bar}
        ''',
        ~s'''
        %{
          "foo" => "bar",
          1 => 2,
          :three => :four,
          [] => [1],
          %{} => nil,
          bing => bong,
          foo: :bar
        }
        ''',
        ~s'''
        %{
          foo: :bar,
          baz:
            beaux()
        }
        ''',
        ~S'''
        if state.trim do
          buffer = trim_left(buffer, 0)
          {rest, line, column} = trim_right(rest, line, column, 0, state)
          {rest, line, column, buffer}
        else
          {rest, line, column, buffer}
        end
        ''',
        ~S'''
        %{acc | key => value, foo => bar, ding: dong, bing: bong}
        ''',
        ~S'''
        %{
          a: a,
          b: b,
        }
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses structs" do
      codes = [
        ~s'''
        %Foo.Bar{}
        ''',
        ~s'%Foo.Bar{name: "alice", height: 73}',
        ~S'''
        %Foo.Bar{
          name: "alice",
          height: 73
        }
        ''',
        ~s'%Foo.Bar{name: name, properties: %Properties{key: key, value: get_value()}}',
        ~S'%__MODULE__{foo: bar}',
        ~S'%module{foo: bar}',
        ~S'%@foo{foo: bar}',
        ~S'%unquote(struct){}',
        ~S'''
        %UploadEntry{
          a: a,
          b: b,
        }
        ''',
        "%^resource{}",
        ~S'%__MODULE__.Foo{bar: "foo"}',
        ~S'%Bar.__MODULE__.Foo{bar: "foo"}',
        ~S'''
        %Bar.__MODULE__.Foo{
          bar: "foo"
        }
        ''',
        ~S'''
        %Foo{"ok": 1}
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses operators" do
      codes = [
        ~s'''
        1 + 2
        ''',
        ~s'''
        1 ** 2
        ''',
        ~s'''
        1 - 2
        ''',
        ~s'''
        1 - -2
        ''',
        ~s'''
        1 * 2
        ''',
        ~s'''
        1 / 2
        ''',
        ~s'''
        1 || foo()
        ''',
        ~s'''
        1 ||| foo()
        ''',
        ~s'''
        1 or foo()
        ''',
        ~s'''
        1 == foo()
        ''',
        ~s'''
        1 != foo()
        ''',
        ~s'''
        1 =~ foo()
        ''',
        ~s'''
        1 === foo()
        ''',
        ~s'''
        1 !== foo()
        ''',
        ~s'''
        1 < foo()
        ''',
        ~s'''
        1 > foo()
        ''',
        ~s'''
        1 <= foo()
        ''',
        ~s'''
        1 >= foo()
        ''',
        ~s'''
        1 |> foo()
        ''',
        ~s'''
        1 <|> foo()
        ''',
        ~s'''
        1 <<< foo()
        ''',
        ~s'''
        1 >>> foo()
        ''',
        ~s'''
        1 <<~ foo()
        ''',
        ~s'''
        1 ~>> foo()
        ''',
        ~s'''
        1 <~ foo()
        ''',
        ~s'''
        1 ~> foo()
        ''',
        ~s'''
        1 <~> foo()
        ''',
        ~s'''
        1 in foo()
        ''',
        ~s'''
        foo not in bar
        ''',
        ~s'''
        1 ^^^ foo()
        ''',
        ~s'''
        1 + 2 * 3 - 2
        ''',
        ~s'''
        one..two
        ''',
        ~s'''
        one..two//2
        ''',
        ~s'''
        one <> two
        ''',
        ~s'''
        one ++ two
        ''',
        ~s'''
        one -- two
        ''',
        ~s'''
        one +++ two
        ''',
        ~s'''
        one --- two
        ''',
        ~s'''
        one ++ two ++ three
        ''',
        ~s'''
        @foo
        ''',
        ~s'''
        !foo
        ''',
        ~s'''
        not foo
        ''',
        ~s'''
        ^foo
        ''',
        ~s'''
        ~~~foo
        ''',
        ~s'''
        -1 / a(b)
        ''',
        ~s'''
        def +value do
          :erlang.+(value)
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses setting module attr" do
      codes = [
        ~s'''
        @foo bar()
        ''',
        ~s'''
        @

        __cursor__()
        ''',
        ~s'''
        @foo %{
          foo: :bar
        }
        ''',
        "@unix_days :calendar.date_to_gregorian_days({1970, 1, 1})"
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parse do block" do
      codes = [
        ~s'''
        foo do
        end
        ''',
        ~s'''
        foo do
         "howdy"
         :partner
        end
        ''',
        ~s'''
        foo arg do
         "howdy"
         :partner
        end
        ''',
        ~s'''
        if arg do
         "howdy"
        else
         :partner
        end
        ''',
        ~S'''
        {%{},
           quote do
             Enum.into(unquote(metadata), unquote(escape_metadata(maybe_application)))
           end}
        ''',
        ~S'''
        type bar(
                foo do
                  a
                end
              )
        ''',
        ~S'''
        if selectable == false do " unselectable" else [] end
        ''',
        ~S'''
        if group_id do
          [~S( data-group-id="), group_id, ~S(")]
        else
          []
        end
        ''',
        ~S'''
        if group_id do [~S( data-group-id="), group_id, ~S(")] else [] end
        ''',
        ~S'''
        if true do
          :ok
        else
        end
        ''',
        ~S'''
        if true do
          :ok
        else
          :error
        rescue
        end
        ''',
        ~S'''
        if true do
          :ok
        else
        rescue
          :error
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "multi line grouped expressions" do
      code = ~S'''
      (
        min_line = line(meta)
        max_line = closing_line(meta)
        Enum.any?(comments, fn %{line: line} -> line > min_line and line < max_line end)
      )
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      (min_line = line(meta)
      max_line = closing_line(meta)
      Enum.any?(comments, fn %{line: line} -> line > min_line and line < max_line end))
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      (foo -> bar
       baz -> boo)
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "when syntax inside normal expression" do
      code = ~S'''
      match?(x when is_nil(x), x)
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "when syntax inside keyword list" do
      code = ~S'[a: b when c]'

      assert Spitfire.parse(code) == s2q(code)
    end

    test "in_match_op inside keyword list" do
      code = ~S'[a: b <- c]'

      assert Spitfire.parse(code) == s2q(code)
    end

    test "case expr" do
      codes = [
        ~s'''
        case foo do
          bar ->
            bar

        end
        ''',
        ~s'''
        case :foo do
          :foo ->
            case get(:foo) do
              :FOO ->
                :bar
              _ ->
                :error
            end

          _ ->
            :error
        end
        ''',
        ~s'''
        case infix do
          nil ->
            {left, parser}

          ^do_block when parser.nestings != [] ->
            {left, next_token(parser)}

          _ ->
            infix.(next_token(parser), left)
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parse ambiguous function calls" do
      codes = [
        ~s'''
        a b c, d
        ''',
        ~s'''
        a b c, d do
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses function calls" do
      codes = [
        ~s'''
        foo()
        ''',
        ~s'''
        foo(arg, arg2)
        ''',
        ~s'''
        foo(
          arg,
          arg2
        )
        ''',
        ~s'''
        foo arg, arg2
        ''',
        ~s'''
        Remote.foo
        ''',
        ~s'''
        Remote.foo()
        ''',
        ~s'''
        Remote.foo(arg, arg2)
        ''',
        ~s'''
        Remote.foo(
          arg,
          arg2
        )
        ''',
        ~s'''
        Remote.foo arg, arg2
        ''',
        ~s'''
        :erlang.foo
        ''',
        ~s'''
        :erlang.foo()
        ''',
        ~s'''
        :erlang.foo(arg, arg2)
        ''',
        ~s'''
        :erlang.foo arg, arg2
        ''',
        ~s'''
        somevar.foo
        ''',
        ~s'''
        somevar.foo()
        ''',
        ~s'''
        :elixir_tokenizer.tokenize(String.to_charlist(code), 1, [])
        ''',
        ~s'''
        somevar.foo(arg, arg2)
        ''',
        ~s'''
        somevar.foo arg, arg2
        ''',
        ~S'''
        defp unquote(:"#{name}_text")(), do: unquote(contents)
        ''',
        ~S'''
        unquote(name)(
          rest,
          <<
            acc::binary,
            unquote(name)(c1)::16,
            unquote(name)(c2)::16,
            unquote(name)(c3)::16,
            unquote(name)(c4)::16,
            unquote(name)(c5)::16,
            unquote(name)(c6)::16,
            unquote(name)(c7)::16,
            unquote(name)(c8)::16
          >>
        )
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses anon functions" do
      codes = [
        ~s'''
        fn -> :ok end
        ''',
        ~s'''
        fn () -> :ok end
        ''',
        ~s'''
        fn ->
          :ok
        end
        ''',
        ~s'''
        fn one ->
          one
        end
        ''',
        ~s'''
        fn
         one ->
          one
        end
        ''',
        ~s'''
        fn(one) ->
          one
        end
        ''',
        ~S'foo(fn a -> a end)'
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses nested stab expressions" do
      assert Spitfire.parse(~s'fn :a -> 1; :b -> 2 end') == s2q(~s'fn :a -> 1; :b -> 2 end')
      assert Spitfire.parse(~s'fn x -> fn y -> x + y end end') == s2q(~s'fn x -> fn y -> x + y end end')
      assert Spitfire.parse(~s'fn x, y -> x + y end') == s2q(~s'fn x, y -> x + y end')

      assert Spitfire.parse(~s'fn {:ok, x} -> x; {:error, _} -> nil end') ==
               s2q(~s'fn {:ok, x} -> x; {:error, _} -> nil end')

      assert Spitfire.parse(~s'fn a -> fn b -> fn c -> a + b + c end end end') ==
               s2q(~s'fn a -> fn b -> fn c -> a + b + c end end end')

      assert Spitfire.parse(~s'fn x -> Enum.map(x, fn y -> y * 2 end) end') ==
               s2q(~s'fn x -> Enum.map(x, fn y -> y * 2 end) end')
    end

    test "parses multi-line stab expressions" do
      code = ~S'''
      fn
        :a -> 1
        :b -> 2
        :c -> 3
      end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      fn
        x when is_integer(x) ->
          x * 2
        x when is_binary(x) ->
          String.length(x)
      end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      case x do
        {:ok, val} ->
          val
        {:error, reason} ->
          raise reason
        _ ->
          nil
      end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      cond do
        x > 0 ->
          :positive
        x < 0 ->
          :negative
        true ->
          :zero
      end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      receive do
        {:msg, data} ->
          handle(data)
        :stop ->
          exit(:normal)
      after
        1000 ->
          :timeout
      end
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      try do
        risky_operation()
      rescue
        e in RuntimeError ->
          handle_runtime(e)
        e in ArgumentError ->
          handle_argument(e)
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses match operator" do
      codes = [
        ~s'''
        foo = :bar
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses nil" do
      code = "nil"
      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses booleans" do
      code = "false"
      assert Spitfire.parse(code) == s2q(code)

      code = "true"
      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses right stab argument with parens" do
      code = "if true do (x, y) -> x end"

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parses cond expression" do
      codes = [
        ~s'''
        cond do
           prefix == nil ->
             :foo
           true ->
             :bar
         end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "|> operator" do
      code = ~S'''
      def parse(code) do
        parser = code |> new() |> next_token() |> next_token()

        parse_program(parser)
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "when operator" do
      codes = [
        ~s'''
        case x do
          foo when is_binary(foo) ->
            :ok
        end
        ''',
        ~s'''
        case x do
          foo when is_binary(foo) ->
            :ok

          bar when is_number(bar) ->
            :ok
        end
        ''',
        ~s'''
        def foo(bar) when is_binary(bar) do
          :ok
        end
        ''',
        ~s'''
        fn foo when is_binary(foo) ->
          :ok
        end
        ''',
        ~s'''
        fn foo, bar, _baz when is_binary(foo) and bar in [:alice, :bob] ->
          :ok
        end
        ''',
        ~S'''
        defp reduce(a, b)
             when a == 1
             when b == 2 do
          foo()
        end
        ''',
        ~S'''
        with {hour, ""} when hour < 24 <- Integer.parse(hour),
             {min, ""} when min < 60 <- Integer.parse(min) do
          {(hour * 60 + min) * 60 * sign, rest}
        else
          _ -> :error
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "capture operator" do
      codes = [
        ~s'''
        &foo/1
        ''',
        ~s'''
        &Foo.foo/1
        ''',
        ~s'''
        & &1
        ''',
        ~s'''
        &Foo.bar(one, &1)
        ''',
        "Enum.all?(chars, &(&1 in @all_spaces))"
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "anonymous function function calls" do
      codes = [
        ~s'''
        foo.()
        ''',
        ~s'''
        foo.(one, two)
        ''',
        ~s'''
        foo.(
          one,
          two
        )
        ''',
        ~s'''
        infix.(next_token(parser), left)
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "case with" do
      code = ~S'''
      Repo.transaction(fn ->
        case with {:ok, api_key} <-
                    api_key_changeset
                    |> Ecto.Changeset.put_assoc(:secrets, [api_key_secret_changeset])
                    |> Repo.insert(),
                  [api_key_secret] <- api_key.secrets,
                  {:ok, api_key} <-
                    api_key
                    |> ApiKey.changeset(%{current_api_key_secret_id: api_key_secret.id})
                    |> Repo.update(),
                  do: %{api_key | current_secret: api_key_secret} do
          {:error, error} -> Repo.rollback(error)
          а -> а
        end
      end)
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "big test" do
      code = ~S'''
      if prefix == nil do
        {row, col} = token_loc(parser.current_token)

        IO.puts(
          IO.ANSI.red() <>
            "#{row}:#{col}: unknown prefix: #{current_token_type(parser)}" <> IO.ANSI.reset()
        )

        {:error, next_token(parser)}
      else
        {left, parser} = prefix.(parser)

        calc_prec = fn parser ->
          {_associativity, power} = peek_precedence(parser)

          precedence =
            case associativity do
              :left -> precedence
              :unassoc -> 0
              :right -> precedence - 1
            end

          precedence < power
        end

        terminals = [:eol, :eof, :"}", :")", :"]"]

        terminals =
          if is_top do
            terminals
          else
            [:"," | terminals]
          end

        while peek_token(parser) not in terminals && calc_prec.(parser) <- {left, parser} do
          infix =
            case peek_token_type(parser) do
              :match_op -> &parse_infix_expression/2
              :when_op -> &parse_infix_expression/2
              :pipe_op -> &parse_infix_expression/2
              :dual_op -> &parse_infix_expression/2
              :mult_op -> &parse_infix_expression/2
              :concat_op -> &parse_infix_expression/2
              :assoc_op -> &parse_assoc_op/2
              :arrow_op -> &parse_infix_expression/2
              :ternary_op -> &parse_infix_expression/2
              :or_op -> &parse_infix_expression/2
              :and_op -> &parse_infix_expression/2
              :comp_op -> &parse_infix_expression/2
              :rel_op -> &parse_infix_expression/2
              :in_op -> &parse_infix_expression/2
              :xor_op -> &parse_infix_expression/2
              :in_match_op -> &parse_infix_expression/2
              :range_op -> &parse_range_expression/2
              :stab_op -> &parse_stab_expression/2
              :do -> &parse_do_block/2
              :dot_call_op -> &parse_dot_call_expression/2
              :. -> &parse_dot_expression/2
              :"," when is_top -> &parse_comma/2
              _ -> nil
            end

          do_block = &parse_do_block/2

          case infix do
            nil ->
              {left, parser}

            ^do_block when parser.nestings != [] ->
              {left, next_token(parser)}

            _ ->
              infix.(next_token(parser), left)
          end
        end
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "function def with case expression with anon function inside" do
      code = ~S'''
      def bar(foo) do
        case foo do
          :foo ->
            :ok
          :bar ->
            Enum.map(some_list, fn item ->
              item.name
            end)
        end
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "case expression with anon function inside" do
      code = ~S'''
      case foo do
        :foo ->
          :ok
        :bar ->
          Enum.map(some_list, fn item ->
            item.name
          end)
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "not really sure" do
      code = ~S'''
      defp parse_stab_expression(parser, lhs) do
        case current_token(parser) do
          :<- ->
            parse_infix_expression(parser, lhs)

          :-> ->
            token = current_token(parser)
            current_sd = parser.stab_depth
            parser = eat_at(parser, :eol, 1)
            exprs = []

            {exprs, parser} =
              while peek_token(parser) not in [:eof, :end] <- {exprs, parser} do
                parser = next_token(parser)
                {ast, parser} = parse_expression(parser, top: true)

                parser = eat_at(parser, :eol, 1)

                {[ast | exprs], eat_eol(parser)}
              end

            rhs =
              case exprs do
                [ast] -> ast
                exprs -> {:__block__, [], Enum.reverse(exprs)}
              end

            {rhs, stabs} =
              Macro.traverse(
                rhs,
                [],
                fn node, acc ->
                  case node do
                    {:->, meta, _args} ->
                      if meta[:depth] == current_sd do
                        {:__remove_me__, [node | acc]}
                      else
                        {node, acc}
                      end

                    _ ->
                      {node, acc}
                  end
                end,
                fn
                  {node, meta, args}, acc when is_list(args) ->
                    args = Enum.reject(args, &(is_list(&1) && Enum.member?(&1, :__remove_me__)))
                    {{node, meta, args}, acc}

                  node, acc ->
                    {node, acc}
                end
              )

            rhs =
              case rhs do
                {:__block__, _, [ast]} -> ast
                [ast] -> ast
                block -> block
              end

            ast =
              [{token, [depth: parser.stab_depth], [wrap(lhs), rhs]}] ++ Enum.reverse(stabs)

            {ast, eat_eol(parser)}
        end
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "lonely parens" do
      code = ~S'''
      ()
      (())
      foo do
        ()
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "multi line for clause" do
      code = ~S'''
      for {^function, arity} <- exports,
          (if docs do
             find_doc_with_content(docs, function, arity)
           else
             get_spec(module, function, arity) != []
           end) do
        h_mod_fun_arity(module, function, arity)
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "ambiguous op" do
      code = "@all_info -1"
      assert Spitfire.parse(code) == s2q(code)
    end

    test "range op" do
      code = ".."
      assert Spitfire.parse(code) == s2q(code)
    end

    test "range operator precedence and nested ranges" do
      codes = [
        "a..b ++ c",
        "a..b -- c",
        "a..b <> c",
        "a..b..c//d",
        "1..2..3//4",
        "a..b//c",
        "1..10//2"
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "from nx repo" do
      code = ~S'''
      def a,
        do: [
          b: :c,
          d:
            {"f",
             quote do
               x
             end, "g"}
        ]
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "from ecto repo" do
      code = ~S'''
      @switches [
        repo: [:string, :keep],
      ]
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "big with" do
      code = ~S'''
      with {:ok, _} <- bar(fn a ->
               with :d <- b do
                 :f
               end
             end) do
        :ok
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "bitstrings" do
      code = ~S'<<?., char, rest::binary>>'

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      <<
        ?.,
        char,
        rest::binary
      >>
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "anonymous function typespecs" do
      code = ~S'''
      @spec start_link((-> term), GenServer.options()) :: on_start
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      @spec get(agent, (state -> a), timeout) :: a when a: var
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "rescue with def" do
      code = ~S'''
      def foo(%mod{} = bar) do
        :ok
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "multiple block identifiers" do
      code = ~S'''
      try do
        foo()
      rescue
        e in E ->
          bar()
      else
        {:ok, value} -> value
        :error -> default
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "starts with a comment" do
      code = """
      # hi there
      some_code = :foo
      """

      assert Spitfire.parse(code) == s2q(code)
    end

    test "default args" do
      code = ~S'''
      def foo(arg \\ :value) do
        :ok
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "literal encoder" do
      codes = [
        ~S'''
        1
        "two"
        :three
        [four]
        try do
          :ok
        rescue
          _ ->
            :error
        end
        ''',
        ~S'1.752',
        ~S'0xABCD',
        ~S'0o01234567',
        ~S'0b10101010',
        ~S'?é',
        ~S'"foo"',
        ~S"'foo'",
        ~S':"foo"',
        ~S":foo",
        ~S'''
        """
        foo
        """
        ''',
        ~S"""
        '''
        foo
        '''
        """,
        ~S'{one, two}',
        ~S'''
        Foo.run(f: &{:ok, {:__literal__, &2, [&1]}})
        '''
      ]

      encoder = fn l, m -> {:ok, {:__literal__, m, [l]}} end

      for code <- codes do
        assert Spitfire.parse(code, literal_encoder: encoder) ==
                 Code.string_to_quoted(code,
                   literal_encoder: encoder,
                   columns: true,
                   token_metadata: true,
                   emit_warnings: false
                 )
      end
    end

    test "sigils" do
      codes = [
        ~S'~s"foo"',
        ~S'~s"foo"bar',
        ~S'~s"hello#{world}"bar',
        ~S'~S"hello#{world}"bar',
        ~S'~S|hello#{world}|bar',
        ~S'''
        ~s|hello#{
          world
        }|bar
        ''',
        ~S'''
        ~s"""
        hello#{
          world
        }
        """bar
        ''',
        ~S'''
          ~S"""
        hello world
          """
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses string interpolation" do
      code = ~S'''
      "foo#{alice}bar"
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      "foo#{
        alice
      }bar"
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      "foo#{}bar"
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      """
      foo#{alice}bar
      """
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      """
      foo#{
        alice
      }bar
      """
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      "#{foo}"
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "end of expression metadata" do
      codes = [
        ~S'''
        foo do
          Some.thing(
            bar
          )
          Some.thing_else!()
        end
        ''',
        ~S'''
        fn foo ->
          send foo, :hi

          :ok
        end
        ''',
        ~S'''
        Module.ParallelChecker.verify(fn ->
          {charlist, file} = find_file!(file, relative_to)
          :elixir_compiler.string(charlist, file, fn _, _ -> :ok end)
        end)
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "closing metadata" do
      codes = [
        ~S'{}',
        ~S'{one, two, three}',
        ~S'%{}',
        ~S'%{"one" => two, three: 4}',
        ~S'foo()',
        ~S'foo(bar)'
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "parses special keywords" do
      codes = [
        "__MODULE__",
        "__MODULE__.foo()",
        "__MODULE__.Foo",
        "Foo.__MODULE__.Bar"
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "from nextls test" do
      code = ~S'''
      defmodule Foo do
        defstruct [:foo, bar: "yo"]

        defmodule State do
          defstruct [:yo]

          def new(attrs) do
            struct(%__MODULE__{}, attrs)
          end
        end

        @spec run(any(), any(), any()) :: :something
        def run(foo, bar, baz) do
          :something
        end
      end
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "unquote_splicing" do
      codes = [
        "sum(1, unquote_splicing(values), 5)",
        "quote(do: (unquote_splicing(calls)))",
        ~S'''
        quote do
          unquote_splicing(calls)
        end
        ''',
        ~S'''
        fn ->
          unquote_splicing(calls)
        end
        ''',
        ~S'''
        fn calls ->
          unquote_splicing(calls)
        end
        '''
      ]

      for code <- codes do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "line and column opt" do
      code = "foo"

      assert Spitfire.parse(code, line: 12, column: 7) == s2q(code, line: 12, column: 7)
    end

    test "ellipsis_op ..." do
      code = ~S'''
      @callback a([B.spec(), ...], C.t(), D.t()) :: [
          E.spec(),
          ...
        ]
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      ... + 1 * 2
      '''

      assert Spitfire.parse(code) == s2q(code)

      code = ~S'''
      @type fun :: (... -> any())
      '''

      assert Spitfire.parse(code) == s2q(code)
    end

    test "blocks inside an anon function as a parameter" do
      code = ~S"""
      M.m fn ->
        what a do
          :alice
        else
          :bob
        end
      end
      """

      assert Spitfire.parse(code) == s2q(code)
    end

    test "parens on a macro with a do block on the right side of a match operator" do
      for code <- [
            ~S"""
            a =
              b() do
                :ok
              end
            """,
            ~S"""
            a =
              b(foo) do
                :ok
              end
            """
          ] do
        # code |> Spitfire.parse!() |> Macro.to_string() |> IO.puts()
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "emoji as identifiers" do
      for code <- [
            ~S|:"▶️"|,
            ~S|:'➡️'|,
            ~S|["➡️": x]|,
            ~S|['➡️': x]|,
            ~S|foo.'➡️'|
          ] do
        assert Spitfire.parse(code) == s2q(code)
      end
    end

    test "quoted dot call identifier" do
      for code <- [
            ~S"""
            :erlang."=<"(left, right)
            """,
            ~S"""
            :erlang.'=<'(left, right)
            """
          ] do
        assert Spitfire.parse(code) == s2q(code)
      end
    end
  end

  describe "code with errors" do
    # TODO: this needs a change to the tokenizer i believe, or a way to splice out the unknown token
    @tag :skip
    test "unknown prefix operator" do
      code = "foo $bar, baz"

      assert Spitfire.parse(code) ==
               {:error, {:foo, [line: 1, column: 1], [{:__block__, [error: true, line: 1, column: 5], []}]},
                [{[line: 1, column: 5], "unknown token: %"}]}
    end

    test "missing bitstring brackets" do
      code = """
      <<one::
      :ok
      """

      assert Spitfire.parse(code) ==
               {:error,
                {:<<>>,
                 [
                   {:end_of_expression, [newlines: 1, line: 2, column: 4]},
                   {:closing, []},
                   {:line, 1},
                   {:column, 1}
                 ],
                 [
                   {:"::", [newlines: 1, line: 1, column: 6], [{:one, [line: 1, column: 3], nil}, :ok]}
                 ]}, [{[line: 1, column: 1], "missing closing brackets for bitstring"}]}
    end

    test "missing closing parentheses" do
      code = "1 * (2 + 3"

      assert Spitfire.parse(code) ==
               {
                 :error,
                 {{:*, [line: 1, column: 3], [1, {:__block__, [error: true, line: 1, column: 3], []}]},
                  [{:closing, [line: 1, column: 10]}, {:line, 1}, {:column, 3}], [{:+, [line: 1, column: 8], [2, 3]}]},
                 [
                   {[line: 1, column: 3], "malformed right-hand side of * operator"},
                   {[line: 1, column: 3], "missing closing parentheses for function invocation"}
                 ]
               }
    end

    test "missing closing list bracket" do
      code = "([1, 2 ++ [4])"

      assert Spitfire.parse(code) ==
               {:error, [1, {:++, [line: 1, column: 8], [2, [4]]}],
                [{[line: 1, column: 2], "missing closing bracket for list"}]}

      code = """
      [1
      :ok
      """

      assert Spitfire.parse(code) ==
               {:error, {:__block__, [], [[1], :ok]}, [{[line: 1, column: 1], "missing closing bracket for list"}]}

      code = """
      [1, 2, 3,,
      """

      assert Spitfire.parse(code) ==
               {:error, [1, 2, 3], [{[line: 1, column: 1], "missing closing bracket for list"}]}
    end

    test "missing closing tuple brace" do
      code = "({1, 2 ++ [4])"

      assert Spitfire.parse(code) ==
               {:error, {1, {:++, [line: 1, column: 8], [2, [4]]}},
                [{[line: 1, column: 2], "missing closing brace for tuple"}]}

      code = """
      {1
      :ok
      """

      assert Spitfire.parse(code) ==
               {:error,
                {:__block__, [],
                 [
                   {:{},
                    [
                      end_of_expression: [newlines: 1, line: 1, column: 3],
                      closing: [],
                      line: 1,
                      column: 1
                    ], [1]},
                   :ok
                 ]}, [{[line: 1, column: 1], "missing closing brace for tuple"}]}
    end

    test "missing closing map brace" do
      code = ~S'foo(%{alice: "bob")'

      assert Spitfire.parse(code) ==
               {:error,
                {:foo, [{:closing, [line: 1, column: 19]}, line: 1, column: 1],
                 [{:%{}, [{:closing, [line: 1, column: 14]}, line: 1, column: 5], [alice: "bob"]}]},
                [{[line: 1, column: 14], "missing closing brace for map"}]}
    end

    test "missing comma in list" do
      code = ~S'[:foo :bar, :baz]'

      assert Spitfire.parse(code) ==
               {:error, [:foo, :baz], [{[line: 1, column: 7], "syntax error"}]}
    end

    test "missing comma in map" do
      code = ~S'%{foo: :bar baz: :boo}'

      assert Spitfire.parse(code) ==
               {:error, {:%{}, [{:closing, [line: 1, column: 22]}, line: 1, column: 1], [foo: :bar]},
                [
                  {[line: 1, column: 13], "syntax error"},
                  {[line: 1, column: 18], "syntax error"}
                ]}
    end

    test "missing comma in tuple" do
      code = ~S'{:foo :bar, :baz}'

      assert Spitfire.parse(code) ==
               {:error, {:foo, :baz}, [{[line: 1, column: 7], "syntax error"}]}
    end

    test "missing end in block" do
      code = ~S'''
      foo do
        Some.thing()
        :ok
      '''

      assert Spitfire.parse(code) == {
               :error,
               {
                 :foo,
                 [
                   end_of_expression: [newlines: 1, line: 3, column: 6],
                   do: [line: 1, column: 5],
                   end: [line: 1, column: 5],
                   line: 1,
                   column: 1
                 ],
                 [
                   [
                     do: {
                       :__block__,
                       [],
                       [
                         {
                           {
                             :.,
                             [line: 2, column: 7],
                             [
                               {:__aliases__, [{:last, [line: 2, column: 3]}, {:line, 2}, {:column, 3}], [:Some]},
                               :thing
                             ]
                           },
                           [
                             {:end_of_expression, [newlines: 1, line: 2, column: 15]},
                             {:closing, [line: 2, column: 14]},
                             {:line, 2},
                             {:column, 8}
                           ],
                           []
                         },
                         :ok
                       ]
                     }
                   ]
                 ]
               },
               [{[line: 1, column: 5], "missing `end` for do block"}]
             }
    end

    test "nested missing end in block" do
      code = ~S'''
      bar do
        foo do
          Some.thing()
          :ok
      end
      '''

      assert Spitfire.parse(code) == {
               :error,
               {
                 :bar,
                 [
                   end_of_expression: [newlines: 1, line: 5, column: 4],
                   do: [line: 1, column: 5],
                   end: [line: 1, column: 5],
                   line: 1,
                   column: 1
                 ],
                 [
                   [
                     do: {
                       :foo,
                       [
                         {:end_of_expression, [newlines: 1, line: 5, column: 4]},
                         {:do, [line: 2, column: 7]},
                         {:end, [line: 5, column: 1]},
                         {:line, 2},
                         {:column, 3}
                       ],
                       [
                         [
                           do:
                             {:__block__, [],
                              [
                                {{:., [line: 3, column: 9],
                                  [
                                    {:__aliases__, [last: [line: 3, column: 5], line: 3, column: 5], [:Some]},
                                    :thing
                                  ]},
                                 [
                                   end_of_expression: [newlines: 1, line: 3, column: 17],
                                   closing: [line: 3, column: 16],
                                   line: 3,
                                   column: 10
                                 ], []},
                                :ok
                              ]}
                         ]
                       ]
                     }
                   ]
                 ]
               },
               [{[line: 1, column: 5], "missing `end` for do block"}]
             }
    end

    test "malformed expression inside parens" do
      code = ~S'''
      foo(1 + )

      bar(two)
      '''

      assert Spitfire.parse(code) == {
               :error,
               {
                 :__block__,
                 [],
                 [
                   {:foo,
                    [
                      end_of_expression: [newlines: 2, line: 1, column: 10],
                      closing: [line: 1, column: 9],
                      line: 1,
                      column: 1
                    ],
                    [
                      {:+, [line: 1, column: 7], [1, {:__block__, [error: true, line: 1, column: 7], []}]}
                    ]},
                   {:bar,
                    [
                      {:end_of_expression, [newlines: 1, line: 3, column: 9]},
                      {:closing, [line: 3, column: 8]},
                      {:line, 3},
                      {:column, 1}
                    ], [{:two, [line: 3, column: 5], nil}]}
                 ]
               },
               [{[line: 1, column: 7], "malformed right-hand side of + operator"}]
             }
    end

    test "missing end parentheses in function call" do
      code = ~S'''
      foo(1 +

      bar(two)
      '''

      assert Spitfire.parse(code) == {
               :error,
               {:foo, [{:line, 1}, {:column, 1}],
                [
                  {:+, [newlines: 2, line: 1, column: 7],
                   [
                     1,
                     {:bar, [{:closing, [line: 3, column: 8]}, line: 3, column: 1], [{:two, [line: 3, column: 5], nil}]}
                   ]}
                ]},
               [{[line: 1, column: 4], "missing closing parentheses for function invocation"}]
             }
    end

    test "missing closing end to anon function and paren" do
      code = ~S'''
      new_list =
        Enum.map(some_list, fn item ->


      send(pid, new_list)
      '''

      assert Spitfire.parse(code) == {
               :error,
               {
                 :=,
                 [newlines: 1, line: 1, column: 10],
                 [
                   {:new_list, [line: 1, column: 1], nil},
                   {
                     {:., [line: 2, column: 7],
                      [
                        {:__aliases__, [last: [line: 2, column: 3], line: 2, column: 3], [:Enum]},
                        :map
                      ]},
                     [line: 2, column: 8],
                     [
                       {:some_list, [line: 2, column: 12], nil},
                       {
                         :fn,
                         [line: 2, column: 23],
                         [
                           {
                             :->,
                             [newlines: 3, line: 2, column: 31],
                             [
                               [{:item, [line: 2, column: 26], nil}],
                               {:send,
                                [
                                  {:end_of_expression, [newlines: 1, line: 5, column: 20]},
                                  {:closing, [line: 5, column: 19]},
                                  {:line, 5},
                                  {:column, 1}
                                ],
                                [
                                  {:pid, [line: 5, column: 6], nil},
                                  {:new_list, [line: 5, column: 11], nil}
                                ]}
                             ]
                           }
                         ]
                       }
                     ]
                   }
                 ]
               },
               [
                 {[line: 2, column: 23], "missing closing end for anonymous function"},
                 {[line: 2, column: 11], "missing closing parentheses for function invocation"}
               ]
             }
    end

    test "example from github issue" do
      code = ~S'''
      defmodule Foo do
        import Baz

        def bat do
          var = 123
          {
        end

        def local_function do
          # ...
        end
      end
      '''

      assert {:error, _ast, _} = result = Spitfire.parse(code)

      assert result ==
               {
                 :error,
                 {
                   :defmodule,
                   [
                     {:end_of_expression, [newlines: 1, line: 12, column: 4]},
                     {:do, [line: 1, column: 15]},
                     {:end, [line: 12, column: 1]},
                     {:line, 1},
                     {:column, 1}
                   ],
                   [
                     {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:Foo]},
                     [
                       do: {
                         :__block__,
                         [],
                         [
                           {:import,
                            [
                              end_of_expression: [newlines: 2, line: 2, column: 13],
                              line: 2,
                              column: 3
                            ],
                            [
                              {:__aliases__, [last: [line: 2, column: 10], line: 2, column: 10], [:Baz]}
                            ]},
                           {
                             :def,
                             [
                               end_of_expression: [newlines: 2, line: 7, column: 6],
                               do: [line: 4, column: 11],
                               end: [line: 7, column: 3],
                               line: 4,
                               column: 3
                             ],
                             [
                               {:bat, [line: 4, column: 7], nil},
                               [
                                 do:
                                   {:__block__, [],
                                    [
                                      {:=,
                                       [
                                         end_of_expression: [newlines: 1, line: 5, column: 14],
                                         line: 5,
                                         column: 9
                                       ], [{:var, [line: 5, column: 5], nil}, 123]},
                                      {:{},
                                       [
                                         {:end_of_expression, [newlines: 1, line: 6, column: 6]},
                                         {:line, 6},
                                         {:column, 5}
                                       ], []}
                                    ]}
                               ]
                             ]
                           },
                           {:def,
                            [
                              {:end_of_expression, [newlines: 1, line: 11, column: 6]},
                              {:do, [line: 9, column: 22]},
                              {:end, [line: 11, column: 3]},
                              {:line, 9},
                              {:column, 3}
                            ],
                            [
                              {:local_function, [line: 9, column: 7], nil},
                              [do: {:__block__, [], []}]
                            ]}
                         ]
                       }
                     ]
                   ]
                 },
                 [{[line: 6, column: 5], "missing closing brace for tuple"}]
               }
    end

    test "example from github issue with tuple elements" do
      code = ~S'''
      defmodule Foo do
        import Baz

        def bat do
          var = 123
          {var,
        end

        def local_function do
          # ...
        end
      end
      '''

      assert {:error, _ast, _} = result = Spitfire.parse(code)

      assert result ==
               {
                 :error,
                 {
                   :defmodule,
                   [
                     {:end_of_expression, [newlines: 1, line: 12, column: 4]},
                     {:do, [line: 1, column: 15]},
                     {:end, [line: 12, column: 1]},
                     {:line, 1},
                     {:column, 1}
                   ],
                   [
                     {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:Foo]},
                     [
                       do: {
                         :__block__,
                         [],
                         [
                           {:import,
                            [
                              end_of_expression: [newlines: 2, line: 2, column: 13],
                              line: 2,
                              column: 3
                            ],
                            [
                              {:__aliases__, [last: [line: 2, column: 10], line: 2, column: 10], [:Baz]}
                            ]},
                           {
                             :def,
                             [
                               end_of_expression: [newlines: 2, line: 7, column: 6],
                               do: [line: 4, column: 11],
                               end: [line: 7, column: 3],
                               line: 4,
                               column: 3
                             ],
                             [
                               {:bat, [line: 4, column: 7], nil},
                               [
                                 do:
                                   {:__block__, [],
                                    [
                                      {:=,
                                       [
                                         end_of_expression: [newlines: 1, line: 5, column: 14],
                                         line: 5,
                                         column: 9
                                       ], [{:var, [line: 5, column: 5], nil}, 123]},
                                      {:{}, [closing: [], line: 6, column: 5], [{:var, [line: 6, column: 6], nil}]}
                                    ]}
                               ]
                             ]
                           },
                           {:def,
                            [
                              {:end_of_expression, [newlines: 1, line: 11, column: 6]},
                              {:do, [line: 9, column: 22]},
                              {:end, [line: 11, column: 3]},
                              {:line, 9},
                              {:column, 3}
                            ],
                            [
                              {:local_function, [line: 9, column: 7], nil},
                              [do: {:__block__, [], []}]
                            ]}
                         ]
                       }
                     ]
                   ]
                 },
                 [{[line: 6, column: 5], "missing closing brace for tuple"}]
               }
    end

    test "heex templates" do
      code = ~S'''
      <%= form_for @changeset, @action, fn f -> %>
        <%= if @changeset.action do %>
          <div class="alert alert-danger">
            <p>Oops, something went wrong! Please check the errors below.</p>
          </div>
        <% end %>

        <div class="form-group">
          <label for="name_input" class="tooltip-label">
            <span>Organization Name</span>
            <span class="tooltip-info"></span>
            <span class="tooltip-text">Must be one word</span>
          </label>
          <%= text_input(f, :name, class: "form-control", id: "name_input") %>
          <div class="has-error">
            <%= error_tag(f, :name) %>
          </div>
        </div>

        <div class="button-submit-wrapper">
          <%= submit("Create Organization", class: "btn btn-primary") %>
        </div>
      <% end %>
      '''

      assert {:error, _ast, _errors} = Spitfire.parse(code)
    end

    test "doesn't drop the cursor node" do
      code =
        ~S'''
        %{state |
          foo: s
        __cursor__()
        ,
          bar: Foo.Bar.load(state.foo, state.baz)}
        '''

      assert Spitfire.parse(code) ==
               {:error,
                {:__block__, [],
                 [
                   {:%{}, [closing: [line: 2, column: 8], line: 1, column: 1],
                    [
                      {:|, [newlines: 1, line: 1, column: 9],
                       [
                         {:state, [line: 1, column: 3], nil},
                         [foo: {:s, [line: 2, column: 8], nil}]
                       ]}
                    ]},
                   {:s, [end_of_expression: [newlines: 1, line: 3, column: 13], line: 2, column: 8],
                    [{:__cursor__, [closing: [line: 3, column: 12], line: 3, column: 1], []}]},
                   {:__block__, [error: true, line: 4, column: 1], []},
                   {{:., [line: 5, column: 15],
                     [
                       {:__aliases__, [last: [line: 5, column: 12], line: 5, column: 8], [:Foo, :Bar]},
                       :load
                     ]}, [closing: [line: 5, column: 41], line: 5, column: 16],
                    [
                      {{:., [line: 5, column: 26], [{:state, [line: 5, column: 21], nil}, :foo]},
                       [no_parens: true, line: 5, column: 27], []},
                      {{:., [line: 5, column: 37], [{:state, [line: 5, column: 32], nil}, :baz]},
                       [no_parens: true, line: 5, column: 38], []}
                    ]},
                   {:__block__,
                    [
                      end_of_expression: [newlines: 1, line: 5, column: 43],
                      error: true,
                      line: 5,
                      column: 42
                    ], []}
                 ]},
                [
                  {[line: 2, column: 8], "missing closing brace for map"},
                  {[line: 4, column: 1], "unknown token: ,"},
                  {[line: 5, column: 42], "unknown token: }"}
                ]}
    end

    test "example from github issue with list elements" do
      code = ~S'''
      defmodule Foo do
        import Baz

        def bat do
          var = 123
          [var,
        end

        def local_function do
          # ...
        end
      end
      '''

      assert {:error, _ast, _} = result = Spitfire.parse(code)

      assert result == {
               :error,
               {
                 :defmodule,
                 [
                   {:end_of_expression, [newlines: 1, line: 12, column: 4]},
                   {:do, [line: 1, column: 15]},
                   {:end, [line: 12, column: 1]},
                   {:line, 1},
                   {:column, 1}
                 ],
                 [
                   {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:Foo]},
                   [
                     do: {
                       :__block__,
                       [],
                       [
                         {:import,
                          [
                            end_of_expression: [newlines: 2, line: 2, column: 13],
                            line: 2,
                            column: 3
                          ],
                          [
                            {:__aliases__, [last: [line: 2, column: 10], line: 2, column: 10], [:Baz]}
                          ]},
                         {
                           :def,
                           [
                             end_of_expression: [newlines: 2, line: 7, column: 6],
                             do: [line: 4, column: 11],
                             end: [line: 7, column: 3],
                             line: 4,
                             column: 3
                           ],
                           [
                             {:bat, [line: 4, column: 7], nil},
                             [
                               do:
                                 {:__block__, [],
                                  [
                                    {:=,
                                     [
                                       end_of_expression: [newlines: 1, line: 5, column: 14],
                                       line: 5,
                                       column: 9
                                     ], [{:var, [line: 5, column: 5], nil}, 123]},
                                    [{:var, [line: 6, column: 6], nil}]
                                  ]}
                             ]
                           ]
                         },
                         {:def,
                          [
                            {:end_of_expression, [newlines: 1, line: 11, column: 6]},
                            {:do, [line: 9, column: 22]},
                            {:end, [line: 11, column: 3]},
                            {:line, 9},
                            {:column, 3}
                          ],
                          [
                            {:local_function, [line: 9, column: 7], nil},
                            [do: {:__block__, [], []}]
                          ]}
                       ]
                     }
                   ]
                 ]
               },
               [{[line: 6, column: 5], "missing closing bracket for list"}]
             }
    end

    test "unclosed interpolation" do
      code = """
      defmodule MyModule do
        import List
        var = '\#{
      end
      """

      assert {:error, _ast, _} = result = Spitfire.parse(code)

      assert result == {
               :error,
               {:defmodule, [do: [line: 1, column: 20], end: [line: 1, column: 20], line: 1, column: 1],
                [
                  {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:MyModule]},
                  [
                    do:
                      {:__block__, [],
                       [
                         {:import,
                          [
                            end_of_expression: [newlines: 1, line: 2, column: 14],
                            line: 2,
                            column: 3
                          ],
                          [
                            {:__aliases__, [last: [line: 2, column: 10], line: 2, column: 10], [:List]}
                          ]},
                         {:=, [line: 3, column: 7],
                          [
                            {:var, [line: 3, column: 3], nil},
                            {:__block__, [error: true, line: 3, column: 7], []}
                          ]}
                       ]}
                  ]
                ]},
               [
                 {[line: 3, column: 7], "malformed right-hand side of = operator"},
                 {[line: 1, column: 20], "missing `end` for do block"}
               ]
             }
    end

    test "orphan comma" do
      code = ~S'''
      foo, do: IO.inspect("bob"); "bob"
      '''

      assert {:error, _ast, errors} = Spitfire.parse(code)
      assert Enum.any?(errors, fn {_, msg} -> String.contains?(msg, ",") end)
    end

    test "semicolon in list/tuple/map" do
      assert {:error, _, errors} = Spitfire.parse("[;]")
      assert Enum.any?(errors, fn {_, msg} -> msg == "unexpected token: ;" end)

      assert {:error, _, errors} = Spitfire.parse("{;}")
      assert Enum.any?(errors, fn {_, msg} -> msg == "unexpected token: ;" end)

      assert {:error, _, errors} = Spitfire.parse("%{;}")
      assert Enum.any?(errors, fn {_, msg} -> msg == "unexpected token: ;" end)
    end

    test "semicolon in parentheses is valid empty block" do
      assert {:ok, {:__block__, _, []}} = Spitfire.parse("(;)")
    end

    test "leading semicolon is skipped" do
      assert {:ok, {:foo, _, nil}} = Spitfire.parse("; foo")
    end

    test "semicolon as statement separator is valid" do
      assert {:ok, _} = Spitfire.parse("foo\n; bar")
      assert {:ok, _} = Spitfire.parse("foo\n\n; bar")

      assert {:ok, 1} = Spitfire.parse("(1;)")
      assert {:ok, 1} = Spitfire.parse("(1;\n)")
      assert {:ok, 1} = Spitfire.parse("(1\n;)")

      code = ~S"""
      defmodule MyModule do
        foo
        ; bar
      end
      """

      assert {:ok, _} = Spitfire.parse(code)
    end

    test "unexpected expression after keyword list" do
      assert {:error, _ast, errors} = Spitfire.parse(~S|foo(a: 1, b)|)

      assert Enum.any?(errors, fn {_, msg} ->
               String.contains?(msg, "unexpected expression after keyword list")
             end)

      assert {:error, _ast, errors} = Spitfire.parse(~S|foo(a: 1, :bar)|)

      assert Enum.any?(errors, fn {_, msg} ->
               String.contains?(msg, "unexpected expression after keyword list")
             end)

      assert {:error, _ast, errors} = Spitfire.parse(~S|@tag foo: bar, baz|)

      assert Enum.any?(errors, fn {_, msg} ->
               String.contains?(msg, "unexpected expression after keyword list")
             end)
    end

    test "__cursor__ after keyword list does not crash" do
      code = ~S|foo(a: 1, __cursor__())|
      assert {:ok, {:foo, _, [[{:a, 1}, {:__cursor__, _, []}]]}} = Spitfire.parse(code)
    end

    test "weird characters" do
      code = """
      [«]
      """

      assert {:error, _ast, [{[line: 1, column: 1], "missing closing bracket for list"}]} = Spitfire.parse(code)

      code = """
      {«}
      """

      assert {:error, _ast, [{[line: 1, column: 1], "missing closing brace for tuple"}]} = Spitfire.parse(code)

      code = """
      %{«}
      """

      assert {:error, _ast, [{[line: 1, column: 1], "missing closing brace for map"}]} = Spitfire.parse(code)

      code = """
      («)
      """

      assert {:error, _ast, [{[line: 1, column: 1], "missing closing parentheses"}]} = Spitfire.parse(code)

      code = """
      defp foo(«) do
        :ok
      end
      """

      assert {:error, _ast, [{[line: 1, column: 9], "missing closing parentheses"}]} = Spitfire.parse(code)

      code = """
      foo(«)
      """

      assert {:error, _ast, [{[line: 1, column: 4], "missing closing parentheses"}]} = Spitfire.parse(code)
    end

    test "missing braces for struct" do
      assert {:error,
              {:=, [line: 1, column: 6],
               [
                 {:%, [line: 1, column: 1],
                  [
                    {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                    {:%{}, [], []}
                  ]},
                 {:x, [line: 1, column: 8], nil}
               ]},
              [{[line: 1, column: 2], "missing opening brace for struct %Foo"}]} ==
               Spitfire.parse("%Foo = x")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                 {:%{}, [], []}
               ]},
              [{[line: 1, column: 2], "missing opening brace for struct %Foo"}]} ==
               Spitfire.parse("%Foo")

      assert {:error,
              {:=, [line: 1, column: 10],
               [
                 {:%, [line: 1, column: 1],
                  [
                    {:__aliases__, [last: [line: 1, column: 6], line: 1, column: 2], [:Foo, :Bar]},
                    {:%{}, [], []}
                  ]},
                 {:x, [line: 1, column: 12], nil}
               ]},
              [{[line: 1, column: 6], "missing opening brace for struct %Foo.Bar"}]} ==
               Spitfire.parse("%Foo.Bar = x")

      assert {:error,
              {:=, [line: 1, column: 13],
               [
                 {:%, [line: 1, column: 1],
                  [
                    {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                    {:%{}, [closing: [line: 1, column: 11], line: 1, column: 6], [a: 42]}
                  ]},
                 {:x, [line: 1, column: 15], nil}
               ]},
              [{[line: 1, column: 2], "missing opening brace for struct %Foo"}]} ==
               Spitfire.parse("%Foo a: 42} = x")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                 {:%{}, [line: 1, column: 6], [a: 1]}
               ]},
              [
                {[line: 1, column: 2], "missing opening brace for struct %Foo"},
                {[line: 1, column: 9], "missing closing brace for struct %Foo"}
              ]} == Spitfire.parse("%Foo a: 1")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                 {:%{}, [closing: [line: 1, column: 16], line: 1, column: 6], [a: 1, b: 2]}
               ]},
              [{[line: 1, column: 2], "missing opening brace for struct %Foo"}]} ==
               Spitfire.parse("%Foo a: 1, b: 2}")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                 {:%{}, [closing: [line: 1, column: 14], line: 1, column: 6],
                  [{:|, [line: 1, column: 8], [{:x, [line: 1, column: 6], nil}, [a: 1]]}]}
               ]},
              [{[line: 1, column: 2], "missing opening brace for struct %Foo"}]} ==
               Spitfire.parse("%Foo x | a: 1}")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo]},
                 {:%{}, [line: 1, column: 6], [{:|, [line: 1, column: 8], [{:x, [line: 1, column: 6], nil}, [a: 1]]}]}
               ]},
              [
                {[line: 1, column: 2], "missing opening brace for struct %Foo"},
                {[line: 1, column: 13], "missing closing brace for struct %Foo"}
              ]} == Spitfire.parse("%Foo x | a: 1")

      assert {:error,
              {:foo, [closing: [line: 1, column: 15], line: 1, column: 1],
               [
                 {:%, [line: 1, column: 5],
                  [
                    {:__aliases__, [last: [line: 1, column: 6], line: 1, column: 6], [:Bar]},
                    {:%{}, [closing: [line: 1, column: 14], line: 1, column: 10], [a: 1]}
                  ]}
               ]},
              [{[line: 1, column: 6], "missing opening brace for struct %Bar"}]} ==
               Spitfire.parse("foo(%Bar a: 1})")

      assert {:error,
              {:|>, [line: 1, column: 3],
               [
                 {:x, [line: 1, column: 1], nil},
                 {:%, [line: 1, column: 6],
                  [
                    {:__aliases__, [last: [line: 1, column: 7], line: 1, column: 7], [:Foo]},
                    {:%{}, [closing: [line: 1, column: 15], line: 1, column: 11], [a: 1]}
                  ]}
               ]},
              [{[line: 1, column: 7], "missing opening brace for struct %Foo"}]} ==
               Spitfire.parse("x |> %Foo a: 1}")

      # Nested structs
      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Outer]},
                 {:%{}, [closing: [line: 1, column: 26], line: 1, column: 7],
                  [
                    inner:
                      {:%, [line: 1, column: 15],
                       [
                         {:__aliases__, [last: [line: 1, column: 16], line: 1, column: 16], [:Inner]},
                         {:%{}, [closing: [line: 1, column: 26], line: 1, column: 22], [a: 1]}
                       ]}
                  ]}
               ]},
              [
                {[line: 1, column: 16], "missing opening brace for struct %Inner"},
                {[line: 1, column: 26], "missing closing brace for struct %Outer"}
              ]} == Spitfire.parse("%Outer{inner: %Inner a: 1}")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Outer]},
                 {:%{}, [closing: [line: 1, column: 27], line: 1, column: 8],
                  [
                    inner:
                      {:%, [line: 1, column: 15],
                       [
                         {:__aliases__, [last: [line: 1, column: 16], line: 1, column: 16], [:Inner]},
                         {:%{}, [closing: [line: 1, column: 26], line: 1, column: 21], [a: 1]}
                       ]}
                  ]}
               ]},
              [{[line: 1, column: 2], "missing opening brace for struct %Outer"}]} ==
               Spitfire.parse("%Outer inner: %Inner{a: 1}}")

      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Outer]},
                 {:%{}, [closing: [line: 1, column: 27], line: 1, column: 8],
                  [
                    inner:
                      {:%, [line: 1, column: 15],
                       [
                         {:__aliases__, [last: [line: 1, column: 16], line: 1, column: 16], [:Inner]},
                         {:%{}, [closing: [line: 1, column: 26], line: 1, column: 22], [a: 1]}
                       ]}
                  ]}
               ]},
              [
                {[line: 1, column: 2], "missing opening brace for struct %Outer"},
                {[line: 1, column: 16], "missing opening brace for struct %Inner"}
              ]} == Spitfire.parse("%Outer inner: %Inner a: 1}}")

      # Module attribute struct
      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:@, [line: 1, column: 2], [{:foo, [line: 1, column: 3], nil}]},
                 {:%{}, [line: 1, column: 7], [a: 1]}
               ]},
              [
                {[line: 1, column: 3], "missing opening brace for struct %@foo"},
                {[line: 1, column: 10], "missing closing brace for struct %@foo"}
              ]} == Spitfire.parse("%@foo a: 1")

      # __MODULE__ struct
      assert {:error,
              {:%, [line: 1, column: 1],
               [
                 {:__MODULE__, [line: 1, column: 2], nil},
                 {:%{}, [line: 1, column: 13], [a: 1]}
               ]},
              [
                {[line: 1, column: 2], "missing opening brace for struct %__MODULE__"},
                {[line: 1, column: 16], "missing closing brace for struct %__MODULE__"}
              ]} == Spitfire.parse("%__MODULE__ a: 1")
    end

    test "stray closing delimiter after identifier" do
      assert {:error, {:__block__, [], [{:x, _, nil}, {:__block__, [error: true, line: 1, column: 3], []}]},
              [{[line: 1, column: 3], "unknown token: ]"}]} = Spitfire.parse("x ]")

      assert {:error, {:__block__, [], [{:x, _, nil}, {:__block__, [error: true, line: 1, column: 3], []}]},
              [{[line: 1, column: 3], "unknown token: }"}]} = Spitfire.parse("x }")

      assert {:error, {:__block__, [], [{:x, _, nil}, {:__block__, [error: true, line: 1, column: 3], []}]},
              [{[line: 1, column: 3], "unknown token: )"}]} = Spitfire.parse("x )")

      assert {:error, {:__block__, [], [{:@, _, [{:x, _, nil}]}, {:__block__, [error: true, line: 1, column: 4], []}]},
              [{[line: 1, column: 4], "unknown token: ]"}]} = Spitfire.parse("@x ]")

      assert {:error, {:__block__, [], [1, {:__block__, [error: true, line: 1, column: 3], []}]},
              [{[line: 1, column: 3], "unknown token: ]"}]} = Spitfire.parse("1 ]")

      assert {:error, _ast, [{[line: 1, column: 3], "unknown token: ]"}, {[line: 1, column: 5], "unknown token: }"}]} =
               Spitfire.parse("x ] }")

      code = """
      x]

      foo = Foo.bar(42)
      """

      assert {:error,
              {:__block__, [],
               [
                 {:x, _, nil},
                 {:__block__, [end_of_expression: _, error: true, line: 1, column: 2], []},
                 {:=, _, [{:foo, _, nil}, {{:., _, [{:__aliases__, _, [:Foo]}, :bar]}, _, [42]}]}
               ]}, [{[line: 1, column: 2], "unknown token: ]"}]} = Spitfire.parse(code)
    end

    test "stray closing delimiter after complete expression" do
      assert {:error,
              {:__block__, [],
               [
                 {:foo, [closing: [line: 1, column: 5], line: 1, column: 1], []},
                 {:__block__, [error: true, line: 1, column: 6], []}
               ]}, [{[line: 1, column: 6], "unknown token: }"}]} = Spitfire.parse("foo()}")

      assert {:error, {:__block__, [], [[1, 2], {:__block__, [error: true, line: 1, column: 7], []}]},
              [{[line: 1, column: 7], "unknown token: }"}]} = Spitfire.parse("[1, 2]}")

      assert {:error,
              {:__block__, [],
               [
                 {:%{}, [closing: [line: 1, column: 7], line: 1, column: 1], [a: 1]},
                 {:__block__, [error: true, line: 1, column: 8], []}
               ]}, [{[line: 1, column: 8], "unknown token: )"}]} = Spitfire.parse("%{a: 1})")
    end

    test "stab expressions with capture between followed by fn" do
      # This case is very contrived and was found by fuzzing tokens, trying to
      # reproduce a real world crash for which we didn't have a repro case.
      assert Spitfire.parse("a -> b &0 c -> d fn e f -> g end end") ==
               {:error,
                [
                  {:->, [line: 1, column: 3], [[{:a, [line: 1, column: 1], nil}], {:__block__, [], []}]},
                  {:->, [line: 1, column: 13],
                   [
                     [],
                     {:d, [line: 1, column: 16],
                      [
                        {:fn, [closing: [line: 1, column: 30], line: 1, column: 18],
                         [
                           {:e, [line: 1, column: 21],
                            [
                              {:->, [line: 1, column: 25],
                               [[{:f, [line: 1, column: 23], nil}], {:g, [line: 1, column: 28], nil}]}
                            ]}
                         ]}
                      ]}
                   ]},
                  {:__block__, [error: true, line: 1, column: 30], []},
                  {:__block__, [error: true, line: 1, column: 34], []}
                ],
                [
                  {[line: 1, column: 11], "syntax error"},
                  {[line: 1, column: 30], "unknown token: end"},
                  {[line: 1, column: 34], "unknown token: end"}
                ]}
    end
  end

  describe "&parse_with_comments/2" do
    test "returns the comments" do
      code = ~S'''
        # hello
        # world
        :foo
      '''

      assert {:ok, _ast, comments} = Spitfire.parse_with_comments(code)
      assert [%{line: 1, text: "# hello"}, %{line: 2, text: "# world"}] = comments
      assert Spitfire.parse_with_comments(code) == s2qwc(code)
    end

    test "returns the same comments as string_to_quoted_with_comments" do
      code = ~S'''
        # This is a comment
        :foo # This is also a valid comment
        defmodule Foo do
          # I am a comment in the module
          def foo() do
            :foo # Another one
          end
        end
        # Some more comments!
      '''

      assert Spitfire.parse_with_comments(code) == s2qwc(code)
    end
  end

  describe "container_cursor_to_quoted/2" do
    test "example from docs" do
      # example from the docs
      code = ~S'''
      max(some_value,
      '''

      assert {:ok,
              {:max, [closing: [line: 2, column: 13], line: 1, column: 1],
               [
                 {:some_value, [line: 1, column: 5], nil},
                 {:__cursor__, [closing: [line: 2, column: 12], line: 2, column: 1], []}
               ]}} = Spitfire.container_cursor_to_quoted(code)
    end

    test "more complex example" do
      # example from the docs
      code = ~S'''
      defmodule Foo do
        def foo() do
      '''

      assert {:ok,
              {:defmodule, [do: [line: 1, column: 15], end: [line: 3, column: 16], line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:Foo]},
                 [
                   do:
                     {:def,
                      [
                        do: [line: 2, column: 13],
                        end: [line: 3, column: 13],
                        line: 2,
                        column: 3
                      ],
                      [
                        {:foo, [closing: [line: 2, column: 11], line: 2, column: 7], []},
                        [
                          do: {:__cursor__, [closing: [line: 3, column: 12], line: 3, column: 1], []}
                        ]
                      ]}
                 ]
               ]}} = Spitfire.container_cursor_to_quoted(code)
    end

    test "ending on kw list" do
      # example from the docs
      code = ~S'''
      defmodule Foo do
        def foo() do
         [foo:
      '''

      assert {:ok,
              {:defmodule, [do: [line: 1, column: 15], end: [line: 4, column: 17], line: 1, column: 1],
               [
                 {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:Foo]},
                 [
                   do:
                     {:def,
                      [
                        do: [line: 2, column: 13],
                        end: [line: 4, column: 14],
                        line: 2,
                        column: 3
                      ],
                      [
                        {:foo, [closing: [line: 2, column: 11], line: 2, column: 7], []},
                        [
                          do: [
                            foo: {:__cursor__, [closing: [line: 4, column: 12], line: 4, column: 1], []}
                          ]
                        ]
                      ]}
                 ]
               ]}} = Spitfire.container_cursor_to_quoted(code)
    end

    test "ending inside a -> expression" do
      # example from the docs
      code = ~S'''
      defmodule Foo do
        def foo(items) do
          Enum.map(items, fn i ->
            case i do
              :ok ->
                :ok

               error ->
      '''

      assert {:ok,
              {
                :defmodule,
                [do: [line: 1, column: 15], end: [line: 9, column: 23], line: 1, column: 1],
                [
                  {:__aliases__, [last: [line: 1, column: 11], line: 1, column: 11], [:Foo]},
                  [
                    do: {
                      :def,
                      [do: [line: 2, column: 18], end: [line: 9, column: 20], line: 2, column: 3],
                      [
                        {:foo, [closing: [line: 2, column: 16], line: 2, column: 7],
                         [{:items, [line: 2, column: 11], nil}]},
                        [
                          do: {
                            {:., [line: 3, column: 9],
                             [
                               {:__aliases__, [last: [line: 3, column: 5], line: 3, column: 5], [:Enum]},
                               :map
                             ]},
                            [closing: [line: 9, column: 19], line: 3, column: 10],
                            [
                              {:items, [line: 3, column: 14], nil},
                              {
                                :fn,
                                [closing: [line: 9, column: 16], line: 3, column: 21],
                                [
                                  {
                                    :->,
                                    [newlines: 1, line: 3, column: 26],
                                    [
                                      [{:i, [line: 3, column: 24], nil}],
                                      {
                                        :case,
                                        [
                                          {:do, [line: 4, column: 14]},
                                          {:end, [line: 9, column: 13]},
                                          {:line, 4},
                                          {:column, 7}
                                        ],
                                        [
                                          {:i, [line: 4, column: 12], nil},
                                          [
                                            do: [
                                              {:->, [newlines: 1, line: 5, column: 13], [[:ok], :ok]},
                                              {:->, [newlines: 1, line: 8, column: 16],
                                               [
                                                 [{:error, [line: 8, column: 10], nil}],
                                                 {:__cursor__,
                                                  [
                                                    closing: [line: 9, column: 12],
                                                    line: 9,
                                                    column: 1
                                                  ], []}
                                               ]}
                                            ]
                                          ]
                                        ]
                                      }
                                    ]
                                  }
                                ]
                              }
                            ]
                          }
                        ]
                      ]
                    }
                  ]
                ]
              }} = Spitfire.container_cursor_to_quoted(code)
    end
  end

  # The Elixir tokenizer emits atom_safe/kw_identifier_safe tokens when
  # existing_atoms_only: true is passed. Spitfire parses these to AST
  # using :erlang.binary_to_existing_atom instead of :binary_to_atom.
  describe "existing_atoms_only option" do
    test "atom_safe token is parsed to :binary_to_existing_atom" do
      code = ~S|:"abc#{foo}"|

      # atom_unsafe -> :binary_to_atom
      assert {:ok, {{:., _, [:erlang, :binary_to_atom]}, _, _}} = Spitfire.parse(code)

      # atom_safe -> :binary_to_existing_atom
      assert {:ok, {{:., _, [:erlang, :binary_to_existing_atom]}, _, _}} =
               Spitfire.parse(code, existing_atoms_only: true)
    end

    test "kw_identifier_safe token is parsed to :binary_to_existing_atom" do
      # in list
      code = ~S|["abc#{foo}": 1]|
      assert {:ok, [{{{:., _, [:erlang, :binary_to_atom]}, _, _}, 1}]} = Spitfire.parse(code)

      assert {:ok, [{{{:., _, [:erlang, :binary_to_existing_atom]}, _, _}, 1}]} =
               Spitfire.parse(code, existing_atoms_only: true)

      # in map
      code = ~S|%{"abc#{foo}": 1}|
      assert {:ok, {:%{}, _, [{{{:., _, [:erlang, :binary_to_atom]}, _, _}, 1}]}} = Spitfire.parse(code)

      assert {:ok, {:%{}, _, [{{{:., _, [:erlang, :binary_to_existing_atom]}, _, _}, 1}]}} =
               Spitfire.parse(code, existing_atoms_only: true)

      # multiple items in bracketless kw list
      code = ~S|foo("a#{b}": 1, "c#{d}": 2)|

      assert {:ok,
              {:foo, _,
               [
                 [
                   {{{:., _, [:erlang, :binary_to_existing_atom]}, _, _}, 1},
                   {{{:., _, [:erlang, :binary_to_existing_atom]}, _, _}, 2}
                 ]
               ]}} =
               Spitfire.parse(code, existing_atoms_only: true)
    end
  end

  defp s2q(code, opts \\ []) do
    Code.string_to_quoted(
      code,
      Keyword.merge([columns: true, token_metadata: true, emit_warnings: false], opts)
    )
  end

  defp s2qwc(code, opts \\ []) do
    Code.string_to_quoted_with_comments(
      code,
      Keyword.merge([columns: true, token_metadata: true, emit_warnings: false], opts)
    )
  end

  def print(ast) do
    ast |> Macro.to_string() |> IO.puts()
    ast
  end
end
