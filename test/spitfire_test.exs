defmodule SpitfireTest do
  use ExUnit.Case

  doctest Spitfire

  describe "valid code" do
    import Kernel, except: [==: 2]
    import Spitfire.TestHelpers

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

      assert Spitfire.parse!(code) ==
               {:defmodule, [line: 1, column: 1],
                [
                  {:__aliases__, [line: 1, column: 11], [:Foo]},
                  [
                    do:
                      {:__block__, [],
                       [
                         {:use, [line: 2, column: 3],
                          [
                            {:__aliases__, [line: 2, column: 7], [:AnotherMod, :Nested]},
                            [some: :option]
                          ]},
                         {:def, [line: 5, column: 3],
                          [
                            {:run, [line: 5, column: 7], [{:arg, [line: 5, column: 11], Elixir}]},
                            [do: {:__block__, [], [{:bar, [line: 6, column: 5], []}, :ok]}]
                          ]}
                       ]}
                  ]
                ]}
    end

    test "access syntax" do
      code = "foo[:bar]"

      assert Spitfire.parse!(code) ==
               {{:., [line: 1, column: 4], [Access, :get]}, [line: 1, column: 4, from_brackets: true],
                [{:foo, [line: 1, column: 1], Elixir}, :bar]}

      code = "foo[:bar][:baz]"

      assert Spitfire.parse(code) ==
               {:ok,
                {{:., [from_brackets: true, closing: [line: 1, column: 15], line: 1, column: 10], [Access, :get]},
                 [from_brackets: true, closing: [line: 1, column: 15], line: 1, column: 10],
                 [
                   {{:., [closing: [line: 1, column: 9], line: 1, column: 4], [Access, :get]},
                    [closing: [line: 1, column: 9], line: 1, column: 4], [{:foo, [line: 1, column: 1], Elixir}, :bar]},
                   :baz
                 ]}}

      code = ~S'(meta[:end_of_expression] || meta)[:line]'

      assert Spitfire.parse(code) ==
               {:ok,
                {{:., [from_brackets: true, closing: [line: 1, column: 41], line: 1, column: 35], [Access, :get]},
                 [from_brackets: true, closing: [line: 1, column: 41], line: 1, column: 35],
                 [
                   {:||, [line: 1, column: 27],
                    [
                      {{:., [closing: [line: 1, column: 25], line: 1, column: 6], [Access, :get]},
                       [closing: [line: 1, column: 25], line: 1, column: 6],
                       [{:meta, [line: 1, column: 2], Elixir}, :end_of_expression]},
                      {:meta, [line: 1, column: 30], Elixir}
                    ]},
                   :line
                 ]}}

      code = "%{bar: :foo}[:bar]"

      assert Spitfire.parse!(code) ==
               {{:., [line: 1, column: 14], [Access, :get]}, [line: 1, column: 14, from_brackets: true],
                [{:%{}, [line: 1, column: 1], [bar: :foo]}, :bar]}
    end

    test "token metadata" do
      import Kernel
      import Spitfire.TestHelpers, only: []

      code = ~S'''
      foo do
        1 + 1
      end
      '''

      assert Spitfire.parse!(code, token_metadata: true) ==
               {:foo, [do: [line: 1, column: 5], end: [line: 3, column: 1], line: 1, column: 1],
                [[do: {:+, [line: 2, column: 5], [1, 1]}]]}

      code = ~S'''
      foo do
        bar do
          1 + 1
        end
      end
      '''

      assert Spitfire.parse!(code, token_metadata: true) ==
               {:foo, [do: [line: 1, column: 5], end: [line: 5, column: 1], line: 1, column: 1],
                [
                  [
                    do:
                      {:bar, [do: [line: 2, column: 7], end: [line: 4, column: 3], line: 2, column: 3],
                       [[do: {:+, [line: 3, column: 7], [1, 1]}]]}
                  ]
                ]}
    end

    test "type syntax" do
      code = ~S'''
      @type foo :: String.t()
      '''

      assert Spitfire.parse!(code) ==
               {:@, [line: 1, column: 1],
                [
                  {:type, [line: 1, column: 2],
                   [
                     {:"::", [line: 1, column: 11],
                      [
                        {:foo, [line: 1, column: 7], Elixir},
                        {{:., [], [{:__aliases__, [line: 1, column: 14], [:String]}, :t]}, [], []}
                      ]}
                   ]}
                ]}

      code = ~S'''
      @spec foo(one :: String.t(), number) :: :ok | :error
      '''

      assert Spitfire.parse!(code) ==
               {:@, [line: 1, column: 1],
                [
                  {:spec, [line: 1, column: 2],
                   [
                     {:"::", [line: 1, column: 38],
                      [
                        {:foo, [line: 1, column: 7],
                         [
                           {:"::", [line: 1, column: 15],
                            [
                              {:one, [line: 1, column: 11], Elixir},
                              {{:., [], [{:__aliases__, [line: 1, column: 18], [:String]}, :t]}, [], []}
                            ]},
                           {:number, [line: 1, column: 30], Elixir}
                         ]},
                        {:|, [line: 1, column: 45], [:ok, :error]}
                      ]}
                   ]}
                ]}
    end

    test "parses unary operators" do
      code = ~S'''
      ^foo
      '''

      assert Spitfire.parse!(code) == {:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], Elixir}]}
    end

    test "parses numbers" do
      code = """
      111_111
      """

      assert Spitfire.parse!(code) == 111_111

      code = """
      1.4
      """

      assert Spitfire.parse(code) == {:ok, 1.4}
    end

    test "parses strings" do
      code = ~s'''
      "foobar" 
      '''

      assert Spitfire.parse!(code) == "foobar"

      code = ~S'''
      """
      foobar
      """
      '''

      assert Spitfire.parse!(code) == """
             foobar
             """
    end

    test "parses charlists" do
      code = ~s'''
      'foobar' 
      '''

      assert Spitfire.parse(code) == {:ok, ~c"foobar"}

      code = ~S"""
      '''
      foobar
      '''
      """

      assert Spitfire.parse(code) ==
               {:ok,
                ~c"""
                foobar
                """}

      code = ~S'''
      'foo#{alice}bar' 
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {{:., [], [List, :to_charlist]}, [],
                 [
                   ~c"foo",
                   {:"::", [], [{{:., [], [Kernel, :to_string]}, [], [{:alice, [], Elixir}]}, {:binary, [], Elixir}]},
                   ~c"bar"
                 ]}}

      code = ~S"""
      '''
      foo#{alice}bar
      '''
      """

      assert Spitfire.parse(code) ==
               {:ok,
                {{:., [], [List, :to_charlist]}, [],
                 [
                   "foo",
                   {:"::", [], [{{:., [], [Kernel, :to_string]}, [], [{:alice, [], Elixir}]}, {:binary, [], Elixir}]},
                   "bar\n"
                 ]}}
    end

    test "parses atoms" do
      code = ~s'''
      :foobar
      '''

      assert Spitfire.parse!(code) == :foobar

      code = ~s'''
      :","
      '''

      assert Spitfire.parse!(code) == :","

      code = ~S'''
      :"foo#{}"
      '''

      assert Spitfire.parse!(code) ==
               {{:., [line: 1, column: 1], [:erlang, :binary_to_atom]}, [line: 1, column: 1],
                [
                  {:<<>>, [line: 1, column: 1],
                   [
                     "foo",
                     {:"::", [line: 1, column: 1],
                      [
                        {{:., [line: 1, column: 1], [Kernel, :to_string]}, [line: 1, column: 1, from_interpolation: true],
                         [{:__block__, [], []}]},
                        {:binary, [], Elixir}
                      ]}
                   ]},
                  :utf8
                ]}

      code = ~S'''
      :"foo#{bar}"
      '''

      assert Spitfire.parse!(code) ==
               {{:., [line: 1, column: 1], [:erlang, :binary_to_atom]}, [line: 1, column: 1],
                [
                  {:<<>>, [line: 1, column: 1],
                   [
                     "foo",
                     {:"::", [line: 1, column: 1],
                      [
                        {{:., [line: 1, column: 1], [Kernel, :to_string]}, [line: 1, column: 1, from_interpolation: true],
                         [{:bar, [line: 1, column: 8], Elixir}]},
                        {:binary, [], Elixir}
                      ]}
                   ]},
                  :utf8
                ]}
    end

    test "parses left stab" do
      code = """
      apple <- apples
      """

      assert Spitfire.parse!(code) ==
               {:<-, [line: 1, column: 7],
                [{:apple, [line: 1, column: 1], Elixir}, {:apples, [line: 1, column: 10], Elixir}]}
    end

    test "parses right stab" do
      code = """
      -> bar
      """

      assert Spitfire.parse!(code) == [{:->, [line: 1, column: 1], [[], {:bar, [line: 1, column: 4], Elixir}]}]

      code = """
      -> :ok
      """

      assert Spitfire.parse!(code) == [{:->, [line: 1, column: 1], [[], :ok]}]

      code = """
      foo -> bar
      """

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 5],
                [[{:foo, [line: 1, column: 1], Elixir}], {:bar, [line: 1, column: 8], Elixir}]}
             ]

      code = """
      foo, bar, baz -> bar
      """

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 15],
                [
                  [
                    {:foo, [line: 1, column: 1], Elixir},
                    {:bar, [line: 1, column: 6], Elixir},
                    {:baz, [line: 1, column: 11], Elixir}
                  ],
                  {:bar, [line: 1, column: 18], Elixir}
                ]}
             ]

      code = """
      alice, bob, carol ->
        :error
        bar
      """

      # if we get a prefix comma operator, that means we might need to backtrack and then
      # parse a comma list. if we hit the operator, it means that we are not actually in an
      # existing comma list, like a list or a map
      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 19],
                [
                  [
                    {:alice, [line: 1, column: 1], Elixir},
                    {:bob, [line: 1, column: 8], Elixir},
                    {:carol, [line: 1, column: 13], Elixir}
                  ],
                  {:__block__, [], [:error, {:bar, [line: 3, column: 3], Elixir}]}
                ]}
             ]

      code = """
      foo ->
        :ok
        baz

      alice, bob, carol ->
        :error
        bar
      """

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 5],
                [[{:foo, [line: 1, column: 1], Elixir}], {:__block__, [], [:ok, {:baz, [line: 3, column: 3], Elixir}]}]},
               {:->, [depth: 0, line: 5, column: 19],
                [
                  [
                    {:alice, [line: 5, column: 1], Elixir},
                    {:bob, [line: 5, column: 8], Elixir},
                    {:carol, [line: 5, column: 13], Elixir}
                  ],
                  {:__block__, [], [:error, {:bar, [line: 7, column: 3], Elixir}]}
                ]}
             ]

      code = ~S'''
      ^foo ->
        :ok
      '''

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 6],
                [[{:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], Elixir}]}], :ok]}
             ]

      code = ~S'''
      @foo ->
        :ok
      '''

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 6],
                [[{:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], Elixir}]}], :ok]}
             ]
    end

    test "parses grouped expressions" do
      codes = [
        {~s'''
         1 + 2 + 3
         ''', {:+, [line: 1, column: 7], [{:+, [line: 1, column: 3], [1, 2]}, 3]}},
        {~s'''
         (1 + 2) + 3
         ''', {:+, [line: 1, column: 9], [{:+, [line: 1, column: 4], [1, 2]}, 3]}},
        {~s'''
         ((1 + 2) + 3)
         ''', {:+, [line: 1, column: 10], [{:+, [line: 1, column: 5], [1, 2]}, 3]}},
        {~s'''
         1 + (2 + 3)
         ''', {:+, [line: 1, column: 3], [1, {:+, [line: 1, column: 8], [2, 3]}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses for comprehension" do
      codes = [
        {~s'''
         for i <- 0..100 do
           i + i
         end
         ''',
         {:for, [line: 1, column: 1],
          [
            {:<-, [line: 1, column: 7], [{:i, [line: 1, column: 5], Elixir}, {:.., [line: 1, column: 11], [0, 100]}]},
            [do: {:+, [line: 2, column: 5], [{:i, [line: 2, column: 3], Elixir}, {:i, [line: 2, column: 7], Elixir}]}]
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses with expression" do
      codes = [
        {~s'''
         with {:ok, school} <- State.get_school(id),
              {:ok, teachers} <- School.list_teachers(school),
              {:ok, teacher} <- Teacher.coolest(teachers) do
           Email.send(teacher, "You are the coolest teacher")
         end
         ''',
         {:with, [line: 1, column: 1],
          [
            {:<-, [line: 1, column: 20],
             [
               {:ok, {:school, [line: 1, column: 12], Elixir}},
               {{:., [], [{:__aliases__, [line: 1, column: 23], [:State]}, :get_school]}, [],
                [{:id, [line: 1, column: 40], Elixir}]}
             ]},
            {:<-, [line: 2, column: 22],
             [
               {:ok, {:teachers, [line: 2, column: 12], Elixir}},
               {{:., [], [{:__aliases__, [line: 2, column: 25], [:School]}, :list_teachers]}, [],
                [{:school, [line: 2, column: 46], Elixir}]}
             ]},
            {:<-, [line: 3, column: 21],
             [
               {:ok, {:teacher, [line: 3, column: 12], Elixir}},
               {{:., [], [{:__aliases__, [line: 3, column: 24], [:Teacher]}, :coolest]}, [],
                [{:teachers, [line: 3, column: 40], Elixir}]}
             ]},
            [
              do:
                {{:., [], [{:__aliases__, [line: 4, column: 3], [:Email]}, :send]}, [],
                 [{:teacher, [line: 4, column: 14], Elixir}, "You are the coolest teacher"]}
            ]
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses variable identifiers" do
      code = ~s'''
      foobar
      alice
      bob
      '''

      assert Spitfire.parse!(code) ==
               {:__block__, [],
                [
                  {:foobar, [line: 1, column: 1], Elixir},
                  {:alice, [line: 2, column: 1], Elixir},
                  {:bob, [line: 3, column: 1], Elixir}
                ]}
    end

    test "parses lists" do
      codes = [
        {~s'''
         []
         ''', {:ok, []}},
        {~s'''
         [arg]
         ''', {:ok, [{:arg, [line: 1, column: 2], Elixir}]}},
        {~s'''
         [one, :two, "three"]
         ''', {:ok, [{:one, [line: 1, column: 2], Elixir}, :two, "three"]}},
        {~s'''
          [
            one,
            :two,
            "three"
          ]
         ''', {:ok, [{:one, [line: 2, column: 4], Elixir}, :two, "three"]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == expected
      end
    end

    test "parses bracket-less keyword lists" do
      codes = [
        {~s'''
         foo(one, two, alice: alice, bob: bob)
         ''',
         {:foo, [line: 1, column: 1],
          [
            {:one, [line: 1, column: 5], Elixir},
            {:two, [line: 1, column: 10], Elixir},
            [alice: {:alice, [line: 1, column: 22], Elixir}, bob: {:bob, [line: 1, column: 34], Elixir}]
          ]}},
        {~s'''
         foo alice: alice do
           :ok
         end
         ''', {:foo, [line: 1, column: 1], [[alice: {:alice, [line: 1, column: 12], Elixir}], [do: :ok]]}},
        {~s'''
         [:one, two: :three]
         ''', [:one, {:two, :three}]}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "another thing" do
      code = ~S'''
      case foo do
        :kw_identifier when is_list or is_map -> &parse_kw_identifier/1
      end
      '''

      assert Spitfire.parse!(code) ==
               {:case, [line: 1, column: 1],
                [
                  {:foo, [line: 1, column: 6], Elixir},
                  [
                    do: [
                      {:->, [depth: 1, line: 2, column: 41],
                       [
                         [
                           {:when, [line: 2, column: 18],
                            [
                              :kw_identifier,
                              {:or, [line: 2, column: 31],
                               [{:is_list, [line: 2, column: 23], Elixir}, {:is_map, [line: 2, column: 34], Elixir}]}
                            ]}
                         ],
                         {:&, [line: 2, column: 44],
                          [{:/, [line: 2, column: 64], [{:parse_kw_identifier, [line: 2, column: 45], Elixir}, 1]}]}
                       ]}
                    ]
                  ]
                ]}
    end

    test "parses pattern matching in list" do
      codes = [
        {~s'''
         [one | rest] = my_list
         ''',
         {:=, [line: 1, column: 14],
          [
            [{:|, [line: 1, column: 6], [{:one, [line: 1, column: 2], Elixir}, {:rest, [line: 1, column: 8], Elixir}]}],
            {:my_list, [line: 1, column: 16], Elixir}
          ]}},
        {~s'''
         [one, two | rest] = my_list
         ''',
         {:=, [line: 1, column: 19],
          [
            [
              {:one, [line: 1, column: 2], Elixir},
              {:|, [line: 1, column: 11], [{:two, [line: 1, column: 7], Elixir}, {:rest, [line: 1, column: 13], Elixir}]}
            ],
            {:my_list, [line: 1, column: 21], Elixir}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses tuples" do
      codes = [
        {~s'''
         {}
         ''', {:{}, [line: 1, column: 1], []}},
        {~s'''
         {one, :two}
         ''', {{:one, [line: 1, column: 2], Elixir}, :two}},
        {~s'''
         {
           one,
           :two,
           "three"
         }
         ''', {:{}, [line: 1, column: 1], [{:one, [line: 2, column: 3], Elixir}, :two, "three"]}},
        {~s'''
         {one, :two, "three"}
         ''', {:{}, [line: 1, column: 1], [{:one, [line: 1, column: 2], Elixir}, :two, "three"]}},
        {~s'''
         {
           one,
           :two,
           "three"
         }
         ''', {:{}, [line: 1, column: 1], [{:one, [line: 2, column: 3], Elixir}, :two, "three"]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses aliases" do
      codes = [
        {~s'''
         Remote
         ''', {:__aliases__, [line: 1, column: 1], [:Remote]}},
        {~s'''
         Remote.Foo
         ''', {:__aliases__, [line: 1, column: 1], [:Remote, :Foo]}},
        {~s'''
         Remote.Foo.Bar
         ''', {:__aliases__, [line: 1, column: 1], [:Remote, :Foo, :Bar]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses maps" do
      codes = [
        {~s'''
         %{}
         ''', {:%{}, [line: 1, column: 1], []}},
        {~s'''
         %{
           foo: "bar",
           alice: "bob"
          }
         ''', {:%{}, [line: 1, column: 1], [{:foo, "bar"}, {:alice, "bob"}]}},
        {~s'''
         %{
           "foo" =>
             "bar",
           "alice" => "bob"
          }
         ''', {:%{}, [line: 1, column: 1], [{"foo", "bar"}, {"alice", "bob"}]}},
        {~s'''
         %{"foo" => "bar", 1 => 2, :three => :four, [] => [1], %{} => nil, bing => bong, foo: :bar}
         ''',
         {:%{}, [line: 1, column: 1],
          [
            {"foo", "bar"},
            {1, 2},
            {:three, :four},
            {[], [1]},
            {{:%{}, [line: 1, column: 55], []}, nil},
            {{:bing, [line: 1, column: 67], Elixir}, {:bong, [line: 1, column: 75], Elixir}},
            {:foo, :bar}
          ]}},
        {~s'''
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
         {:%{}, [line: 1, column: 1],
          [
            {"foo", "bar"},
            {1, 2},
            {:three, :four},
            {[], [1]},
            {{:%{}, [line: 6, column: 3], []}, nil},
            {{:bing, [line: 7, column: 3], Elixir}, {:bong, [line: 7, column: 11], Elixir}},
            {:foo, :bar}
          ]}},
        {~s'''
         %{
           foo: :bar,
           baz:
             beaux()
         }
         ''',
         {:%{}, [line: 1, column: 1],
          [
            {:foo, :bar},
            {:baz, {:beaux, [], []}}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses structs" do
      codes = [
        {~s'''
         %Foo.Bar{}
         ''',
         {:%, [line: 1, column: 1],
          [
            {:__aliases__, [last: [line: 1, column: 2], line: 1, column: 2], [:Foo, :Bar]},
            {:%{}, [closing: [line: 1, column: 6], line: 1, column: 5], []}
          ]}},
        {~s'%Foo.Bar{name: "alice", height: 73}',
         {:%, [line: 1, column: 1],
          [
            {:__aliases__, [last: [line: 1, column: 6], line: 1, column: 2], [:Foo, :Bar]},
            {:%{}, [closing: [line: 1, column: 35], line: 1, column: 9], [name: "alice", height: 73]}
          ]}},
        {
          ~s'%Foo.Bar{name: name, properties: %Properties{key: key, value: get_value()}}',
          {:%, [line: 1, column: 1],
           [
             {:__aliases__, [last: [line: 1, column: 6], line: 1, column: 2], [:Foo, :Bar]},
             {:%{}, [closing: [line: 1, column: 75], line: 1, column: 9],
              [
                name: {:name, [line: 1, column: 16], Elixir},
                properties:
                  {:%, [line: 1, column: 34],
                   [
                     {:__aliases__, [last: [line: 1, column: 35], line: 1, column: 35], [:Properties]},
                     {:%{}, [closing: [line: 1, column: 74], line: 1, column: 45],
                      [
                        key: {:key, [line: 1, column: 51], Elixir},
                        value: {:get_value, [closing: [line: 1, column: 73], line: 1, column: 63], []}
                      ]}
                   ]}
              ]}
           ]}
        },
        {~S'%__MODULE__{foo: bar}',
         {:%, [line: 1, column: 1],
          [
            {:__MODULE__, [line: 1, column: 2], Elixir},
            {:%{}, [closing: [line: 1, column: 21], line: 1, column: 12], [foo: {:bar, [line: 1, column: 18], Elixir}]}
          ]}},
        {~S'%module{foo: bar}',
         {:%, [line: 1, column: 1],
          [
            {:module, [line: 1, column: 2], Elixir},
            {:%{}, [closing: [line: 1, column: 17], line: 1, column: 8], [foo: {:bar, [line: 1, column: 14], Elixir}]}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
      end
    end

    test "parses operators" do
      codes = [
        {~s'''
         1 + 2
         ''', {:+, [line: 1, column: 3], [1, 2]}},
        {~s'''
         1 - 2
         ''', {:-, [line: 1, column: 3], [1, 2]}},
        {~s'''
         1 - -2
         ''', {:-, [line: 1, column: 3], [1, {:-, [line: 1, column: 5], [2]}]}},
        {~s'''
         1 * 2
         ''', {:*, [line: 1, column: 3], [1, 2]}},
        {~s'''
         1 / 2
         ''', {:/, [line: 1, column: 3], [1, 2]}},
        {~s'''
         1 || foo()
         ''', {:||, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 ||| foo()
         ''', {:|||, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 or foo()
         ''', {:or, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 == foo()
         ''', {:==, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 != foo()
         ''', {:!=, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 =~ foo()
         ''', {:=~, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 === foo()
         ''', {:===, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 !== foo()
         ''', {:!==, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 < foo()
         ''', {:<, [line: 1, column: 3], [1, {:foo, [line: 1, column: 5], []}]}},
        {~s'''
         1 > foo()
         ''', {:>, [line: 1, column: 3], [1, {:foo, [line: 1, column: 5], []}]}},
        {~s'''
         1 <= foo()
         ''', {:<=, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 >= foo()
         ''', {:>=, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 |> foo()
         ''', {:|>, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 <|> foo()
         ''', {:"<|>", [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 <<< foo()
         ''', {:<<<, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 >>> foo()
         ''', {:>>>, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 <<~ foo()
         ''', {:<<~, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 ~>> foo()
         ''', {:~>>, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 <~ foo()
         ''', {:<~, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 ~> foo()
         ''', {:~>, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         1 <~> foo()
         ''', {:<~>, [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 in foo()
         ''', {:in, [line: 1, column: 3], [1, {:foo, [line: 1, column: 6], []}]}},
        {~s'''
         foo not in bar
         ''',
         {:not, [line: 1, column: 5],
          [
            {:in, [], [{:foo, [line: 1, column: 1], Elixir}, {:bar, [line: 1, column: 12], Elixir}]}
          ]}},
        {~s'''
         1 ^^^ foo()
         ''', {:"^^^", [line: 1, column: 3], [1, {:foo, [line: 1, column: 7], []}]}},
        {~s'''
         1 + 2 * 3 - 2
         ''',
         {:-, [line: 1, column: 11],
          [
            {:+, [line: 1, column: 3], [1, {:*, [line: 1, column: 7], [2, 3]}]},
            2
          ]}},
        {~s'''
         one..two
         ''', {:.., [line: 1, column: 4], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 6], Elixir}]}},
        {~s'''
         one..two//2
         ''',
         {:"..//", [line: 1, column: 4], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 6], Elixir}, 2]}},
        {~s'''
         one <> two
         ''', {:<>, [line: 1, column: 5], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 8], Elixir}]}},
        {~s'''
         one ++ two
         ''', {:++, [line: 1, column: 5], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 8], Elixir}]}},
        {~s'''
         one -- two
         ''', {:--, [line: 1, column: 5], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 8], Elixir}]}},
        {~s'''
         one +++ two
         ''', {:+++, [line: 1, column: 5], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 9], Elixir}]}},
        {~s'''
         one --- two
         ''', {:---, [line: 1, column: 5], [{:one, [line: 1, column: 1], Elixir}, {:two, [line: 1, column: 9], Elixir}]}},
        {~s'''
         one ++ two ++ three
         ''',
         {:++, [line: 1, column: 5],
          [
            {:one, [line: 1, column: 1], Elixir},
            {:++, [line: 1, column: 12], [{:two, [line: 1, column: 8], Elixir}, {:three, [line: 1, column: 15], Elixir}]}
          ]}},
        {~s'''
         @foo
         ''', {:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], Elixir}]}},
        {~s'''
         !foo
         ''', {:!, [line: 1, column: 1], [{:foo, [line: 1, column: 2], Elixir}]}},
        {~s'''
         not foo
         ''', {:not, [line: 1, column: 1], [{:foo, [line: 1, column: 5], Elixir}]}},
        {~s'''
         ^foo
         ''', {:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], Elixir}]}},
        {~s'''
         ~~~foo
         ''', {:"~~~", [line: 1, column: 1], [{:foo, [line: 1, column: 4], Elixir}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses setting module attr" do
      codes = [
        {~s'''
         @foo bar()
         ''', {:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], [{:bar, [line: 1, column: 6], []}]}]}},
        {~s'''
         @foo %{
           foo: :bar
         }
         ''', {:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], [{:%{}, [line: 1, column: 6], [foo: :bar]}]}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parse do block" do
      codes = [
        {~s'''
         foo do
         end
         ''', {:foo, [line: 1, column: 1], [[do: {:__block__, [], []}]]}},
        {~s'''
         foo do
          "howdy"
          :partner
         end
         ''',
         {:foo, [line: 1, column: 1],
          [
            [
              do:
                {:__block__, [],
                 [
                   "howdy",
                   :partner
                 ]}
            ]
          ]}},
        {~s'''
         foo arg do
          "howdy"
          :partner
         end
         ''',
         {:foo, [line: 1, column: 1],
          [
            {:arg, [line: 1, column: 5], Elixir},
            [
              do:
                {:__block__, [],
                 [
                   "howdy",
                   :partner
                 ]}
            ]
          ]}},
        {~s'''
         if arg do
          "howdy"
          else
          :partner
         end
         ''', {:if, [line: 1, column: 1], [{:arg, [line: 1, column: 4], Elixir}, [do: "howdy", else: :partner]]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "case expr" do
      codes = [
        {~s'''
         case foo do
          bar ->
            bar

         end
         ''',
         {:ok,
          {:case, [line: 1, column: 1],
           [
             {:foo, [line: 1, column: 6], Elixir},
             [
               do: [
                 {:->, [depth: 1, line: 2, column: 6],
                  [[{:bar, [line: 2, column: 2], Elixir}], {:bar, [line: 3, column: 4], Elixir}]}
               ]
             ]
           ]}}},
        {~s'''
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
         {:ok,
          {:case, [line: 1, column: 1],
           [
             :foo,
             [
               do: [
                 {:->, [depth: 1, line: 2, column: 8],
                  [
                    [:foo],
                    {:case, [line: 3, column: 5],
                     [
                       {:get, [line: 3, column: 10], [:foo]},
                       [
                         do: [
                           {:->, [depth: 2, line: 4, column: 12], [[:FOO], :bar]},
                           {:->, [depth: 2, line: 6, column: 9], [[{:_, [line: 6, column: 7], Elixir}], :error]}
                         ]
                       ]
                     ]}
                  ]},
                 {:->, [depth: 1, line: 10, column: 5], [[{:_, [line: 10, column: 3], Elixir}], :error]}
               ]
             ]
           ]}}},
        {~s'''
         case infix do
           nil ->
             {left, parser}
                                                   
           ^do_block when parser.nestings != [] ->
             {left, next_token(parser)}
                                                   
           _ ->
             infix.(next_token(parser), left)
         end
         ''',
         {:ok,
          {:case, [line: 1, column: 1],
           [
             {:infix, [line: 1, column: 6], Elixir},
             [
               do: [
                 {:->, [depth: 1, line: 2, column: 7],
                  [[nil], {{:left, [line: 3, column: 6], Elixir}, {:parser, [line: 3, column: 12], Elixir}}]},
                 {:->, [depth: 1, line: 5, column: 40],
                  [
                    [
                      {:when, [line: 5, column: 13],
                       [
                         {:^, [line: 5, column: 3], [{:do_block, [line: 5, column: 4], Elixir}]},
                         {:!=, [line: 5, column: 34],
                          [
                            {{:., [], [{:parser, [line: 5, column: 18], Elixir}, :nestings]}, [], []},
                            []
                          ]}
                       ]}
                    ],
                    {{:left, [line: 6, column: 6], Elixir},
                     {:next_token, [line: 6, column: 12], [{:parser, [line: 6, column: 23], Elixir}]}}
                  ]},
                 {:->, [depth: 1, line: 8, column: 5],
                  [
                    [{:_, [line: 8, column: 3], Elixir}],
                    {{:., [], [{:infix, [line: 9, column: 5], Elixir}]}, [],
                     [
                       {:next_token, [line: 9, column: 12], [{:parser, [line: 9, column: 23], Elixir}]},
                       {:left, [line: 9, column: 32], Elixir}
                     ]}
                  ]}
               ]
             ]
           ]}}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == expected
      end
    end

    test "parse ambiguous function calls" do
      codes = [
        {~s'''
         a b c, d
         ''',
         {:a, [line: 1, column: 1],
          [
            {:b, [line: 1, column: 3],
             [
               {:c, [line: 1, column: 5], Elixir},
               {:d, [line: 1, column: 8], Elixir}
             ]}
          ]}},
        {~s'''
         a b c, d do
         end
         ''',
         {:a, [line: 1, column: 1],
          [
            {:b, [line: 1, column: 3],
             [
               {:c, [line: 1, column: 5], Elixir},
               {:d, [line: 1, column: 8], Elixir}
             ]},
            [do: {:__block__, [], []}]
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses function calls" do
      codes = [
        {~s'''
         foo()
         ''', {:foo, [line: 1, column: 1], []}},
        {~s'''
         foo(arg, arg2)
         ''',
         {:foo, [line: 1, column: 1], [{:arg, [line: 1, column: 5], Elixir}, {:arg2, [line: 1, column: 10], Elixir}]}},
        {~s'''
         foo(
           arg,
           arg2
         )
         ''',
         {:foo, [line: 1, column: 1], [{:arg, [line: 2, column: 3], Elixir}, {:arg2, [line: 3, column: 3], Elixir}]}},
        {~s'''
         foo arg, arg2
         ''',
         {:foo, [line: 1, column: 1], [{:arg, [line: 1, column: 5], Elixir}, {:arg2, [line: 1, column: 10], Elixir}]}},
        {~s'''
         Remote.foo
         ''', {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [], []}},
        {~s'''
         Remote.foo()
         ''', {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [], []}},
        {~s'''
         Remote.foo(arg, arg2)
         ''',
         {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [],
          [{:arg, [line: 1, column: 12], Elixir}, {:arg2, [line: 1, column: 17], Elixir}]}},
        {~s'''
         Remote.foo(
           arg,
           arg2
         )
         ''',
         {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [],
          [{:arg, [line: 2, column: 3], Elixir}, {:arg2, [line: 3, column: 3], Elixir}]}},
        {~s'''
         Remote.foo arg, arg2
         ''',
         {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [],
          [{:arg, [line: 1, column: 12], Elixir}, {:arg2, [line: 1, column: 17], Elixir}]}},
        {~s'''
         :erlang.foo
         ''', {{:., [], [:erlang, :foo]}, [], []}},
        {~s'''
         :erlang.foo()
         ''', {{:., [], [:erlang, :foo]}, [], []}},
        {~s'''
         :erlang.foo(arg, arg2)
         ''',
         {{:., [], [:erlang, :foo]}, [], [{:arg, [line: 1, column: 13], Elixir}, {:arg2, [line: 1, column: 18], Elixir}]}},
        {~s'''
         :erlang.foo arg, arg2
         ''',
         {{:., [], [:erlang, :foo]}, [], [{:arg, [line: 1, column: 13], Elixir}, {:arg2, [line: 1, column: 18], Elixir}]}},
        {~s'''
         somevar.foo
         ''', {{:., [], [{:somevar, [line: 1, column: 1], Elixir}, :foo]}, [], []}},
        {~s'''
         somevar.foo()
         ''', {{:., [], [{:somevar, [line: 1, column: 1], Elixir}, :foo]}, [], []}},
        {~s'''
         :elixir_tokenizer.tokenize(String.to_charlist(code), 1, [])
         ''',
         {{:., [], [:elixir_tokenizer, :tokenize]}, [],
          [
            {{:., [], [{:__aliases__, [line: 1, column: 28], [:String]}, :to_charlist]}, [],
             [{:code, [line: 1, column: 47], Elixir}]},
            1,
            []
          ]}},
        {~s'''
         somevar.foo(arg, arg2)
         ''',
         {{:., [], [{:somevar, [line: 1, column: 1], Elixir}, :foo]}, [],
          [{:arg, [line: 1, column: 13], Elixir}, {:arg2, [line: 1, column: 18], Elixir}]}},
        {~s'''
         somevar.foo arg, arg2
         ''',
         {{:., [], [{:somevar, [line: 1, column: 1], Elixir}, :foo]}, [],
          [{:arg, [line: 1, column: 13], Elixir}, {:arg2, [line: 1, column: 18], Elixir}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses anon functions" do
      codes = [
        {~s'''
         fn -> :ok end
         ''', {:fn, [line: 1, column: 1], [{:->, [line: 1, column: 4], [[], :ok]}]}},
        {~s'''
         fn ->
           :ok
         end
         ''', {:fn, [line: 1, column: 1], [{:->, [line: 1, column: 4], [[], :ok]}]}},
        {~s'''
         fn one ->
           one
         end
         ''',
         {:fn, [line: 1, column: 1],
          [
            {:->, [depth: 1, line: 1, column: 8],
             [[{:one, [line: 1, column: 4], Elixir}], {:one, [line: 2, column: 3], Elixir}]}
          ]}},
        {~s'''
         fn
          one ->
           one
         end
         ''',
         {:fn, [line: 1, column: 1],
          [
            {:->, [depth: 1, line: 2, column: 6],
             [[{:one, [line: 2, column: 2], Elixir}], {:one, [line: 3, column: 3], Elixir}]}
          ]}},
        {~s'''
         fn(one) ->
           one
         end
         ''',
         {:fn, [line: 1, column: 1],
          [
            {:->, [depth: 1, line: 1, column: 9],
             [[{:one, [line: 1, column: 4], Elixir}], {:one, [line: 2, column: 3], Elixir}]}
          ]}},
        {~S'foo(fn a -> a end)',
         {:foo, [closing: [line: 1, column: 18], line: 1, column: 1],
          [
            {:fn, [closing: [line: 1, column: 15], line: 1, column: 5],
             [
               {:->, [line: 1, column: 10], [[{:a, [line: 1, column: 8], Elixir}], {:a, [line: 1, column: 13], Elixir}]}
             ]}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
      end
    end

    test "parses match operator" do
      codes = [
        {~s'''
         foo = :bar
         ''', {:=, [line: 1, column: 5], [{:foo, [line: 1, column: 1], Elixir}, :bar]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "parses nil" do
      code = "nil"
      assert Spitfire.parse!(code) == nil
    end

    test "parses booleans" do
      code = "false"
      assert Spitfire.parse!(code) == false

      code = "true"
      assert Spitfire.parse!(code) == true
    end

    test "parses cond expression" do
      codes = [
        {~s'''
         cond do
            prefix == nil ->
              :foo
            true ->
              :bar
          end
         ''',
         {:cond, [line: 1, column: 1],
          [
            [
              do: [
                {:->, [depth: 1, line: 2, column: 18],
                 [[{:==, [line: 2, column: 11], [{:prefix, [line: 2, column: 4], Elixir}, nil]}], :foo]},
                {:->, [depth: 1, line: 4, column: 9], [[true], :bar]}
              ]
            ]
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "|> operator" do
      code = ~S'''
      def parse(code) do
        parser = code |> new() |> next_token() |> next_token()

        parse_program(parser)
      end
      '''

      assert Spitfire.parse!(code) ==
               {:def, [line: 1, column: 1],
                [
                  {:parse, [line: 1, column: 5], [{:code, [line: 1, column: 11], Elixir}]},
                  [
                    do:
                      {:__block__, [],
                       [
                         {:=, [line: 2, column: 10],
                          [
                            {:parser, [line: 2, column: 3], Elixir},
                            {:|>, [line: 2, column: 42],
                             [
                               {:|>, [line: 2, column: 26],
                                [
                                  {:|>, [line: 2, column: 17],
                                   [{:code, [line: 2, column: 12], Elixir}, {:new, [line: 2, column: 20], []}]},
                                  {:next_token, [line: 2, column: 29], []}
                                ]},
                               {:next_token, [line: 2, column: 45], []}
                             ]}
                          ]},
                         {:parse_program, [line: 4, column: 3], [{:parser, [line: 4, column: 17], Elixir}]}
                       ]}
                  ]
                ]}
    end

    test "when operator" do
      codes = [
        {~s'''
         foo when is_binary(foo) ->
           :ok
         ''',
         [
           {:->, [depth: 0, line: 1, column: 25],
            [
              [
                {:when, [line: 1, column: 5],
                 [
                   {:foo, [line: 1, column: 1], Elixir},
                   {:is_binary, [line: 1, column: 10], [{:foo, [line: 1, column: 20], Elixir}]}
                 ]}
              ],
              :ok
            ]}
         ]},
        {~s'''
         foo when is_binary(foo) ->
           :ok

         bar when is_number(bar) ->
           :ok
         ''',
         [
           {:->, [depth: 0, line: 1, column: 25],
            [
              [
                {:when, [line: 1, column: 5],
                 [
                   {:foo, [line: 1, column: 1], Elixir},
                   {:is_binary, [line: 1, column: 10], [{:foo, [line: 1, column: 20], Elixir}]}
                 ]}
              ],
              :ok
            ]},
           {:->, [depth: 0, line: 4, column: 25],
            [
              [
                {:when, [line: 4, column: 5],
                 [
                   {:bar, [line: 4, column: 1], Elixir},
                   {:is_number, [line: 4, column: 10], [{:bar, [line: 4, column: 20], Elixir}]}
                 ]}
              ],
              :ok
            ]}
         ]},
        {~s'''
         def foo(bar) when is_binary(bar) do
           :ok
         end
         ''',
         {:def, [line: 1, column: 1],
          [
            {:when, [line: 1, column: 14],
             [
               {:foo, [line: 1, column: 5], [{:bar, [line: 1, column: 9], Elixir}]},
               {:is_binary, [line: 1, column: 19], [{:bar, [line: 1, column: 29], Elixir}]}
             ]},
            [do: :ok]
          ]}},
        {~s'''
         fn foo when is_binary(foo) ->
           :ok
         end
         ''',
         {:fn, [line: 1, column: 1],
          [
            {:->, [depth: 1, line: 1, column: 28],
             [
               [
                 {:when, [line: 1, column: 8],
                  [
                    {:foo, [line: 1, column: 4], Elixir},
                    {:is_binary, [line: 1, column: 13], [{:foo, [line: 1, column: 23], Elixir}]}
                  ]}
               ],
               :ok
             ]}
          ]}},
        {~s'''
         fn foo, bar, _baz when is_binary(foo) and bar in [:alice, :bob] ->
           :ok
         end
         ''',
         {:fn, [line: 1, column: 1],
          [
            {:->, [depth: 1, line: 1, column: 65],
             [
               [
                 {:when, [line: 1, column: 19],
                  [
                    {:foo, [line: 1, column: 4], Elixir},
                    {:bar, [line: 1, column: 9], Elixir},
                    {:_baz, [line: 1, column: 14], Elixir},
                    {:and, [line: 1, column: 39],
                     [
                       {:is_binary, [line: 1, column: 24], [{:foo, [line: 1, column: 34], Elixir}]},
                       {:in, [line: 1, column: 47], [{:bar, [line: 1, column: 43], Elixir}, [:alice, :bob]]}
                     ]}
                  ]}
               ],
               :ok
             ]}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "capture operator" do
      codes = [
        {~s'''
         &foo/1
         ''', {:&, [line: 1, column: 1], [{:/, [line: 1, column: 5], [{:foo, [line: 1, column: 2], Elixir}, 1]}]}},
        {~s'''
         &Foo.foo/1
         ''',
         {:&, [line: 1, column: 1],
          [{:/, [line: 1, column: 9], [{{:., [], [{:__aliases__, [line: 1, column: 2], [:Foo]}, :foo]}, [], []}, 1]}]}},
        {~s'''
         & &1
         ''', {:&, [line: 1, column: 1], [{:&, [line: 1, column: 3], [1]}]}},
        {~s'''
         &Foo.bar(one, &1)
         ''',
         {:&, [line: 1, column: 1],
          [
            {{:., [], [{:__aliases__, [line: 1, column: 2], [:Foo]}, :bar]}, [],
             [{:one, [line: 1, column: 10], Elixir}, {:&, [line: 1, column: 15], [1]}]}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
    end

    test "anonymous function function calls" do
      codes = [
        {~s'''
         foo.()
         ''', {{:., [], [{:foo, [], Elixir}]}, [], []}},
        {~s'''
         foo.(one, two)
         ''', {{:., [], [{:foo, [], Elixir}]}, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
        {~s'''
         foo.(
           one,
           two
         )
         ''', {{:., [], [{:foo, [], Elixir}]}, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
        {~s'''
         infix.(next_token(parser), left)
         ''', {{:., [], [{:infix, [], Elixir}]}, [], [{:next_token, [], [{:parser, [], Elixir}]}, {:left, [], Elixir}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse!(code) == expected
      end
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

      assert Spitfire.parse!(code) ==
               {:if, [],
                [
                  {:==, [], [{:prefix, [], Elixir}, nil]},
                  [
                    do:
                      {:__block__, [],
                       [
                         {:=, [],
                          [
                            {{:row, [], Elixir}, {:col, [], Elixir}},
                            {:token_loc, [],
                             [
                               {{:., [], [{:parser, [], Elixir}, :current_token]}, [], []}
                             ]}
                          ]},
                         {{:., [], [{:__aliases__, [], [:IO]}, :puts]}, [],
                          [
                            {:<>, [],
                             [
                               {{:., [], [{:__aliases__, [], [:IO, :ANSI]}, :red]}, [], []},
                               {:<>, [],
                                [
                                  {:<<>>, [],
                                   [
                                     {:"::", [],
                                      [
                                        {{:., [], [Kernel, :to_string]}, [], [{:row, [], Elixir}]},
                                        {:binary, [], Elixir}
                                      ]},
                                     ":",
                                     {:"::", [],
                                      [
                                        {{:., [], [Kernel, :to_string]}, [], [{:col, [], Elixir}]},
                                        {:binary, [], Elixir}
                                      ]},
                                     ": unknown prefix: ",
                                     {:"::", [],
                                      [
                                        {{:., [], [Kernel, :to_string]}, [],
                                         [{:current_token_type, [], [{:parser, [], Elixir}]}]},
                                        {:binary, [], Elixir}
                                      ]}
                                   ]},
                                  {{:., [], [{:__aliases__, [], [:IO, :ANSI]}, :reset]}, [], []}
                                ]}
                             ]}
                          ]},
                         {:error, {:next_token, [], [{:parser, [], Elixir}]}}
                       ]},
                    else:
                      {:__block__, [],
                       [
                         {:=, [],
                          [
                            {{:left, [], Elixir}, {:parser, [], Elixir}},
                            {{:., [], [{:prefix, [], Elixir}]}, [], [{:parser, [], Elixir}]}
                          ]},
                         {:=, [],
                          [
                            {:calc_prec, [], Elixir},
                            {:fn, [],
                             [
                               {:->, [depth: 2],
                                [
                                  [{:parser, [], Elixir}],
                                  {:__block__, [],
                                   [
                                     {:=, [],
                                      [
                                        {{:_associativity, [], Elixir}, {:power, [], Elixir}},
                                        {:peek_precedence, [], [{:parser, [], Elixir}]}
                                      ]},
                                     {:=, [],
                                      [
                                        {:precedence, [], Elixir},
                                        {:case, [],
                                         [
                                           {:associativity, [], Elixir},
                                           [
                                             do: [
                                               {:->, [depth: 3], [[:left], {:precedence, [], Elixir}]},
                                               {:->, [depth: 3], [[:unassoc], 0]},
                                               {:->, [depth: 3],
                                                [
                                                  [:right],
                                                  {:-, [], [{:precedence, [], Elixir}, 1]}
                                                ]}
                                             ]
                                           ]
                                         ]}
                                      ]},
                                     {:<, [], [{:precedence, [], Elixir}, {:power, [], Elixir}]}
                                   ]}
                                ]}
                             ]}
                          ]},
                         {:=, [], [{:terminals, [], Elixir}, [:eol, :eof, :"}", :")", :"]"]]},
                         {:=, [],
                          [
                            {:terminals, [], Elixir},
                            {:if, [],
                             [
                               {:is_top, [], Elixir},
                               [
                                 do: {:terminals, [], Elixir},
                                 else: [{:|, [], [:",", {:terminals, [], Elixir}]}]
                               ]
                             ]}
                          ]},
                         {:while, [],
                          [
                            {:<-, [],
                             [
                               {:&&, [],
                                [
                                  {:not, [],
                                   [
                                     {:in, [],
                                      [
                                        {:peek_token, [], [{:parser, [], Elixir}]},
                                        {:terminals, [], Elixir}
                                      ]}
                                   ]},
                                  {{:., [], [{:calc_prec, [], Elixir}]}, [], [{:parser, [], Elixir}]}
                                ]},
                               {{:left, [], Elixir}, {:parser, [], Elixir}}
                             ]},
                            [
                              do:
                                {:__block__, [],
                                 [
                                   {:=, [],
                                    [
                                      {:infix, [], Elixir},
                                      {:case, [],
                                       [
                                         {:peek_token_type, [], [{:parser, [], Elixir}]},
                                         [
                                           do: [
                                             {:->, [depth: 3],
                                              [
                                                [:match_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:when_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:pipe_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:dual_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:mult_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:concat_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:assoc_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_assoc_op, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:arrow_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:ternary_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:or_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:and_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:comp_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:rel_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:in_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:xor_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:in_match_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:range_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_range_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:stab_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_stab_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:do],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_do_block, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:dot_call_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_dot_call_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:.],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_dot_expression, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [{:when, [], [:",", {:is_top, [], Elixir}]}],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_comma, [], Elixir}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3], [[{:_, [], Elixir}], nil]}
                                           ]
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:do_block, [], Elixir},
                                      {:&, [],
                                       [
                                         {:/, [], [{:parse_do_block, [], Elixir}, 2]}
                                       ]}
                                    ]},
                                   {:case, [],
                                    [
                                      {:infix, [], Elixir},
                                      [
                                        do: [
                                          {:->, [depth: 3], [[nil], {{:left, [], Elixir}, {:parser, [], Elixir}}]},
                                          {:->, [depth: 3],
                                           [
                                             [
                                               {:when, [],
                                                [
                                                  {:^, [], [{:do_block, [], Elixir}]},
                                                  {:!=, [],
                                                   [
                                                     {{:., [], [{:parser, [], Elixir}, :nestings]}, [], []},
                                                     []
                                                   ]}
                                                ]}
                                             ],
                                             {{:left, [], Elixir}, {:next_token, [], [{:parser, [], Elixir}]}}
                                           ]},
                                          {:->, [depth: 3],
                                           [
                                             [{:_, [], Elixir}],
                                             {{:., [], [{:infix, [], Elixir}]}, [],
                                              [
                                                {:next_token, [], [{:parser, [], Elixir}]},
                                                {:left, [], Elixir}
                                              ]}
                                           ]}
                                        ]
                                      ]
                                    ]}
                                 ]}
                            ]
                          ]}
                       ]}
                  ]
                ]}
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

      assert Spitfire.parse!(code) ==
               {:def, [],
                [
                  {:bar, [], [{:foo, [], Elixir}]},
                  [
                    do:
                      {:case, [],
                       [
                         {:foo, [], Elixir},
                         [
                           do: [
                             {:->, [depth: 2], [[:foo], :ok]},
                             {:->, [depth: 2],
                              [
                                [:bar],
                                {{:., [], [{:__aliases__, [], [:Enum]}, :map]}, [],
                                 [
                                   {:some_list, [], Elixir},
                                   {:fn, [],
                                    [
                                      {:->, [depth: 3],
                                       [
                                         [{:item, [], Elixir}],
                                         {{:., [], [{:item, [], Elixir}, :name]}, [], []}
                                       ]}
                                    ]}
                                 ]}
                              ]}
                           ]
                         ]
                       ]}
                  ]
                ]}
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

      assert Spitfire.parse!(code) ==
               {:case, [],
                [
                  {:foo, [], Elixir},
                  [
                    do: [
                      {:->, [depth: 1], [[:foo], :ok]},
                      {:->, [depth: 1],
                       [
                         [:bar],
                         {{:., [], [{:__aliases__, [], [:Enum]}, :map]}, [],
                          [
                            {:some_list, [], Elixir},
                            {:fn, [],
                             [
                               {:->, [depth: 2],
                                [
                                  [{:item, [], Elixir}],
                                  {{:., [], [{:item, [], Elixir}, :name]}, [], []}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]
                  ]
                ]}
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

      assert Spitfire.parse!(code) ==
               {:defp, [],
                [
                  {:parse_stab_expression, [], [{:parser, [], Elixir}, {:lhs, [], Elixir}]},
                  [
                    do:
                      {:case, [],
                       [
                         {:current_token, [], [{:parser, [], Elixir}]},
                         [
                           do: [
                             {:->, [depth: 2],
                              [
                                [:<-],
                                {:parse_infix_expression, [], [{:parser, [], Elixir}, {:lhs, [], Elixir}]}
                              ]},
                             {:->, [depth: 2],
                              [
                                [:->],
                                {:__block__, [],
                                 [
                                   {:=, [],
                                    [
                                      {:token, [], Elixir},
                                      {:current_token, [], [{:parser, [], Elixir}]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:current_sd, [], Elixir},
                                      {{:., [], [{:parser, [], Elixir}, :stab_depth]}, [], []}
                                    ]},
                                   {:=, [],
                                    [
                                      {:parser, [], Elixir},
                                      {:eat_at, [], [{:parser, [], Elixir}, :eol, 1]}
                                    ]},
                                   {:=, [], [{:exprs, [], Elixir}, []]},
                                   {:=, [],
                                    [
                                      {{:exprs, [], Elixir}, {:parser, [], Elixir}},
                                      {:while, [],
                                       [
                                         {:<-, [],
                                          [
                                            {:not, [],
                                             [
                                               {:in, [],
                                                [
                                                  {:peek_token, [], [{:parser, [], Elixir}]},
                                                  [:eof, :end]
                                                ]}
                                             ]},
                                            {{:exprs, [], Elixir}, {:parser, [], Elixir}}
                                          ]},
                                         [
                                           do:
                                             {:__block__, [],
                                              [
                                                {:=, [],
                                                 [
                                                   {:parser, [], Elixir},
                                                   {:next_token, [], [{:parser, [], Elixir}]}
                                                 ]},
                                                {:=, [],
                                                 [
                                                   {{:ast, [], Elixir}, {:parser, [], Elixir}},
                                                   {:parse_expression, [], [{:parser, [], Elixir}, [top: true]]}
                                                 ]},
                                                {:=, [],
                                                 [
                                                   {:parser, [], Elixir},
                                                   {:eat_at, [], [{:parser, [], Elixir}, :eol, 1]}
                                                 ]},
                                                {[
                                                   {:|, [], [{:ast, [], Elixir}, {:exprs, [], Elixir}]}
                                                 ], {:eat_eol, [], [{:parser, [], Elixir}]}}
                                              ]}
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:rhs, [], Elixir},
                                      {:case, [],
                                       [
                                         {:exprs, [], Elixir},
                                         [
                                           do: [
                                             {:->, [depth: 3], [[[{:ast, [], Elixir}]], {:ast, [], Elixir}]},
                                             {:->, [depth: 3],
                                              [
                                                [{:exprs, [], Elixir}],
                                                {:{}, [],
                                                 [
                                                   :__block__,
                                                   [],
                                                   {{:., [],
                                                     [
                                                       {:__aliases__, [], [:Enum]},
                                                       :reverse
                                                     ]}, [], [{:exprs, [], Elixir}]}
                                                 ]}
                                              ]}
                                           ]
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {{:rhs, [], Elixir}, {:stabs, [], Elixir}},
                                      {{:., [], [{:__aliases__, [], [:Macro]}, :traverse]}, [],
                                       [
                                         {:rhs, [], Elixir},
                                         [],
                                         {:fn, [],
                                          [
                                            {:->, [depth: 3],
                                             [
                                               [
                                                 {:node, [], Elixir},
                                                 {:acc, [], Elixir}
                                               ],
                                               {:case, [],
                                                [
                                                  {:node, [], Elixir},
                                                  [
                                                    do: [
                                                      {:->, [depth: 4],
                                                       [
                                                         [
                                                           {:{}, [],
                                                            [
                                                              :->,
                                                              {:meta, [], Elixir},
                                                              {:_args, [], Elixir}
                                                            ]}
                                                         ],
                                                         {:if, [],
                                                          [
                                                            {:==, [],
                                                             [
                                                               {{:., [], [Access, :get]}, [],
                                                                [{:meta, [], Elixir}, :depth]},
                                                               {:current_sd, [], Elixir}
                                                             ]},
                                                            [
                                                              do:
                                                                {:__remove_me__,
                                                                 [
                                                                   {:|, [],
                                                                    [
                                                                      {:node, [], Elixir},
                                                                      {:acc, [], Elixir}
                                                                    ]}
                                                                 ]},
                                                              else: {{:node, [], Elixir}, {:acc, [], Elixir}}
                                                            ]
                                                          ]}
                                                       ]},
                                                      {:->, [depth: 4],
                                                       [
                                                         [{:_, [], Elixir}],
                                                         {{:node, [], Elixir}, {:acc, [], Elixir}}
                                                       ]}
                                                    ]
                                                  ]
                                                ]}
                                             ]}
                                          ]},
                                         {:fn, [],
                                          [
                                            {:->, [depth: 3],
                                             [
                                               [
                                                 {:when, [],
                                                  [
                                                    {:{}, [],
                                                     [
                                                       {:node, [], Elixir},
                                                       {:meta, [], Elixir},
                                                       {:args, [], Elixir}
                                                     ]},
                                                    {:acc, [], Elixir},
                                                    {:is_list, [], [{:args, [], Elixir}]}
                                                  ]}
                                               ],
                                               {:__block__, [],
                                                [
                                                  {:=, [],
                                                   [
                                                     {:args, [], Elixir},
                                                     {{:., [],
                                                       [
                                                         {:__aliases__, [], [:Enum]},
                                                         :reject
                                                       ]}, [],
                                                      [
                                                        {:args, [], Elixir},
                                                        {:&, [],
                                                         [
                                                           {:&&, [],
                                                            [
                                                              {:is_list, [], [{:&, [], [1]}]},
                                                              {{:., [],
                                                                [
                                                                  {:__aliases__, [], [:Enum]},
                                                                  :member?
                                                                ]}, [], [{:&, [], [1]}, :__remove_me__]}
                                                            ]}
                                                         ]}
                                                      ]}
                                                   ]},
                                                  {{:{}, [],
                                                    [
                                                      {:node, [], Elixir},
                                                      {:meta, [], Elixir},
                                                      {:args, [], Elixir}
                                                    ]}, {:acc, [], Elixir}}
                                                ]}
                                             ]},
                                            {:->, [depth: 3],
                                             [
                                               [
                                                 {:node, [], Elixir},
                                                 {:acc, [], Elixir}
                                               ],
                                               {{:node, [], Elixir}, {:acc, [], Elixir}}
                                             ]}
                                          ]}
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:rhs, [], Elixir},
                                      {:case, [],
                                       [
                                         {:rhs, [], Elixir},
                                         [
                                           do: [
                                             {:->, [depth: 3],
                                              [
                                                [
                                                  {:{}, [],
                                                   [
                                                     :__block__,
                                                     {:_, [], Elixir},
                                                     [{:ast, [], Elixir}]
                                                   ]}
                                                ],
                                                {:ast, [], Elixir}
                                              ]},
                                             {:->, [depth: 3], [[[{:ast, [], Elixir}]], {:ast, [], Elixir}]},
                                             {:->, [depth: 3], [[{:block, [], Elixir}], {:block, [], Elixir}]}
                                           ]
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:ast, [], Elixir},
                                      {:++, [],
                                       [
                                         [
                                           {:{}, [],
                                            [
                                              {:token, [], Elixir},
                                              [
                                                depth: {{:., [], [{:parser, [], Elixir}, :stab_depth]}, [], []}
                                              ],
                                              [
                                                {:wrap, [], [{:lhs, [], Elixir}]},
                                                {:rhs, [], Elixir}
                                              ]
                                            ]}
                                         ],
                                         {{:., [], [{:__aliases__, [], [:Enum]}, :reverse]}, [], [{:stabs, [], Elixir}]}
                                       ]}
                                    ]},
                                   {{:ast, [], Elixir}, {:eat_eol, [], [{:parser, [], Elixir}]}}
                                 ]}
                              ]}
                           ]
                         ]
                       ]}
                  ]
                ]}
    end
  end

  # INFO: the above describe block uses a new implementation of `==` that deletes all the `meta` fields in the AST, to make it easy
  # to bootstrap the original structure. As more metadata is added, we want to move them to the describe below so that all the
  # meta is properly tested
  describe "with original ==" do
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

      assert Spitfire.parse(code) ==
               {:ok,
                {:with, [do: [line: 5, column: 13], end: [line: 7, column: 1], line: 1, column: 1],
                 [
                   {:<-, [line: 1, column: 15],
                    [
                      {:ok, {:_, [line: 1, column: 12], Elixir}},
                      {:bar, [closing: [line: 5, column: 11], line: 1, column: 18],
                       [
                         {:fn, [closing: [line: 5, column: 8], line: 1, column: 22],
                          [
                            {:->, [newlines: 1, depth: 1, line: 1, column: 27],
                             [
                               [{:a, [line: 1, column: 25], Elixir}],
                               {:with,
                                [
                                  do: [line: 2, column: 23],
                                  end: [line: 4, column: 10],
                                  line: 2,
                                  column: 10
                                ],
                                [
                                  {:<-, [line: 2, column: 18], [:d, {:b, [line: 2, column: 21], Elixir}]},
                                  [do: :f]
                                ]}
                             ]}
                          ]}
                       ]}
                    ]},
                   [do: :ok]
                 ]}}
    end

    test "bitstrings" do
      code = ~S'<<?., char, rest::binary>>'

      assert Spitfire.parse(code) ==
               {:ok,
                {:<<>>, [closing: [line: 1, column: 25], line: 1, column: 1],
                 [
                   46,
                   {:char, [line: 1, column: 7], Elixir},
                   {:"::", [line: 1, column: 17],
                    [
                      {:rest, [line: 1, column: 13], Elixir},
                      {:binary, [line: 1, column: 19], Elixir}
                    ]}
                 ]}}
    end

    test "anonymous function typespecs" do
      code = ~S'''
      @spec start_link((-> term), GenServer.options()) :: on_start
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {:@, [line: 1, column: 1],
                 [
                   {:spec, [line: 1, column: 2],
                    [
                      {:"::", [line: 1, column: 50],
                       [
                         {:start_link, [closing: [line: 1, column: 48], line: 1, column: 7],
                          [
                            [
                              {:->, [line: 1, column: 19], [[], {:term, [line: 1, column: 22], Elixir}]}
                            ],
                            {{:., [line: 1, column: 38],
                              [
                                {:__aliases__, [last: [line: 1, column: 29], line: 1, column: 29], [:GenServer]},
                                :options
                              ]}, [closing: [line: 1, column: 47], line: 1, column: 39], []}
                          ]},
                         {:on_start, [line: 1, column: 53], Elixir}
                       ]}
                    ]}
                 ]}}

      code = ~S'''
      @spec get(agent, (state -> a), timeout) :: a when a: var
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {:@, [line: 1, column: 1],
                 [
                   {:spec, [line: 1, column: 2],
                    [
                      {:when, [line: 1, column: 46],
                       [
                         {:"::", [line: 1, column: 41],
                          [
                            {:get, [closing: [line: 1, column: 39], line: 1, column: 7],
                             [
                               {:agent, [line: 1, column: 11], Elixir},
                               [
                                 {:->, [depth: 0, line: 1, column: 25],
                                  [
                                    [{:state, [line: 1, column: 19], Elixir}],
                                    {:a, [line: 1, column: 28], Elixir}
                                  ]}
                               ],
                               {:timeout, [line: 1, column: 32], Elixir}
                             ]},
                            {:a, [line: 1, column: 44], Elixir}
                          ]},
                         [a: {:var, [line: 1, column: 54], Elixir}]
                       ]}
                    ]}
                 ]}}
    end

    test "rescue with def" do
      code = ~S'''
      def foo(%mod{} = bar) do
        :ok
      end
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {:def, [do: [line: 1, column: 23], end: [line: 3, column: 1], line: 1, column: 1],
                 [
                   {:foo, [closing: [line: 1, column: 21], line: 1, column: 5],
                    [
                      {:=, [line: 1, column: 16],
                       [
                         {:%, [line: 1, column: 9],
                          [
                            {:mod, [line: 1, column: 10], Elixir},
                            {:%{}, [closing: [line: 1, column: 14], line: 1, column: 13], []}
                          ]},
                         {:bar, [line: 1, column: 18], Elixir}
                       ]}
                    ]},
                   [do: :ok]
                 ]}}
    end

    test "default args" do
      code = ~S'''
      def foo(arg \\ :value) do
        :ok
      end
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {:def, [do: [line: 1, column: 24], end: [line: 3, column: 1], line: 1, column: 1],
                 [
                   {:foo, [closing: [line: 1, column: 22], line: 1, column: 5],
                    [{:\\, [line: 1, column: 13], [{:arg, [line: 1, column: 9], Elixir}, :value]}]},
                   [do: :ok]
                 ]}}
    end

    test "literal encoder" do
      codes = [
        {~S'''
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
         {:__block__, [],
          [
            {:__literal__,
             [
               end_of_expression: [newlines: 1, line: 1, column: 2],
               token: "1",
               line: 1,
               column: 1
             ], 1},
            {:__literal__,
             [
               end_of_expression: [newlines: 1, line: 2, column: 6],
               delimiter: "\"",
               line: 2,
               column: 1
             ], "two"},
            {:__literal__, [end_of_expression: [newlines: 1, line: 3, column: 7], line: 3, column: 1], :three},
            {:__literal__,
             [
               end_of_expression: [newlines: 1, line: 4, column: 7],
               closing: [line: 4, column: 6],
               line: 4,
               column: 1
             ], [{:four, [line: 4, column: 2], Elixir}]},
            {:try, [do: [line: 5, column: 5], end: [line: 10, column: 1], line: 5, column: 1],
             [
               [
                 {{:__literal__, [line: 5, column: 5], :do}, {:__literal__, [line: 6, column: 3], :ok}},
                 {{:__literal__, [line: 7, column: 1], :rescue},
                  [
                    {:->, [newlines: 1, depth: 1, line: 8, column: 5],
                     [
                       [{:_, [line: 8, column: 3], Elixir}],
                       {:__literal__, [line: 9, column: 5], :error}
                     ]}
                  ]}
               ]
             ]}
          ]}},
        {~S'1.752', {:__literal__, [token: "1.752", line: 1, column: 1], 1.752}},
        {~S'0xABCD', {:__literal__, [token: "0xABCD", line: 1, column: 1], 43_981}},
        {~S'0o01234567', {:__literal__, [token: "0o01234567", line: 1, column: 1], 342_391}},
        {~S'0b10101010', {:__literal__, [token: "0b10101010", line: 1, column: 1], 170}},
        {~S'?é', {:__literal__, [token: "?é", line: 1, column: 1], 233}},
        {~S'"foo"', {:__literal__, [delimiter: "\"", line: 1, column: 1], "foo"}},
        {~S"'foo'", {:__literal__, [delimiter: "'", line: 1, column: 1], ~c"foo"}},
        {~S':"foo"', {:__literal__, [delimiter: "\"", line: 1, column: 1], :foo}},
        {~S":foo", {:__literal__, [line: 1, column: 1], :foo}},
        {~S'''
         """
         foo
         """
         ''', {:__literal__, [delimiter: "\"\"\"", indentation: 0, line: 1, column: 1], "foo\n"}},
        {~S"""
         '''
         foo
         '''
         """, {:__literal__, [delimiter: "'''", indentation: 0, line: 1, column: 1], ~c"foo\n"}},
        {~S'{one, two}',
         {:__literal__, [closing: [line: 1, column: 10], line: 1, column: 1],
          {{:one, [line: 1, column: 2], Elixir}, {:two, [line: 1, column: 7], Elixir}}}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code, literal_encoder: fn l, m -> {:ok, {:__literal__, m, l}} end) == {:ok, expected}
      end
    end

    test "sigils" do
      codes = [
        {~S'~s"foo"', {:sigil_s, [delimiter: "\"", line: 1, column: 1], [{:<<>>, [line: 1, column: 1], ["foo"]}, []]}},
        {~S'~s"foo"bar',
         {:sigil_s, [delimiter: "\"", line: 1, column: 1], [{:<<>>, [line: 1, column: 1], ["foo"]}, ~c"bar"]}},
        {~S'~s"hello#{world}"bar',
         {:sigil_s, [delimiter: "\"", line: 1, column: 1],
          [
            {:<<>>, [line: 1, column: 1],
             [
               "hello",
               {:"::", [line: 1, column: 9],
                [
                  {{:., [line: 1, column: 9], [Kernel, :to_string]},
                   [
                     from_interpolation: true,
                     line: 1,
                     column: 9
                   ], [{:world, [line: 1, column: 11], Elixir}]},
                  {:binary, [line: 1, column: 9], Elixir}
                ]}
             ]},
            ~c"bar"
          ]}},
        {~S'~S"hello#{world}"bar',
         {:sigil_S, [delimiter: "\"", line: 1, column: 1], [{:<<>>, [line: 1, column: 1], ["hello\#{world}"]}, ~c"bar"]}},
        {~S'~S|hello#{world}|bar',
         {:sigil_S, [delimiter: "|", line: 1, column: 1], [{:<<>>, [line: 1, column: 1], ["hello\#{world}"]}, ~c"bar"]}},
        {~S'''
           ~S"""
         hello world
           """
         ''',
         {:sigil_S, [delimiter: "\"\"\"", line: 1, column: 3],
          [{:<<>>, [indentation: 2, line: 1, column: 3], ["hello world\n"]}, []]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
      end
    end

    test "parses string interpolation" do
      code = ~S'''
      "foo#{alice}bar"
      '''

      assert Spitfire.parse!(code) ==
               {:<<>>, [delimiter: "\"", line: 1, column: 1],
                [
                  "foo",
                  {:"::", [line: 1, column: 5],
                   [
                     {{:., [line: 1, column: 5], [Kernel, :to_string]},
                      [
                        from_interpolation: true,
                        closing: [line: 1, column: 12],
                        line: 1,
                        column: 5
                      ], [{:alice, [line: 1, column: 7], Elixir}]},
                     {:binary, [line: 1, column: 5], Elixir}
                   ]},
                  "bar"
                ]}

      code = ~S'''
      "foo#{}bar"
      '''

      assert Spitfire.parse!(code) ==
               {:<<>>, [delimiter: "\"", line: 1, column: 1],
                [
                  "foo",
                  {:"::", [line: 1, column: 5],
                   [
                     {{:., [line: 1, column: 5], [Kernel, :to_string]},
                      [
                        from_interpolation: true,
                        closing: [line: 1, column: 7],
                        line: 1,
                        column: 5
                      ], [{:__block__, [], []}]},
                     {:binary, [line: 1, column: 5], Elixir}
                   ]},
                  "bar"
                ]}

      code = ~S'''
      """
      foo#{alice}bar
      """
      '''

      assert Spitfire.parse!(code) ==
               {:<<>>, [delimiter: "\"\"\"", indentation: 0, line: 1, column: 1],
                [
                  "foo",
                  {:"::", [line: 2, column: 4],
                   [
                     {{:., [line: 2, column: 4], [Kernel, :to_string]},
                      [
                        from_interpolation: true,
                        closing: [line: 2, column: 11],
                        line: 2,
                        column: 4
                      ], [{:alice, [line: 2, column: 6], Elixir}]},
                     {:binary, [line: 2, column: 4], Elixir}
                   ]},
                  "bar\n"
                ]}

      code = ~S'''
      "#{foo}"
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {:<<>>, [delimiter: "\"", line: 1, column: 1],
                 [
                   {:"::", [line: 1, column: 2],
                    [
                      {{:., [line: 1, column: 2], [Kernel, :to_string]},
                       [
                         from_interpolation: true,
                         closing: [line: 1, column: 7],
                         line: 1,
                         column: 2
                       ], [{:foo, [line: 1, column: 4], Elixir}]},
                      {:binary, [line: 1, column: 2], Elixir}
                    ]}
                 ]}}
    end

    test "end of expression metadata" do
      codes = [
        {~S'''
         foo do
           Some.thing(
             bar
           )
           Some.thing_else!()
         end
         ''',
         {:foo, [do: [line: 1, column: 5], end: [line: 6, column: 1], line: 1, column: 1],
          [
            [
              do:
                {:__block__, [],
                 [
                   {{:., [line: 2, column: 7],
                     [
                       {:__aliases__, [{:last, [line: 2, column: 3]}, line: 2, column: 3], [:Some]},
                       :thing
                     ]},
                    [
                      end_of_expression: [newlines: 1, line: 4, column: 4],
                      closing: [line: 4, column: 3],
                      line: 2,
                      column: 8
                    ], [{:bar, [line: 3, column: 5], Elixir}]},
                   {{:., [line: 5, column: 7],
                     [
                       {:__aliases__, [{:last, [line: 5, column: 3]}, line: 5, column: 3], [:Some]},
                       :thing_else!
                     ]}, [{:closing, [line: 5, column: 20]}, line: 5, column: 8], []}
                 ]}
            ]
          ]}},
        {~S'''
         fn foo ->
           send foo, :hi

           :ok
         end
         ''',
         {:fn, [closing: [line: 5, column: 1], line: 1, column: 1],
          [
            {:->, [newlines: 1, depth: 1, line: 1, column: 8],
             [
               [{:foo, [line: 1, column: 4], Elixir}],
               {:__block__, [],
                [
                  {:send,
                   [
                     end_of_expression: [newlines: 2, line: 2, column: 16],
                     line: 2,
                     column: 3
                   ], [{:foo, [line: 2, column: 8], Elixir}, :hi]},
                  :ok
                ]}
             ]}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
      end
    end

    test "closing metadata" do
      codes = [
        {~S'{}', {:{}, [closing: [line: 1, column: 2], line: 1, column: 1], []}},
        {~S'{one, two, three}',
         {:{}, [closing: [line: 1, column: 17], line: 1, column: 1],
          [
            {:one, [line: 1, column: 2], Elixir},
            {:two, [line: 1, column: 7], Elixir},
            {:three, [line: 1, column: 12], Elixir}
          ]}},
        {~S'%{}', {:%{}, [closing: [line: 1, column: 3], line: 1, column: 2], []}},
        {~S'%{"one" => two, three: 4}',
         {:%{}, [closing: [line: 1, column: 25], line: 1, column: 2],
          [{"one", {:two, [line: 1, column: 12], Elixir}}, {:three, 4}]}},
        {~S'foo()', {:foo, [closing: [line: 1, column: 5], line: 1, column: 1], []}},
        {~S'foo(bar)',
         {:foo, [closing: [line: 1, column: 8], line: 1, column: 1], [{:bar, [line: 1, column: 5], Elixir}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
      end
    end

    test "parses special keywords" do
      codes = [
        {"__MODULE__", {:__MODULE__, [line: 1, column: 1], Elixir}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
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

      assert Spitfire.parse(code) ==
               {
                 :ok,
                 {
                   :defmodule,
                   [do: [line: 1, column: 15], end: [line: 16, column: 1], line: 1, column: 1],
                   [
                     {:__aliases__, [{:last, [line: 1, column: 11]}, {:line, 1}, {:column, 11}], [:Foo]},
                     [
                       do: {
                         :__block__,
                         [],
                         [
                           {:defstruct, [end_of_expression: [newlines: 2, line: 2, column: 30], line: 2, column: 3],
                            [[:foo, {:bar, "yo"}]]},
                           {
                             :defmodule,
                             [
                               end_of_expression: [newlines: 2, line: 10, column: 6],
                               do: [line: 4, column: 19],
                               end: [line: 10, column: 3],
                               line: 4,
                               column: 3
                             ],
                             [
                               {:__aliases__, [{:last, [line: 4, column: 13]}, {:line, 4}, {:column, 13}], [:State]},
                               [
                                 do: {
                                   :__block__,
                                   [],
                                   [
                                     {:defstruct,
                                      [end_of_expression: [newlines: 2, line: 5, column: 20], line: 5, column: 5],
                                      [[:yo]]},
                                     {
                                       :def,
                                       [do: [line: 7, column: 20], end: [line: 9, column: 5], line: 7, column: 5],
                                       [
                                         {:new, [closing: [line: 7, column: 18], line: 7, column: 9],
                                          [{:attrs, [line: 7, column: 13], Elixir}]},
                                         [
                                           do: {
                                             :struct,
                                             [closing: [line: 8, column: 34], line: 8, column: 7],
                                             [
                                               {
                                                 :%,
                                                 [line: 8, column: 14],
                                                 [
                                                   {:__MODULE__, [line: 8, column: 15], Elixir},
                                                   {:%{}, [closing: [line: 8, column: 26], line: 8, column: 25], []}
                                                 ]
                                               },
                                               {:attrs, [line: 8, column: 29], Elixir}
                                             ]
                                           }
                                         ]
                                       ]
                                     }
                                   ]
                                 }
                               ]
                             ]
                           },
                           {
                             :@,
                             [end_of_expression: [newlines: 1, line: 12, column: 47], line: 12, column: 3],
                             [
                               {
                                 :spec,
                                 [line: 12, column: 4],
                                 [
                                   {
                                     :"::",
                                     [line: 12, column: 34],
                                     [
                                       {
                                         :run,
                                         [closing: [line: 12, column: 32], line: 12, column: 9],
                                         [
                                           {:any, [closing: [line: 12, column: 17], line: 12, column: 13], []},
                                           {:any, [closing: [line: 12, column: 24], line: 12, column: 20], []},
                                           {:any, [closing: [line: 12, column: 31], line: 12, column: 27], []}
                                         ]
                                       },
                                       :something
                                     ]
                                   }
                                 ]
                               }
                             ]
                           },
                           {
                             :def,
                             [do: [line: 13, column: 26], end: [line: 15, column: 3], line: 13, column: 3],
                             [
                               {
                                 :run,
                                 [closing: [line: 13, column: 24], line: 13, column: 7],
                                 [
                                   {:foo, [line: 13, column: 11], Elixir},
                                   {:bar, [line: 13, column: 16], Elixir},
                                   {:baz, [line: 13, column: 21], Elixir}
                                 ]
                               },
                               [do: :something]
                             ]
                           }
                         ]
                       }
                     ]
                   ]
                 }
               }
    end
  end

  describe "code with errors" do
    # TODO: this needs a change to the tokenizer i believe, or a way to splice out the unknown token
    @tag :skip
    test "unknown prefix operator" do
      code = "foo $bar, baz"

      assert Spitfire.parse(code) ==
               {:error, {:foo, [line: 1, column: 1], [{:__error__, [line: 1, column: 5], ["unknown token: %"]}]},
                [{[line: 1, column: 5], "unknown token: %"}]}
    end

    test "missing closing parentheses" do
      code = "1 * (2 + 3"

      assert Spitfire.parse(code) ==
               {:error,
                {:*, [line: 1, column: 3], [1, {:__error__, [line: 1, column: 10], ["missing closing parentheses"]}]},
                [{[line: 1, column: 10], "missing closing parentheses"}]}
    end

    test "missing closing list bracket" do
      code = "([1, 2 ++ [4])"

      assert Spitfire.parse(code) ==
               {:error, [1, {:++, [line: 1, column: 8], [2, [4]]}],
                [{[line: 1, column: 2], "missing closing bracket for list"}]}
    end

    test "missing closing tuple brace" do
      code = "({1, 2 ++ [4])"

      assert Spitfire.parse(code) ==
               {:error, {1, {:++, [line: 1, column: 8], [2, [4]]}},
                [{[line: 1, column: 2], "missing closing brace for tuple"}]}
    end

    test "missing closing map brace" do
      code = ~S'foo(%{alice: "bob")'

      assert Spitfire.parse(code) ==
               {:error,
                {:foo, [{:closing, [line: 1, column: 19]}, line: 1, column: 1],
                 [{:%{}, [{:closing, [line: 1, column: 14]}, line: 1, column: 6], [alice: "bob"]}]},
                [{[line: 1, column: 14], "missing closing brace for map"}]}
    end

    test "missing comma in list" do
      code = ~S'[:foo :bar, :baz]'

      assert Spitfire.parse(code) == {:error, [:foo, :baz], [{[line: 1, column: 7], "syntax error"}]}
    end

    test "missing comma in map" do
      code = ~S'%{foo: :bar baz: :boo}'

      assert Spitfire.parse(code) ==
               {:error, {:%{}, [{:closing, [line: 1, column: 22]}, line: 1, column: 2], [foo: :bar]},
                [
                  {[line: 1, column: 13], "syntax error"},
                  {[line: 1, column: 18], "syntax error"}
                ]}
    end

    test "missing comma in tuple" do
      code = ~S'{:foo :bar, :baz}'

      assert Spitfire.parse(code) == {:error, {:foo, :baz}, [{[line: 1, column: 7], "syntax error"}]}
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
                 [do: [line: 1, column: 5], end: [line: 1, column: 5], line: 1, column: 1],
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
                             [{:__aliases__, [{:last, [line: 2, column: 3]}, {:line, 2}, {:column, 3}], [:Some]}, :thing]
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
                 [do: [line: 1, column: 5], end: [line: 1, column: 5], line: 1, column: 1],
                 [
                   [
                     do: {
                       :foo,
                       [do: [line: 2, column: 7], end: [line: 5, column: 1], line: 2, column: 3],
                       [
                         [
                           do: {
                             :__block__,
                             [],
                             [
                               {
                                 {
                                   :.,
                                   [line: 3, column: 9],
                                   [
                                     {:__aliases__, [{:last, [line: 3, column: 5]}, {:line, 3}, {:column, 5}], [:Some]},
                                     :thing
                                   ]
                                 },
                                 [
                                   {:end_of_expression, [newlines: 1, line: 3, column: 17]},
                                   {:closing, [line: 3, column: 16]},
                                   {:line, 3},
                                   {:column, 10}
                                 ],
                                 []
                               },
                               :ok
                             ]
                           }
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
                      {:end_of_expression, [newlines: 2, line: 1, column: 10]},
                      {:closing, [line: 1, column: 9]},
                      line: 1,
                      column: 1
                    ],
                    [
                      {:+, [line: 1, column: 7],
                       [1, {:__error__, [line: 1, column: 7], ["malformed right-hand side of + operator"]}]}
                    ]},
                   {:bar, [{:closing, [line: 3, column: 8]}, line: 3, column: 1], [{:two, [line: 3, column: 5], Elixir}]}
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
               {:foo, [{:closing, [line: 3, column: 8]}, {:line, 1}, {:column, 1}],
                [
                  {:+, [line: 1, column: 7],
                   [
                     1,
                     {:bar, [{:closing, [line: 3, column: 8]}, line: 3, column: 1],
                      [{:two, [line: 3, column: 5], Elixir}]}
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
                 [line: 1, column: 10],
                 [
                   {:new_list, [line: 1, column: 1], Elixir},
                   {
                     {:., [line: 2, column: 7],
                      [{:__aliases__, [{:last, [line: 2, column: 3]}, {:line, 2}, {:column, 3}], [:Enum]}, :map]},
                     [{:closing, [line: 5, column: 19]}, {:line, 2}, {:column, 8}],
                     [
                       {:some_list, [line: 2, column: 12], Elixir},
                       {
                         :fn,
                         [closing: [line: 5, column: 19], line: 2, column: 23],
                         [
                           {
                             :->,
                             [newlines: 3, depth: 1, line: 2, column: 31],
                             [
                               [{:item, [line: 2, column: 26], Elixir}],
                               {
                                 :send,
                                 [closing: [line: 5, column: 19], line: 5, column: 1],
                                 [{:pid, [line: 5, column: 6], Elixir}, {:new_list, [line: 5, column: 11], Elixir}]
                               }
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
                   [do: [line: 1, column: 15], end: [line: 12, column: 1], line: 1, column: 1],
                   [
                     {:__aliases__, [{:last, [line: 1, column: 11]}, {:line, 1}, {:column, 11}], [:Foo]},
                     [
                       do: {
                         :__block__,
                         [],
                         [
                           {
                             :import,
                             [end_of_expression: [newlines: 2, line: 2, column: 13], line: 2, column: 3],
                             [{:__aliases__, [{:last, [line: 2, column: 10]}, {:line, 2}, {:column, 10}], [:Baz]}]
                           },
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
                               {:bat, [line: 4, column: 7], Elixir},
                               [
                                 do: {
                                   :__block__,
                                   [],
                                   [
                                     {
                                       :=,
                                       [end_of_expression: [newlines: 1, line: 5, column: 14], line: 5, column: 9],
                                       [{:var, [line: 5, column: 5], Elixir}, 123]
                                     },
                                     {:{}, [line: 6, column: 5], []}
                                   ]
                                 }
                               ]
                             ]
                           },
                           {
                             :def,
                             [do: [line: 9, column: 22], end: [line: 11, column: 3], line: 9, column: 3],
                             [{:local_function, [line: 9, column: 7], Elixir}, [do: {:__block__, [], []}]]
                           }
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
                   [do: [line: 1, column: 15], end: [line: 12, column: 1], line: 1, column: 1],
                   [
                     {:__aliases__, [{:last, [line: 1, column: 11]}, {:line, 1}, {:column, 11}], [:Foo]},
                     [
                       do: {
                         :__block__,
                         [],
                         [
                           {
                             :import,
                             [end_of_expression: [newlines: 2, line: 2, column: 13], line: 2, column: 3],
                             [{:__aliases__, [{:last, [line: 2, column: 10]}, {:line, 2}, {:column, 10}], [:Baz]}]
                           },
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
                               {:bat, [line: 4, column: 7], Elixir},
                               [
                                 do: {
                                   :__block__,
                                   [],
                                   [
                                     {
                                       :=,
                                       [end_of_expression: [newlines: 1, line: 5, column: 14], line: 5, column: 9],
                                       [{:var, [line: 5, column: 5], Elixir}, 123]
                                     },
                                     {:{}, [closing: nil, line: 6, column: 5], [{:var, [line: 6, column: 6], Elixir}]}
                                   ]
                                 }
                               ]
                             ]
                           },
                           {
                             :def,
                             [do: [line: 9, column: 22], end: [line: 11, column: 3], line: 9, column: 3],
                             [{:local_function, [line: 9, column: 7], Elixir}, [do: {:__block__, [], []}]]
                           }
                         ]
                       }
                     ]
                   ]
                 },
                 [{[line: 6, column: 5], "missing closing brace for tuple"}]
               }
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
                 [do: [line: 1, column: 15], end: [line: 12, column: 1], line: 1, column: 1],
                 [
                   {:__aliases__, [{:last, [line: 1, column: 11]}, {:line, 1}, {:column, 11}], [:Foo]},
                   [
                     do: {
                       :__block__,
                       [],
                       [
                         {
                           :import,
                           [end_of_expression: [newlines: 2, line: 2, column: 13], line: 2, column: 3],
                           [{:__aliases__, [{:last, [line: 2, column: 10]}, {:line, 2}, {:column, 10}], [:Baz]}]
                         },
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
                             {:bat, [line: 4, column: 7], Elixir},
                             [
                               do: {
                                 :__block__,
                                 [],
                                 [
                                   {
                                     :=,
                                     [end_of_expression: [newlines: 1, line: 5, column: 14], line: 5, column: 9],
                                     [{:var, [line: 5, column: 5], Elixir}, 123]
                                   },
                                   [{:var, [line: 6, column: 6], Elixir}]
                                 ]
                               }
                             ]
                           ]
                         },
                         {
                           :def,
                           [do: [line: 9, column: 22], end: [line: 11, column: 3], line: 9, column: 3],
                           [{:local_function, [line: 9, column: 7], Elixir}, [do: {:__block__, [], []}]]
                         }
                       ]
                     }
                   ]
                 ]
               },
               [{[line: 6, column: 5], "missing closing bracket for list"}]
             }
    end
  end
end
