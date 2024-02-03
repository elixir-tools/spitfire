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
                            {:run, [line: 5, column: 7], [{:arg, [line: 5, column: 11], nil}]},
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
                [{:foo, [line: 1, column: 1], nil}, :bar]}

      code = "foo[:bar][:baz]"

      assert Spitfire.parse(code) ==
               {:ok,
                {{:., [from_brackets: true, closing: [line: 1, column: 15], line: 1, column: 10], [Access, :get]},
                 [from_brackets: true, closing: [line: 1, column: 15], line: 1, column: 10],
                 [
                   {{:., [closing: [line: 1, column: 9], line: 1, column: 4], [Access, :get]},
                    [closing: [line: 1, column: 9], line: 1, column: 4], [{:foo, [line: 1, column: 1], nil}, :bar]},
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
                       [{:meta, [line: 1, column: 2], nil}, :end_of_expression]},
                      {:meta, [line: 1, column: 30], nil}
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
                        {:foo, [line: 1, column: 7], nil},
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
                              {:one, [line: 1, column: 11], nil},
                              {{:., [], [{:__aliases__, [line: 1, column: 18], [:String]}, :t]}, [], []}
                            ]},
                           {:number, [line: 1, column: 30], nil}
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

      assert Spitfire.parse!(code) == {:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}
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
                   {:"::", [], [{{:., [], [Kernel, :to_string]}, [], [{:alice, [], nil}]}, {:binary, [], nil}]},
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
                   {:"::", [], [{{:., [], [Kernel, :to_string]}, [], [{:alice, [], nil}]}, {:binary, [], nil}]},
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
                        {:binary, [], nil}
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
                         [{:bar, [line: 1, column: 8], nil}]},
                        {:binary, [], nil}
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
               {:<-, [line: 1, column: 7], [{:apple, [line: 1, column: 1], nil}, {:apples, [line: 1, column: 10], nil}]}
    end

    test "parses right stab" do
      code = """
      -> bar
      """

      assert Spitfire.parse!(code) == [{:->, [line: 1, column: 1], [[], {:bar, [line: 1, column: 4], nil}]}]

      code = """
      -> :ok
      """

      assert Spitfire.parse!(code) == [{:->, [line: 1, column: 1], [[], :ok]}]

      code = """
      foo -> bar
      """

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 5],
                [[{:foo, [line: 1, column: 1], nil}], {:bar, [line: 1, column: 8], nil}]}
             ]

      code = """
      foo, bar, baz -> bar
      """

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 15],
                [
                  [
                    {:foo, [line: 1, column: 1], nil},
                    {:bar, [line: 1, column: 6], nil},
                    {:baz, [line: 1, column: 11], nil}
                  ],
                  {:bar, [line: 1, column: 18], nil}
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
                    {:alice, [line: 1, column: 1], nil},
                    {:bob, [line: 1, column: 8], nil},
                    {:carol, [line: 1, column: 13], nil}
                  ],
                  {:__block__, [], [:error, {:bar, [line: 3, column: 3], nil}]}
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
                [[{:foo, [line: 1, column: 1], nil}], {:__block__, [], [:ok, {:baz, [line: 3, column: 3], nil}]}]},
               {:->, [depth: 0, line: 5, column: 19],
                [
                  [
                    {:alice, [line: 5, column: 1], nil},
                    {:bob, [line: 5, column: 8], nil},
                    {:carol, [line: 5, column: 13], nil}
                  ],
                  {:__block__, [], [:error, {:bar, [line: 7, column: 3], nil}]}
                ]}
             ]

      code = ~S'''
      ^foo ->
        :ok
      '''

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 6],
                [[{:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}], :ok]}
             ]

      code = ~S'''
      @foo ->
        :ok
      '''

      assert Spitfire.parse!(code) == [
               {:->, [depth: 0, line: 1, column: 6],
                [[{:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}], :ok]}
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
            {:<-, [line: 1, column: 7], [{:i, [line: 1, column: 5], nil}, {:.., [line: 1, column: 11], [0, 100]}]},
            [do: {:+, [line: 2, column: 5], [{:i, [line: 2, column: 3], nil}, {:i, [line: 2, column: 7], nil}]}]
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
               {:ok, {:school, [line: 1, column: 12], nil}},
               {{:., [], [{:__aliases__, [line: 1, column: 23], [:State]}, :get_school]}, [],
                [{:id, [line: 1, column: 40], nil}]}
             ]},
            {:<-, [line: 2, column: 22],
             [
               {:ok, {:teachers, [line: 2, column: 12], nil}},
               {{:., [], [{:__aliases__, [line: 2, column: 25], [:School]}, :list_teachers]}, [],
                [{:school, [line: 2, column: 46], nil}]}
             ]},
            {:<-, [line: 3, column: 21],
             [
               {:ok, {:teacher, [line: 3, column: 12], nil}},
               {{:., [], [{:__aliases__, [line: 3, column: 24], [:Teacher]}, :coolest]}, [],
                [{:teachers, [line: 3, column: 40], nil}]}
             ]},
            [
              do:
                {{:., [], [{:__aliases__, [line: 4, column: 3], [:Email]}, :send]}, [],
                 [{:teacher, [line: 4, column: 14], nil}, "You are the coolest teacher"]}
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
                  {:foobar, [line: 1, column: 1], nil},
                  {:alice, [line: 2, column: 1], nil},
                  {:bob, [line: 3, column: 1], nil}
                ]}
    end

    test "parses lists" do
      codes = [
        {~s'''
         []
         ''', {:ok, []}},
        {~s'''
         [arg]
         ''', {:ok, [{:arg, [line: 1, column: 2], nil}]}},
        {~s'''
         [one, :two, "three"]
         ''', {:ok, [{:one, [line: 1, column: 2], nil}, :two, "three"]}},
        {~s'''
          [
            one,
            :two,
            "three"
          ]
         ''', {:ok, [{:one, [line: 2, column: 4], nil}, :two, "three"]}}
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
            {:one, [line: 1, column: 5], nil},
            {:two, [line: 1, column: 10], nil},
            [alice: {:alice, [line: 1, column: 22], nil}, bob: {:bob, [line: 1, column: 34], nil}]
          ]}},
        {~s'''
         foo alice: alice do
           :ok
         end
         ''', {:foo, [line: 1, column: 1], [[alice: {:alice, [line: 1, column: 12], nil}], [do: :ok]]}},
        {~s'''
         [:one, two: :three]
         ''', [:one, {:two, :three}]},
        {~s'''
         @moduledoc deprecated:
            "Use the new child specifications outlined in the Supervisor module instead"
         ''',
         {:@, [line: 1, column: 1],
          [
            {:moduledoc, [line: 1, column: 2],
             [
               [
                 deprecated: "Use the new child specifications outlined in the Supervisor module instead"
               ]
             ]}
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
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
                  {:foo, [line: 1, column: 6], nil},
                  [
                    do: [
                      {:->, [depth: 1, line: 2, column: 41],
                       [
                         [
                           {:when, [line: 2, column: 18],
                            [
                              :kw_identifier,
                              {:or, [line: 2, column: 31],
                               [{:is_list, [line: 2, column: 23], nil}, {:is_map, [line: 2, column: 34], nil}]}
                            ]}
                         ],
                         {:&, [line: 2, column: 44],
                          [{:/, [line: 2, column: 64], [{:parse_kw_identifier, [line: 2, column: 45], nil}, 1]}]}
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
            [{:|, [line: 1, column: 6], [{:one, [line: 1, column: 2], nil}, {:rest, [line: 1, column: 8], nil}]}],
            {:my_list, [line: 1, column: 16], nil}
          ]}},
        {~s'''
         [one, two | rest] = my_list
         ''',
         {:=, [line: 1, column: 19],
          [
            [
              {:one, [line: 1, column: 2], nil},
              {:|, [line: 1, column: 11], [{:two, [line: 1, column: 7], nil}, {:rest, [line: 1, column: 13], nil}]}
            ],
            {:my_list, [line: 1, column: 21], nil}
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
         ''', {{:one, [line: 1, column: 2], nil}, :two}},
        {~s'''
         {
           one,
           :two,
           "three"
         }
         ''', {:{}, [line: 1, column: 1], [{:one, [line: 2, column: 3], nil}, :two, "three"]}},
        {~s'''
         {one, :two, "three"}
         ''', {:{}, [line: 1, column: 1], [{:one, [line: 1, column: 2], nil}, :two, "three"]}},
        {~s'''
         {
           one,
           :two,
           "three"
         }
         ''', {:{}, [line: 1, column: 1], [{:one, [line: 2, column: 3], nil}, :two, "three"]}}
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
            {{:bing, [line: 1, column: 67], nil}, {:bong, [line: 1, column: 75], nil}},
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
            {{:bing, [line: 7, column: 3], nil}, {:bong, [line: 7, column: 11], nil}},
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
                name: {:name, [line: 1, column: 16], nil},
                properties:
                  {:%, [line: 1, column: 34],
                   [
                     {:__aliases__, [last: [line: 1, column: 35], line: 1, column: 35], [:Properties]},
                     {:%{}, [closing: [line: 1, column: 74], line: 1, column: 45],
                      [
                        key: {:key, [line: 1, column: 51], nil},
                        value: {:get_value, [closing: [line: 1, column: 73], line: 1, column: 63], []}
                      ]}
                   ]}
              ]}
           ]}
        },
        {~S'%__MODULE__{foo: bar}',
         {:%, [line: 1, column: 1],
          [
            {:__MODULE__, [line: 1, column: 2], nil},
            {:%{}, [closing: [line: 1, column: 21], line: 1, column: 12], [foo: {:bar, [line: 1, column: 18], nil}]}
          ]}},
        {~S'%module{foo: bar}',
         {:%, [line: 1, column: 1],
          [
            {:module, [line: 1, column: 2], nil},
            {:%{}, [closing: [line: 1, column: 17], line: 1, column: 8], [foo: {:bar, [line: 1, column: 14], nil}]}
          ]}},
        {~S'%@foo{foo: bar}',
         {:%, [line: 1, column: 1],
          [
            {:@, [line: 1, column: 2], [{:foo, [line: 1, column: 3], nil}]},
            {:%{}, [closing: [line: 1, column: 15], line: 1, column: 6], [foo: {:bar, [line: 1, column: 12], nil}]}
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
         1 ** 2
         ''', {:**, [line: 1, column: 3], [1, 2]}},
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
            {:in, [], [{:foo, [line: 1, column: 1], nil}, {:bar, [line: 1, column: 12], nil}]}
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
         ''', {:.., [line: 1, column: 4], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 6], nil}]}},
        {~s'''
         one..two//2
         ''', {:"..//", [line: 1, column: 4], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 6], nil}, 2]}},
        {~s'''
         one <> two
         ''', {:<>, [line: 1, column: 5], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 8], nil}]}},
        {~s'''
         one ++ two
         ''', {:++, [line: 1, column: 5], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 8], nil}]}},
        {~s'''
         one -- two
         ''', {:--, [line: 1, column: 5], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 8], nil}]}},
        {~s'''
         one +++ two
         ''', {:+++, [line: 1, column: 5], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 9], nil}]}},
        {~s'''
         one --- two
         ''', {:---, [line: 1, column: 5], [{:one, [line: 1, column: 1], nil}, {:two, [line: 1, column: 9], nil}]}},
        {~s'''
         one ++ two ++ three
         ''',
         {:++, [line: 1, column: 5],
          [
            {:one, [line: 1, column: 1], nil},
            {:++, [line: 1, column: 12], [{:two, [line: 1, column: 8], nil}, {:three, [line: 1, column: 15], nil}]}
          ]}},
        {~s'''
         @foo
         ''', {:@, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}},
        {~s'''
         !foo
         ''', {:!, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}},
        {~s'''
         not foo
         ''', {:not, [line: 1, column: 1], [{:foo, [line: 1, column: 5], nil}]}},
        {~s'''
         ^foo
         ''', {:^, [line: 1, column: 1], [{:foo, [line: 1, column: 2], nil}]}},
        {~s'''
         ~~~foo
         ''', {:"~~~", [line: 1, column: 1], [{:foo, [line: 1, column: 4], nil}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
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
            {:arg, [line: 1, column: 5], nil},
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
         ''', {:if, [line: 1, column: 1], [{:arg, [line: 1, column: 4], nil}, [do: "howdy", else: :partner]]}},
        {~S'''
         {%{},
            quote do
              Enum.into(unquote(metadata), unquote(escape_metadata(maybe_application)))
            end}
         ''',
         {{:%{}, [closing: [line: 1, column: 4], line: 1, column: 3], []},
          {:quote, [do: [line: 2, column: 16], end: [line: 4, column: 10], line: 2, column: 10],
           [
             [
               do:
                 {{:., [line: 3, column: 16],
                   [
                     {:__aliases__, [last: [line: 3, column: 12], line: 3, column: 12], [:Enum]},
                     :into
                   ]}, [closing: [line: 3, column: 84], line: 3, column: 17],
                  [
                    {:unquote, [closing: [line: 3, column: 38], line: 3, column: 22],
                     [{:metadata, [line: 3, column: 30], nil}]},
                    {:unquote, [closing: [line: 3, column: 83], line: 3, column: 41],
                     [
                       {:escape_metadata, [closing: [line: 3, column: 82], line: 3, column: 49],
                        [{:maybe_application, [line: 3, column: 65], nil}]}
                     ]}
                  ]}
             ]
           ]}}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
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

      assert Spitfire.parse(code) ==
               {:ok,
                {:__block__, [closing: [line: 5, column: 1], line: 1, column: 1],
                 [
                   {:=, [end_of_expression: [newlines: 1, line: 2, column: 24], line: 2, column: 12],
                    [
                      {:min_line, [line: 2, column: 3], nil},
                      {:line, [closing: [line: 2, column: 23], line: 2, column: 14],
                       [{:meta, [line: 2, column: 19], nil}]}
                    ]},
                   {:=, [end_of_expression: [newlines: 1, line: 3, column: 32], line: 3, column: 12],
                    [
                      {:max_line, [line: 3, column: 3], nil},
                      {:closing_line, [closing: [line: 3, column: 31], line: 3, column: 14],
                       [{:meta, [line: 3, column: 27], nil}]}
                    ]},
                   {{:., [line: 4, column: 7],
                     [
                       {:__aliases__, [last: [line: 4, column: 3], line: 4, column: 3], [:Enum]},
                       :any?
                     ]}, [closing: [line: 4, column: 82], line: 4, column: 8],
                    [
                      {:comments, [line: 4, column: 13], nil},
                      {:fn, [closing: [line: 4, column: 79], line: 4, column: 23],
                       [
                         {:->, [line: 4, column: 40],
                          [
                            [
                              {:%{}, [closing: [line: 4, column: 38], line: 4, column: 27],
                               [line: {:line, [line: 4, column: 34], nil}]}
                            ],
                            {:and, [line: 4, column: 59],
                             [
                               {:>, [line: 4, column: 48],
                                [
                                  {:line, [line: 4, column: 43], nil},
                                  {:min_line, [line: 4, column: 50], nil}
                                ]},
                               {:<, [line: 4, column: 68],
                                [
                                  {:line, [line: 4, column: 63], nil},
                                  {:max_line, [line: 4, column: 70], nil}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]}
                 ]}}

      code = ~S'''
      (min_line = line(meta)
      max_line = closing_line(meta)
      Enum.any?(comments, fn %{line: line} -> line > min_line and line < max_line end))
      '''

      assert Spitfire.parse(code) ==
               {:ok,
                {:__block__, [closing: [line: 3, column: 83], line: 1, column: 1],
                 [
                   {:=, [end_of_expression: [newlines: 1, line: 1, column: 23], line: 1, column: 11],
                    [
                      {:min_line, [line: 1, column: 2], nil},
                      {:line, [closing: [line: 1, column: 22], line: 1, column: 13],
                       [{:meta, [line: 1, column: 18], nil}]}
                    ]},
                   {:=, [end_of_expression: [newlines: 1, line: 2, column: 32], line: 2, column: 12],
                    [
                      {:max_line, [line: 2, column: 3], nil},
                      {:closing_line, [closing: [line: 2, column: 31], line: 2, column: 14],
                       [{:meta, [line: 2, column: 27], nil}]}
                    ]},
                   {{:., [line: 3, column: 7],
                     [
                       {:__aliases__, [last: [line: 3, column: 3], line: 3, column: 3], [:Enum]},
                       :any?
                     ]}, [closing: [line: 3, column: 82], line: 3, column: 8],
                    [
                      {:comments, [line: 3, column: 13], nil},
                      {:fn, [closing: [line: 3, column: 79], line: 3, column: 23],
                       [
                         {:->, [line: 3, column: 40],
                          [
                            [
                              {:%{}, [closing: [line: 3, column: 38], line: 3, column: 27],
                               [line: {:line, [line: 3, column: 34], nil}]}
                            ],
                            {:and, [line: 3, column: 59],
                             [
                               {:>, [line: 3, column: 48],
                                [
                                  {:line, [line: 3, column: 43], nil},
                                  {:min_line, [line: 3, column: 50], nil}
                                ]},
                               {:<, [line: 3, column: 68],
                                [
                                  {:line, [line: 3, column: 63], nil},
                                  {:max_line, [line: 3, column: 70], nil}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]}
                 ]}}
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
             {:foo, [line: 1, column: 6], nil},
             [
               do: [
                 {:->, [depth: 1, line: 2, column: 6],
                  [[{:bar, [line: 2, column: 2], nil}], {:bar, [line: 3, column: 4], nil}]}
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
                           {:->, [depth: 2, line: 6, column: 9], [[{:_, [line: 6, column: 7], nil}], :error]}
                         ]
                       ]
                     ]}
                  ]},
                 {:->, [depth: 1, line: 10, column: 5], [[{:_, [line: 10, column: 3], nil}], :error]}
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
             {:infix, [line: 1, column: 6], nil},
             [
               do: [
                 {:->, [depth: 1, line: 2, column: 7],
                  [[nil], {{:left, [line: 3, column: 6], nil}, {:parser, [line: 3, column: 12], nil}}]},
                 {:->, [depth: 1, line: 5, column: 40],
                  [
                    [
                      {:when, [line: 5, column: 13],
                       [
                         {:^, [line: 5, column: 3], [{:do_block, [line: 5, column: 4], nil}]},
                         {:!=, [line: 5, column: 34],
                          [
                            {{:., [], [{:parser, [line: 5, column: 18], nil}, :nestings]}, [], []},
                            []
                          ]}
                       ]}
                    ],
                    {{:left, [line: 6, column: 6], nil},
                     {:next_token, [line: 6, column: 12], [{:parser, [line: 6, column: 23], nil}]}}
                  ]},
                 {:->, [depth: 1, line: 8, column: 5],
                  [
                    [{:_, [line: 8, column: 3], nil}],
                    {{:., [], [{:infix, [line: 9, column: 5], nil}]}, [],
                     [
                       {:next_token, [line: 9, column: 12], [{:parser, [line: 9, column: 23], nil}]},
                       {:left, [line: 9, column: 32], nil}
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
               {:c, [line: 1, column: 5], nil},
               {:d, [line: 1, column: 8], nil}
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
               {:c, [line: 1, column: 5], nil},
               {:d, [line: 1, column: 8], nil}
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
         ''', {:foo, [line: 1, column: 1], [{:arg, [line: 1, column: 5], nil}, {:arg2, [line: 1, column: 10], nil}]}},
        {~s'''
         foo(
           arg,
           arg2
         )
         ''', {:foo, [line: 1, column: 1], [{:arg, [line: 2, column: 3], nil}, {:arg2, [line: 3, column: 3], nil}]}},
        {~s'''
         foo arg, arg2
         ''', {:foo, [line: 1, column: 1], [{:arg, [line: 1, column: 5], nil}, {:arg2, [line: 1, column: 10], nil}]}},
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
          [{:arg, [line: 1, column: 12], nil}, {:arg2, [line: 1, column: 17], nil}]}},
        {~s'''
         Remote.foo(
           arg,
           arg2
         )
         ''',
         {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [],
          [{:arg, [line: 2, column: 3], nil}, {:arg2, [line: 3, column: 3], nil}]}},
        {~s'''
         Remote.foo arg, arg2
         ''',
         {{:., [], [{:__aliases__, [line: 1, column: 1], [:Remote]}, :foo]}, [],
          [{:arg, [line: 1, column: 12], nil}, {:arg2, [line: 1, column: 17], nil}]}},
        {~s'''
         :erlang.foo
         ''', {{:., [], [:erlang, :foo]}, [], []}},
        {~s'''
         :erlang.foo()
         ''', {{:., [], [:erlang, :foo]}, [], []}},
        {~s'''
         :erlang.foo(arg, arg2)
         ''', {{:., [], [:erlang, :foo]}, [], [{:arg, [line: 1, column: 13], nil}, {:arg2, [line: 1, column: 18], nil}]}},
        {~s'''
         :erlang.foo arg, arg2
         ''', {{:., [], [:erlang, :foo]}, [], [{:arg, [line: 1, column: 13], nil}, {:arg2, [line: 1, column: 18], nil}]}},
        {~s'''
         somevar.foo
         ''', {{:., [], [{:somevar, [line: 1, column: 1], nil}, :foo]}, [], []}},
        {~s'''
         somevar.foo()
         ''', {{:., [], [{:somevar, [line: 1, column: 1], nil}, :foo]}, [], []}},
        {~s'''
         :elixir_tokenizer.tokenize(String.to_charlist(code), 1, [])
         ''',
         {{:., [], [:elixir_tokenizer, :tokenize]}, [],
          [
            {{:., [], [{:__aliases__, [line: 1, column: 28], [:String]}, :to_charlist]}, [],
             [{:code, [line: 1, column: 47], nil}]},
            1,
            []
          ]}},
        {~s'''
         somevar.foo(arg, arg2)
         ''',
         {{:., [], [{:somevar, [line: 1, column: 1], nil}, :foo]}, [],
          [{:arg, [line: 1, column: 13], nil}, {:arg2, [line: 1, column: 18], nil}]}},
        {~s'''
         somevar.foo arg, arg2
         ''',
         {{:., [], [{:somevar, [line: 1, column: 1], nil}, :foo]}, [],
          [{:arg, [line: 1, column: 13], nil}, {:arg2, [line: 1, column: 18], nil}]}},
        {~S'''
         defp unquote(:"#{name}_text")(), do: unquote(contents)
         ''',
         {:defp, [line: 1, column: 1],
          [
            {{:unquote, [closing: [line: 1, column: 29], line: 1, column: 6],
              [
                {{:., [line: 1, column: 14], [:erlang, :binary_to_atom]}, [delimiter: "\"", line: 1, column: 14],
                 [
                   {:<<>>, [line: 1, column: 14],
                    [
                      {:"::", [line: 1, column: 16],
                       [
                         {{:., [line: 1, column: 16], [Kernel, :to_string]},
                          [
                            from_interpolation: true,
                            closing: [line: 1, column: 22],
                            line: 1,
                            column: 16
                          ], [{:name, [line: 1, column: 18], nil}]},
                         {:binary, [line: 1, column: 16], nil}
                       ]},
                      "_text"
                    ]},
                   :utf8
                 ]}
              ]},
             [
               closing: [line: 1, column: 31],
               closing: [line: 1, column: 29],
               line: 1,
               column: 6
             ], []},
            [
              do:
                {:unquote, [closing: [line: 1, column: 54], line: 1, column: 38],
                 [{:contents, [line: 1, column: 46], nil}]}
            ]
          ]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
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
             [[{:one, [line: 1, column: 4], nil}], {:one, [line: 2, column: 3], nil}]}
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
             [[{:one, [line: 2, column: 2], nil}], {:one, [line: 3, column: 3], nil}]}
          ]}},
        {~s'''
         fn(one) ->
           one
         end
         ''',
         {:fn, [line: 1, column: 1],
          [
            {:->, [depth: 1, line: 1, column: 9],
             [[{:one, [line: 1, column: 4], nil}], {:one, [line: 2, column: 3], nil}]}
          ]}},
        {~S'foo(fn a -> a end)',
         {:foo, [closing: [line: 1, column: 18], line: 1, column: 1],
          [
            {:fn, [closing: [line: 1, column: 15], line: 1, column: 5],
             [
               {:->, [line: 1, column: 10], [[{:a, [line: 1, column: 8], nil}], {:a, [line: 1, column: 13], nil}]}
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
         ''', {:=, [line: 1, column: 5], [{:foo, [line: 1, column: 1], nil}, :bar]}}
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
                 [[{:==, [line: 2, column: 11], [{:prefix, [line: 2, column: 4], nil}, nil]}], :foo]},
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
                  {:parse, [line: 1, column: 5], [{:code, [line: 1, column: 11], nil}]},
                  [
                    do:
                      {:__block__, [],
                       [
                         {:=, [line: 2, column: 10],
                          [
                            {:parser, [line: 2, column: 3], nil},
                            {:|>, [line: 2, column: 42],
                             [
                               {:|>, [line: 2, column: 26],
                                [
                                  {:|>, [line: 2, column: 17],
                                   [{:code, [line: 2, column: 12], nil}, {:new, [line: 2, column: 20], []}]},
                                  {:next_token, [line: 2, column: 29], []}
                                ]},
                               {:next_token, [line: 2, column: 45], []}
                             ]}
                          ]},
                         {:parse_program, [line: 4, column: 3], [{:parser, [line: 4, column: 17], nil}]}
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
                   {:foo, [line: 1, column: 1], nil},
                   {:is_binary, [line: 1, column: 10], [{:foo, [line: 1, column: 20], nil}]}
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
                   {:foo, [line: 1, column: 1], nil},
                   {:is_binary, [line: 1, column: 10], [{:foo, [line: 1, column: 20], nil}]}
                 ]}
              ],
              :ok
            ]},
           {:->, [depth: 0, line: 4, column: 25],
            [
              [
                {:when, [line: 4, column: 5],
                 [
                   {:bar, [line: 4, column: 1], nil},
                   {:is_number, [line: 4, column: 10], [{:bar, [line: 4, column: 20], nil}]}
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
               {:foo, [line: 1, column: 5], [{:bar, [line: 1, column: 9], nil}]},
               {:is_binary, [line: 1, column: 19], [{:bar, [line: 1, column: 29], nil}]}
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
                    {:foo, [line: 1, column: 4], nil},
                    {:is_binary, [line: 1, column: 13], [{:foo, [line: 1, column: 23], nil}]}
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
                    {:foo, [line: 1, column: 4], nil},
                    {:bar, [line: 1, column: 9], nil},
                    {:_baz, [line: 1, column: 14], nil},
                    {:and, [line: 1, column: 39],
                     [
                       {:is_binary, [line: 1, column: 24], [{:foo, [line: 1, column: 34], nil}]},
                       {:in, [line: 1, column: 47], [{:bar, [line: 1, column: 43], nil}, [:alice, :bob]]}
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
         ''', {:&, [line: 1, column: 1], [{:/, [line: 1, column: 5], [{:foo, [line: 1, column: 2], nil}, 1]}]}},
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
             [{:one, [line: 1, column: 10], nil}, {:&, [line: 1, column: 15], [1]}]}
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
         ''', {{:., [], [{:foo, [], nil}]}, [], []}},
        {~s'''
         foo.(one, two)
         ''', {{:., [], [{:foo, [], nil}]}, [], [{:one, [], nil}, {:two, [], nil}]}},
        {~s'''
         foo.(
           one,
           two
         )
         ''', {{:., [], [{:foo, [], nil}]}, [], [{:one, [], nil}, {:two, [], nil}]}},
        {~s'''
         infix.(next_token(parser), left)
         ''', {{:., [], [{:infix, [], nil}]}, [], [{:next_token, [], [{:parser, [], nil}]}, {:left, [], nil}]}}
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
                  {:==, [], [{:prefix, [], nil}, nil]},
                  [
                    do:
                      {:__block__, [],
                       [
                         {:=, [],
                          [
                            {{:row, [], nil}, {:col, [], nil}},
                            {:token_loc, [],
                             [
                               {{:., [], [{:parser, [], nil}, :current_token]}, [], []}
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
                                        {{:., [], [Kernel, :to_string]}, [], [{:row, [], nil}]},
                                        {:binary, [], nil}
                                      ]},
                                     ":",
                                     {:"::", [],
                                      [
                                        {{:., [], [Kernel, :to_string]}, [], [{:col, [], nil}]},
                                        {:binary, [], nil}
                                      ]},
                                     ": unknown prefix: ",
                                     {:"::", [],
                                      [
                                        {{:., [], [Kernel, :to_string]}, [],
                                         [{:current_token_type, [], [{:parser, [], nil}]}]},
                                        {:binary, [], nil}
                                      ]}
                                   ]},
                                  {{:., [], [{:__aliases__, [], [:IO, :ANSI]}, :reset]}, [], []}
                                ]}
                             ]}
                          ]},
                         {:error, {:next_token, [], [{:parser, [], nil}]}}
                       ]},
                    else:
                      {:__block__, [],
                       [
                         {:=, [],
                          [
                            {{:left, [], nil}, {:parser, [], nil}},
                            {{:., [], [{:prefix, [], nil}]}, [], [{:parser, [], nil}]}
                          ]},
                         {:=, [],
                          [
                            {:calc_prec, [], nil},
                            {:fn, [],
                             [
                               {:->, [depth: 2],
                                [
                                  [{:parser, [], nil}],
                                  {:__block__, [],
                                   [
                                     {:=, [],
                                      [
                                        {{:_associativity, [], nil}, {:power, [], nil}},
                                        {:peek_precedence, [], [{:parser, [], nil}]}
                                      ]},
                                     {:=, [],
                                      [
                                        {:precedence, [], nil},
                                        {:case, [],
                                         [
                                           {:associativity, [], nil},
                                           [
                                             do: [
                                               {:->, [depth: 3], [[:left], {:precedence, [], nil}]},
                                               {:->, [depth: 3], [[:unassoc], 0]},
                                               {:->, [depth: 3],
                                                [
                                                  [:right],
                                                  {:-, [], [{:precedence, [], nil}, 1]}
                                                ]}
                                             ]
                                           ]
                                         ]}
                                      ]},
                                     {:<, [], [{:precedence, [], nil}, {:power, [], nil}]}
                                   ]}
                                ]}
                             ]}
                          ]},
                         {:=, [], [{:terminals, [], nil}, [:eol, :eof, :"}", :")", :"]"]]},
                         {:=, [],
                          [
                            {:terminals, [], nil},
                            {:if, [],
                             [
                               {:is_top, [], nil},
                               [
                                 do: {:terminals, [], nil},
                                 else: [{:|, [], [:",", {:terminals, [], nil}]}]
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
                                        {:peek_token, [], [{:parser, [], nil}]},
                                        {:terminals, [], nil}
                                      ]}
                                   ]},
                                  {{:., [], [{:calc_prec, [], nil}]}, [], [{:parser, [], nil}]}
                                ]},
                               {{:left, [], nil}, {:parser, [], nil}}
                             ]},
                            [
                              do:
                                {:__block__, [],
                                 [
                                   {:=, [],
                                    [
                                      {:infix, [], nil},
                                      {:case, [],
                                       [
                                         {:peek_token_type, [], [{:parser, [], nil}]},
                                         [
                                           do: [
                                             {:->, [depth: 3],
                                              [
                                                [:match_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:when_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:pipe_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:dual_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:mult_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:concat_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:assoc_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_assoc_op, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:arrow_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:ternary_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:or_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:and_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:comp_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:rel_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:in_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:xor_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:in_match_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_infix_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:range_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_range_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:stab_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_stab_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:do],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_do_block, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:dot_call_op],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_dot_call_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [:.],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_dot_expression, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3],
                                              [
                                                [{:when, [], [:",", {:is_top, [], nil}]}],
                                                {:&, [],
                                                 [
                                                   {:/, [], [{:parse_comma, [], nil}, 2]}
                                                 ]}
                                              ]},
                                             {:->, [depth: 3], [[{:_, [], nil}], nil]}
                                           ]
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:do_block, [], nil},
                                      {:&, [],
                                       [
                                         {:/, [], [{:parse_do_block, [], nil}, 2]}
                                       ]}
                                    ]},
                                   {:case, [],
                                    [
                                      {:infix, [], nil},
                                      [
                                        do: [
                                          {:->, [depth: 3], [[nil], {{:left, [], nil}, {:parser, [], nil}}]},
                                          {:->, [depth: 3],
                                           [
                                             [
                                               {:when, [],
                                                [
                                                  {:^, [], [{:do_block, [], nil}]},
                                                  {:!=, [],
                                                   [
                                                     {{:., [], [{:parser, [], nil}, :nestings]}, [], []},
                                                     []
                                                   ]}
                                                ]}
                                             ],
                                             {{:left, [], nil}, {:next_token, [], [{:parser, [], nil}]}}
                                           ]},
                                          {:->, [depth: 3],
                                           [
                                             [{:_, [], nil}],
                                             {{:., [], [{:infix, [], nil}]}, [],
                                              [
                                                {:next_token, [], [{:parser, [], nil}]},
                                                {:left, [], nil}
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
                  {:bar, [], [{:foo, [], nil}]},
                  [
                    do:
                      {:case, [],
                       [
                         {:foo, [], nil},
                         [
                           do: [
                             {:->, [depth: 2], [[:foo], :ok]},
                             {:->, [depth: 2],
                              [
                                [:bar],
                                {{:., [], [{:__aliases__, [], [:Enum]}, :map]}, [],
                                 [
                                   {:some_list, [], nil},
                                   {:fn, [],
                                    [
                                      {:->, [depth: 3],
                                       [
                                         [{:item, [], nil}],
                                         {{:., [], [{:item, [], nil}, :name]}, [], []}
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
                  {:foo, [], nil},
                  [
                    do: [
                      {:->, [depth: 1], [[:foo], :ok]},
                      {:->, [depth: 1],
                       [
                         [:bar],
                         {{:., [], [{:__aliases__, [], [:Enum]}, :map]}, [],
                          [
                            {:some_list, [], nil},
                            {:fn, [],
                             [
                               {:->, [depth: 2],
                                [
                                  [{:item, [], nil}],
                                  {{:., [], [{:item, [], nil}, :name]}, [], []}
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
                  {:parse_stab_expression, [], [{:parser, [], nil}, {:lhs, [], nil}]},
                  [
                    do:
                      {:case, [],
                       [
                         {:current_token, [], [{:parser, [], nil}]},
                         [
                           do: [
                             {:->, [depth: 2],
                              [
                                [:<-],
                                {:parse_infix_expression, [], [{:parser, [], nil}, {:lhs, [], nil}]}
                              ]},
                             {:->, [depth: 2],
                              [
                                [:->],
                                {:__block__, [],
                                 [
                                   {:=, [],
                                    [
                                      {:token, [], nil},
                                      {:current_token, [], [{:parser, [], nil}]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:current_sd, [], nil},
                                      {{:., [], [{:parser, [], nil}, :stab_depth]}, [], []}
                                    ]},
                                   {:=, [],
                                    [
                                      {:parser, [], nil},
                                      {:eat_at, [], [{:parser, [], nil}, :eol, 1]}
                                    ]},
                                   {:=, [], [{:exprs, [], nil}, []]},
                                   {:=, [],
                                    [
                                      {{:exprs, [], nil}, {:parser, [], nil}},
                                      {:while, [],
                                       [
                                         {:<-, [],
                                          [
                                            {:not, [],
                                             [
                                               {:in, [],
                                                [
                                                  {:peek_token, [], [{:parser, [], nil}]},
                                                  [:eof, :end]
                                                ]}
                                             ]},
                                            {{:exprs, [], nil}, {:parser, [], nil}}
                                          ]},
                                         [
                                           do:
                                             {:__block__, [],
                                              [
                                                {:=, [],
                                                 [
                                                   {:parser, [], nil},
                                                   {:next_token, [], [{:parser, [], nil}]}
                                                 ]},
                                                {:=, [],
                                                 [
                                                   {{:ast, [], nil}, {:parser, [], nil}},
                                                   {:parse_expression, [], [{:parser, [], nil}, [top: true]]}
                                                 ]},
                                                {:=, [],
                                                 [
                                                   {:parser, [], nil},
                                                   {:eat_at, [], [{:parser, [], nil}, :eol, 1]}
                                                 ]},
                                                {[
                                                   {:|, [], [{:ast, [], nil}, {:exprs, [], nil}]}
                                                 ], {:eat_eol, [], [{:parser, [], nil}]}}
                                              ]}
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:rhs, [], nil},
                                      {:case, [],
                                       [
                                         {:exprs, [], nil},
                                         [
                                           do: [
                                             {:->, [depth: 3], [[[{:ast, [], nil}]], {:ast, [], nil}]},
                                             {:->, [depth: 3],
                                              [
                                                [{:exprs, [], nil}],
                                                {:{}, [],
                                                 [
                                                   :__block__,
                                                   [],
                                                   {{:., [],
                                                     [
                                                       {:__aliases__, [], [:Enum]},
                                                       :reverse
                                                     ]}, [], [{:exprs, [], nil}]}
                                                 ]}
                                              ]}
                                           ]
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {{:rhs, [], nil}, {:stabs, [], nil}},
                                      {{:., [], [{:__aliases__, [], [:Macro]}, :traverse]}, [],
                                       [
                                         {:rhs, [], nil},
                                         [],
                                         {:fn, [],
                                          [
                                            {:->, [depth: 3],
                                             [
                                               [
                                                 {:node, [], nil},
                                                 {:acc, [], nil}
                                               ],
                                               {:case, [],
                                                [
                                                  {:node, [], nil},
                                                  [
                                                    do: [
                                                      {:->, [depth: 4],
                                                       [
                                                         [
                                                           {:{}, [],
                                                            [
                                                              :->,
                                                              {:meta, [], nil},
                                                              {:_args, [], nil}
                                                            ]}
                                                         ],
                                                         {:if, [],
                                                          [
                                                            {:==, [],
                                                             [
                                                               {{:., [], [Access, :get]}, [], [{:meta, [], nil}, :depth]},
                                                               {:current_sd, [], nil}
                                                             ]},
                                                            [
                                                              do:
                                                                {:__remove_me__,
                                                                 [
                                                                   {:|, [],
                                                                    [
                                                                      {:node, [], nil},
                                                                      {:acc, [], nil}
                                                                    ]}
                                                                 ]},
                                                              else: {{:node, [], nil}, {:acc, [], nil}}
                                                            ]
                                                          ]}
                                                       ]},
                                                      {:->, [depth: 4],
                                                       [
                                                         [{:_, [], nil}],
                                                         {{:node, [], nil}, {:acc, [], nil}}
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
                                                       {:node, [], nil},
                                                       {:meta, [], nil},
                                                       {:args, [], nil}
                                                     ]},
                                                    {:acc, [], nil},
                                                    {:is_list, [], [{:args, [], nil}]}
                                                  ]}
                                               ],
                                               {:__block__, [],
                                                [
                                                  {:=, [],
                                                   [
                                                     {:args, [], nil},
                                                     {{:., [],
                                                       [
                                                         {:__aliases__, [], [:Enum]},
                                                         :reject
                                                       ]}, [],
                                                      [
                                                        {:args, [], nil},
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
                                                      {:node, [], nil},
                                                      {:meta, [], nil},
                                                      {:args, [], nil}
                                                    ]}, {:acc, [], nil}}
                                                ]}
                                             ]},
                                            {:->, [depth: 3],
                                             [
                                               [
                                                 {:node, [], nil},
                                                 {:acc, [], nil}
                                               ],
                                               {{:node, [], nil}, {:acc, [], nil}}
                                             ]}
                                          ]}
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:rhs, [], nil},
                                      {:case, [],
                                       [
                                         {:rhs, [], nil},
                                         [
                                           do: [
                                             {:->, [depth: 3],
                                              [
                                                [
                                                  {:{}, [],
                                                   [
                                                     :__block__,
                                                     {:_, [], nil},
                                                     [{:ast, [], nil}]
                                                   ]}
                                                ],
                                                {:ast, [], nil}
                                              ]},
                                             {:->, [depth: 3], [[[{:ast, [], nil}]], {:ast, [], nil}]},
                                             {:->, [depth: 3], [[{:block, [], nil}], {:block, [], nil}]}
                                           ]
                                         ]
                                       ]}
                                    ]},
                                   {:=, [],
                                    [
                                      {:ast, [], nil},
                                      {:++, [],
                                       [
                                         [
                                           {:{}, [],
                                            [
                                              {:token, [], nil},
                                              [
                                                depth: {{:., [], [{:parser, [], nil}, :stab_depth]}, [], []}
                                              ],
                                              [
                                                {:wrap, [], [{:lhs, [], nil}]},
                                                {:rhs, [], nil}
                                              ]
                                            ]}
                                         ],
                                         {{:., [], [{:__aliases__, [], [:Enum]}, :reverse]}, [], [{:stabs, [], nil}]}
                                       ]}
                                    ]},
                                   {{:ast, [], nil}, {:eat_eol, [], [{:parser, [], nil}]}}
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
                      {:ok, {:_, [line: 1, column: 12], nil}},
                      {:bar, [closing: [line: 5, column: 11], line: 1, column: 18],
                       [
                         {:fn, [closing: [line: 5, column: 8], line: 1, column: 22],
                          [
                            {:->, [newlines: 1, depth: 1, line: 1, column: 27],
                             [
                               [{:a, [line: 1, column: 25], nil}],
                               {:with,
                                [
                                  do: [line: 2, column: 23],
                                  end: [line: 4, column: 10],
                                  line: 2,
                                  column: 10
                                ],
                                [
                                  {:<-, [line: 2, column: 18], [:d, {:b, [line: 2, column: 21], nil}]},
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
                   {:char, [line: 1, column: 7], nil},
                   {:"::", [line: 1, column: 17],
                    [
                      {:rest, [line: 1, column: 13], nil},
                      {:binary, [line: 1, column: 19], nil}
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
                              {:->, [line: 1, column: 19], [[], {:term, [line: 1, column: 22], nil}]}
                            ],
                            {{:., [line: 1, column: 38],
                              [
                                {:__aliases__, [last: [line: 1, column: 29], line: 1, column: 29], [:GenServer]},
                                :options
                              ]}, [closing: [line: 1, column: 47], line: 1, column: 39], []}
                          ]},
                         {:on_start, [line: 1, column: 53], nil}
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
                               {:agent, [line: 1, column: 11], nil},
                               [
                                 {:->, [depth: 0, line: 1, column: 25],
                                  [
                                    [{:state, [line: 1, column: 19], nil}],
                                    {:a, [line: 1, column: 28], nil}
                                  ]}
                               ],
                               {:timeout, [line: 1, column: 32], nil}
                             ]},
                            {:a, [line: 1, column: 44], nil}
                          ]},
                         [a: {:var, [line: 1, column: 54], nil}]
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
                            {:mod, [line: 1, column: 10], nil},
                            {:%{}, [closing: [line: 1, column: 14], line: 1, column: 13], []}
                          ]},
                         {:bar, [line: 1, column: 18], nil}
                       ]}
                    ]},
                   [do: :ok]
                 ]}}
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

      assert Spitfire.parse(code) ==
               {:ok,
                {:try, [do: [line: 1, column: 5], end: [line: 9, column: 1], line: 1, column: 1],
                 [
                   [
                     do: {:foo, [closing: [line: 2, column: 7], line: 2, column: 3], []},
                     rescue: [
                       {:->, [newlines: 1, depth: 1, line: 4, column: 10],
                        [
                          [
                            {:in, [line: 4, column: 5],
                             [
                               {:e, [line: 4, column: 3], nil},
                               {:__aliases__, [last: [line: 4, column: 8], line: 4, column: 8], [:E]}
                             ]}
                          ],
                          {:bar, [closing: [line: 5, column: 9], line: 5, column: 5], []}
                        ]}
                     ],
                     else: [
                       {:->, [depth: 1, line: 7, column: 16],
                        [
                          [ok: {:value, [line: 7, column: 9], nil}],
                          {:value,
                           [
                             end_of_expression: [newlines: 1, line: 7, column: 24],
                             line: 7,
                             column: 19
                           ], nil}
                        ]},
                       {:->, [depth: 1, line: 8, column: 10], [[:error], {:default, [line: 8, column: 13], nil}]}
                     ]
                   ]
                 ]}}
    end

    test "starts with a comment" do
      code = """
      # hi there
      some_code = :foo
      """

      assert Spitfire.parse(code) ==
               {:ok, {:=, [line: 2, column: 11], [{:some_code, [line: 2, column: 1], nil}, :foo]}}
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
                    [{:\\, [line: 1, column: 13], [{:arg, [line: 1, column: 9], nil}, :value]}]},
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
             ], [{:four, [line: 4, column: 2], nil}]},
            {:try, [do: [line: 5, column: 5], end: [line: 10, column: 1], line: 5, column: 1],
             [
               [
                 {{:__literal__, [line: 5, column: 5], :do}, {:__literal__, [line: 6, column: 3], :ok}},
                 {{:__literal__, [line: 7, column: 1], :rescue},
                  [
                    {:->, [newlines: 1, depth: 1, line: 8, column: 5],
                     [
                       [{:_, [line: 8, column: 3], nil}],
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
          {{:one, [line: 1, column: 2], nil}, {:two, [line: 1, column: 7], nil}}}}
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
                   ], [{:world, [line: 1, column: 11], nil}]},
                  {:binary, [line: 1, column: 9], nil}
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
                      ], [{:alice, [line: 1, column: 7], nil}]},
                     {:binary, [line: 1, column: 5], nil}
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
                     {:binary, [line: 1, column: 5], nil}
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
                      ], [{:alice, [line: 2, column: 6], nil}]},
                     {:binary, [line: 2, column: 4], nil}
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
                       ], [{:foo, [line: 1, column: 4], nil}]},
                      {:binary, [line: 1, column: 2], nil}
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
                    ], [{:bar, [line: 3, column: 5], nil}]},
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
               [{:foo, [line: 1, column: 4], nil}],
               {:__block__, [],
                [
                  {:send,
                   [
                     end_of_expression: [newlines: 2, line: 2, column: 16],
                     line: 2,
                     column: 3
                   ], [{:foo, [line: 2, column: 8], nil}, :hi]},
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
            {:one, [line: 1, column: 2], nil},
            {:two, [line: 1, column: 7], nil},
            {:three, [line: 1, column: 12], nil}
          ]}},
        {~S'%{}', {:%{}, [closing: [line: 1, column: 3], line: 1, column: 2], []}},
        {~S'%{"one" => two, three: 4}',
         {:%{}, [closing: [line: 1, column: 25], line: 1, column: 2],
          [{"one", {:two, [line: 1, column: 12], nil}}, {:three, 4}]}},
        {~S'foo()', {:foo, [closing: [line: 1, column: 5], line: 1, column: 1], []}},
        {~S'foo(bar)', {:foo, [closing: [line: 1, column: 8], line: 1, column: 1], [{:bar, [line: 1, column: 5], nil}]}}
      ]

      for {code, expected} <- codes do
        assert Spitfire.parse(code) == {:ok, expected}
      end
    end

    test "parses special keywords" do
      codes = [
        {"__MODULE__", {:__MODULE__, [line: 1, column: 1], nil}}
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
                                          [{:attrs, [line: 7, column: 13], nil}]},
                                         [
                                           do: {
                                             :struct,
                                             [closing: [line: 8, column: 34], line: 8, column: 7],
                                             [
                                               {
                                                 :%,
                                                 [line: 8, column: 14],
                                                 [
                                                   {:__MODULE__, [line: 8, column: 15], nil},
                                                   {:%{}, [closing: [line: 8, column: 26], line: 8, column: 25], []}
                                                 ]
                                               },
                                               {:attrs, [line: 8, column: 29], nil}
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
                                   {:foo, [line: 13, column: 11], nil},
                                   {:bar, [line: 13, column: 16], nil},
                                   {:baz, [line: 13, column: 21], nil}
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
                   {:bar, [{:closing, [line: 3, column: 8]}, line: 3, column: 1], [{:two, [line: 3, column: 5], nil}]}
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
                 [line: 1, column: 10],
                 [
                   {:new_list, [line: 1, column: 1], nil},
                   {
                     {:., [line: 2, column: 7],
                      [{:__aliases__, [{:last, [line: 2, column: 3]}, {:line, 2}, {:column, 3}], [:Enum]}, :map]},
                     [{:closing, [line: 5, column: 19]}, {:line, 2}, {:column, 8}],
                     [
                       {:some_list, [line: 2, column: 12], nil},
                       {
                         :fn,
                         [closing: [line: 5, column: 19], line: 2, column: 23],
                         [
                           {
                             :->,
                             [newlines: 3, depth: 1, line: 2, column: 31],
                             [
                               [{:item, [line: 2, column: 26], nil}],
                               {
                                 :send,
                                 [closing: [line: 5, column: 19], line: 5, column: 1],
                                 [{:pid, [line: 5, column: 6], nil}, {:new_list, [line: 5, column: 11], nil}]
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
                               {:bat, [line: 4, column: 7], nil},
                               [
                                 do: {
                                   :__block__,
                                   [],
                                   [
                                     {
                                       :=,
                                       [end_of_expression: [newlines: 1, line: 5, column: 14], line: 5, column: 9],
                                       [{:var, [line: 5, column: 5], nil}, 123]
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
                             [{:local_function, [line: 9, column: 7], nil}, [do: {:__block__, [], []}]]
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
                               {:bat, [line: 4, column: 7], nil},
                               [
                                 do: {
                                   :__block__,
                                   [],
                                   [
                                     {
                                       :=,
                                       [end_of_expression: [newlines: 1, line: 5, column: 14], line: 5, column: 9],
                                       [{:var, [line: 5, column: 5], nil}, 123]
                                     },
                                     {:{}, [closing: nil, line: 6, column: 5], [{:var, [line: 6, column: 6], nil}]}
                                   ]
                                 }
                               ]
                             ]
                           },
                           {
                             :def,
                             [do: [line: 9, column: 22], end: [line: 11, column: 3], line: 9, column: 3],
                             [{:local_function, [line: 9, column: 7], nil}, [do: {:__block__, [], []}]]
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
                             {:bat, [line: 4, column: 7], nil},
                             [
                               do: {
                                 :__block__,
                                 [],
                                 [
                                   {
                                     :=,
                                     [end_of_expression: [newlines: 1, line: 5, column: 14], line: 5, column: 9],
                                     [{:var, [line: 5, column: 5], nil}, 123]
                                   },
                                   [{:var, [line: 6, column: 6], nil}]
                                 ]
                               }
                             ]
                           ]
                         },
                         {
                           :def,
                           [do: [line: 9, column: 22], end: [line: 11, column: 3], line: 9, column: 3],
                           [{:local_function, [line: 9, column: 7], nil}, [do: {:__block__, [], []}]]
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
