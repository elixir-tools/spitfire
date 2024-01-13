defmodule SpitfireTest do
  use ExUnit.Case

  doctest Spitfire

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

    assert Spitfire.parse(code) ==
             {:defmodule, [],
              [
                {:__aliases__, [], [:Foo]},
                [
                  do:
                    {:__block__, [],
                     [
                       {:use, [],
                        [
                          {:__aliases__, [], [:AnotherMod, :Nested]},
                          [some: :option]
                        ]},
                       {:def, [],
                        [
                          {:run, [], [{:arg, [], Elixir}]},
                          [do: {:__block__, [], [{:bar, [], []}, :ok]}]
                        ]}
                     ]}
                ]
              ]}
  end

  test "access syntax" do
    code = "foo[:bar]"

    assert Spitfire.parse(code) == {{:., [], [Access, :get]}, [], [{:foo, [], Elixir}, :bar]}

    code = "%{bar: :foo}[:bar]"

    assert Spitfire.parse(code) ==
             {{:., [], [Access, :get]}, [], [{:%{}, [], [bar: :foo]}, :bar]}
  end

  test "literal encoder" do
    code = ~S'''
    1
    "two"
    :three
    [four]
    '''

    assert Spitfire.parse(code, literal_encoder: fn l, m -> {:ok, {:__literal__, m, l}} end) ==
             {:__block__, [],
              [
                {:__literal__, [line: 1, column: 1], 1},
                {:__literal__, [line: 2, column: 1], "two"},
                {:__literal__, [line: 3, column: 1], :three},
                {:__literal__, [line: 4, column: 1], [{:four, [], Elixir}]}
              ]}
  end

  test "type syntax" do
    code = ~S'''
    @type foo :: String.t()
    '''

    assert Spitfire.parse(code) ==
             {:@, [],
              [
                {:type, [],
                 [
                   {:"::", [],
                    [
                      {:foo, [], Elixir},
                      {{:., [], [{:__aliases__, [], [:String]}, :t]}, [], []}
                    ]}
                 ]}
              ]}

    code = ~S'''
    @spec foo(one :: String.t(), number) :: :ok | :error
    '''

    assert Spitfire.parse(code) ==
             {:@, [],
              [
                {:spec, [],
                 [
                   {:"::", [],
                    [
                      {:foo, [],
                       [
                         {:"::", [],
                          [
                            {:one, [], Elixir},
                            {{:., [], [{:__aliases__, [], [:String]}, :t]}, [], []}
                          ]},
                         {:number, [], Elixir}
                       ]},
                      {:|, [], [:ok, :error]}
                    ]}
                 ]}
              ]}
  end

  test "parses unary operators" do
    code = ~S'''
    ^foo
    '''

    assert Spitfire.parse(code) == {:^, [], [{:foo, [], Elixir}]}
  end

  test "parses numbers" do
    code = """
    111_111
    """

    assert Spitfire.parse(code) == 111_111
  end

  test "parses strings" do
    code = ~s'''
    "foobar" 
    '''

    assert Spitfire.parse(code) == "foobar"
  end

  test "parses string interpolation" do
    code = ~S'''
    "foo#{alice}bar"
    '''

    assert Spitfire.parse(code) ==
             {:<<>>, [],
              [
                "foo",
                {:"::", [],
                 [
                   {{:., [], [Kernel, :to_string]}, [], [{:alice, [], Elixir}]},
                   {:binary, [], Elixir}
                 ]},
                "bar"
              ]}

    code = ~S'''
    "foo#{}bar"
    '''

    assert Spitfire.parse(code) ==
             {:<<>>, [],
              [
                "foo",
                {:"::", [],
                 [
                   {{:., [], [Kernel, :to_string]}, [], [{:__block__, [], []}]},
                   {:binary, [], Elixir}
                 ]},
                "bar"
              ]}
  end

  test "parses atoms" do
    code = ~s'''
    :foobar
    '''

    assert Spitfire.parse(code) == :foobar

    code = ~s'''
    :","
    '''

    assert Spitfire.parse(code) == :","

    code = ~S'''
    :"foo#{}"
    '''

    assert Spitfire.parse(code) ==
             {{:., [], [:erlang, :binary_to_atom]}, [],
              [
                {:<<>>, [],
                 [
                   "foo",
                   {:"::", [],
                    [
                      {{:., [], [Kernel, :to_string]}, [], [{:__block__, [], []}]},
                      {:binary, [], Elixir}
                    ]}
                 ]},
                :utf8
              ]}

    code = ~S'''
    :"foo#{bar}"
    '''

    assert Spitfire.parse(code) ==
             {{:., [], [:erlang, :binary_to_atom]}, [],
              [
                {:<<>>, [],
                 [
                   "foo",
                   {:"::", [],
                    [
                      {{:., [], [Kernel, :to_string]}, [], [{:bar, [], Elixir}]},
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

    assert Spitfire.parse(code) == {:<-, [], [{:apple, [], Elixir}, {:apples, [], Elixir}]}
  end

  test "parses right stab" do
    code = """
    -> bar
    """

    assert Spitfire.parse(code) == [{:->, [], [[], {:bar, [], Elixir}]}]

    code = """
    -> :ok
    """

    assert Spitfire.parse(code) == [{:->, [], [[], :ok]}]

    code = """
    foo -> bar
    """

    assert Spitfire.parse(code) == [{:->, [depth: 0], [[{:foo, [], Elixir}], {:bar, [], Elixir}]}]

    code = """
    foo, bar, baz -> bar
    """

    assert Spitfire.parse(code) == [
             {:->, [depth: 0],
              [
                [{:foo, [], Elixir}, {:bar, [], Elixir}, {:baz, [], Elixir}],
                {:bar, [], Elixir}
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
    assert Spitfire.parse(code) == [
             {:->, [depth: 0],
              [
                [{:alice, [], Elixir}, {:bob, [], Elixir}, {:carol, [], Elixir}],
                {:__block__, [], [:error, {:bar, [], Elixir}]}
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

    assert Spitfire.parse(code) == [
             {:->, [depth: 0], [[{:foo, [], Elixir}], {:__block__, [], [:ok, {:baz, [], Elixir}]}]},
             {:->, [depth: 0],
              [
                [{:alice, [], Elixir}, {:bob, [], Elixir}, {:carol, [], Elixir}],
                {:__block__, [], [:error, {:bar, [], Elixir}]}
              ]}
           ]

    code = ~S'''
    ^foo ->
      :ok
    '''

    assert Spitfire.parse(code) == [{:->, [depth: 0], [[{:^, [], [{:foo, [], Elixir}]}], :ok]}]

    code = ~S'''
    @foo ->
      :ok
    '''

    assert Spitfire.parse(code) == [{:->, [depth: 0], [[{:@, [], [{:foo, [], Elixir}]}], :ok]}]
  end

  test "parses grouped expressions" do
    codes = [
      {~s'''
       1 + 2 + 3
       ''', {:+, [], [{:+, [], [1, 2]}, 3]}},
      {~s'''
       (1 + 2) + 3
       ''', {:+, [], [{:+, [], [1, 2]}, 3]}},
      {~s'''
       ((1 + 2) + 3)
       ''', {:+, [], [{:+, [], [1, 2]}, 3]}},
      {~s'''
       1 + (2 + 3)
       ''', {:+, [], [1, {:+, [], [2, 3]}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses for comprehension" do
    codes = [
      {~s'''
       for i <- 0..100 do
         i + i
       end
       ''',
       {:for, [],
        [
          {:<-, [], [{:i, [], Elixir}, {:.., [], [0, 100]}]},
          [do: {:+, [], [{:i, [], Elixir}, {:i, [], Elixir}]}]
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
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
       {:with, [],
        [
          {:<-, [],
           [
             {:ok, {:school, [], Elixir}},
             {{:., [], [{:__aliases__, [], [:State]}, :get_school]}, [], [{:id, [], Elixir}]}
           ]},
          {:<-, [],
           [
             {:ok, {:teachers, [], Elixir}},
             {{:., [], [{:__aliases__, [], [:School]}, :list_teachers]}, [], [{:school, [], Elixir}]}
           ]},
          {:<-, [],
           [
             {:ok, {:teacher, [], Elixir}},
             {{:., [], [{:__aliases__, [], [:Teacher]}, :coolest]}, [], [{:teachers, [], Elixir}]}
           ]},
          [
            do:
              {{:., [], [{:__aliases__, [], [:Email]}, :send]}, [],
               [{:teacher, [], Elixir}, "You are the coolest teacher"]}
          ]
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses variable identifiers" do
    code = ~s'''
    foobar
    alice
    bob
    '''

    assert Spitfire.parse(code) ==
             {:__block__, [],
              [
                {:foobar, [], Elixir},
                {:alice, [], Elixir},
                {:bob, [], Elixir}
              ]}
  end

  test "parses lists" do
    codes = [
      {~s'''
       []
       ''', []},
      {~s'''
       [arg]
       ''', [{:arg, [], Elixir}]},
      {~s'''
        [one, :two, "three"]
       ''', [{:one, [], Elixir}, :two, "three"]},
      {~s'''
        [
          one,
          :two,
          "three"
        ]
       ''', [{:one, [], Elixir}, :two, "three"]}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses bracket-less keyword lists" do
    codes = [
      {~s'''
       foo(one, two, alice: alice, bob: bob)
       ''', {:foo, [], [{:one, [], Elixir}, {:two, [], Elixir}, [alice: {:alice, [], Elixir}, bob: {:bob, [], Elixir}]]}},
      {~s'''
       foo alice: alice do
         :ok
       end
       ''', {:foo, [], [[alice: {:alice, [], Elixir}], [do: :ok]]}},
      {~s'''
       [:one, two: :three]
       ''', [:one, {:two, :three}]}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "another thing" do
    code = ~S'''
    case foo do
      :kw_identifier when is_list or is_map -> &parse_kw_identifier/1
    end
    '''

    assert Spitfire.parse(code) ==
             {:case, [],
              [
                {:foo, [], Elixir},
                [
                  do: [
                    {:->, [depth: 1],
                     [
                       [
                         {:when, [],
                          [
                            :kw_identifier,
                            {:or, [], [{:is_list, [], Elixir}, {:is_map, [], Elixir}]}
                          ]}
                       ],
                       {:&, [], [{:/, [], [{:parse_kw_identifier, [], Elixir}, 1]}]}
                     ]}
                  ]
                ]
              ]}
  end

  test "parses pattern matching in list" do
    codes = [
      {~s'''
       [one | rest] = my_list
       ''', {:=, [], [[{:|, [], [{:one, [], Elixir}, {:rest, [], Elixir}]}], {:my_list, [], Elixir}]}},
      {~s'''
       [one, two | rest] = my_list
       ''',
       {:=, [],
        [
          [{:one, [], Elixir}, {:|, [], [{:two, [], Elixir}, {:rest, [], Elixir}]}],
          {:my_list, [], Elixir}
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses tuples" do
    codes = [
      {~s'''
       {}
       ''', {:{}, [], []}},
      {~s'''
        {one, :two}
       ''', {{:one, [], Elixir}, :two}},
      {~s'''
        {
          one,
          :two,
          "three"
        }
       ''', {:{}, [], [{:one, [], Elixir}, :two, "three"]}},
      {~s'''
        {one, :two, "three"}
       ''', {:{}, [], [{:one, [], Elixir}, :two, "three"]}},
      {~s'''
        {
          one,
          :two,
          "three"
        }
       ''', {:{}, [], [{:one, [], Elixir}, :two, "three"]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses aliases" do
    codes = [
      {~s'''
       Remote
       ''', {:__aliases__, [], [:Remote]}},
      {~s'''
        Remote.Foo
       ''', {:__aliases__, [], [:Remote, :Foo]}},
      {~s'''
        Remote.Foo.Bar
       ''', {:__aliases__, [], [:Remote, :Foo, :Bar]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses maps" do
    codes = [
      {~s'''
       %{}
       ''', {:%{}, [], []}},
      {~s'''
       %{
         foo: "bar",
         alice: "bob"
        }
       ''', {:%{}, [], [{:foo, "bar"}, {:alice, "bob"}]}},
      {~s'''
       %{
         "foo" => "bar",
         "alice" => "bob"
        }
       ''', {:%{}, [], [{"foo", "bar"}, {"alice", "bob"}]}},
      {~s'''
        %{"foo" => "bar", 1 => 2, :three => :four, [] => [1], %{} => nil, bing => bong, foo: :bar}
       ''',
       {:%{}, [],
        [
          {"foo", "bar"},
          {1, 2},
          {:three, :four},
          {[], [1]},
          {{:%{}, [], []}, nil},
          {{:bing, [], Elixir}, {:bong, [], Elixir}},
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
       {:%{}, [],
        [
          {"foo", "bar"},
          {1, 2},
          {:three, :four},
          {[], [1]},
          {{:%{}, [], []}, nil},
          {{:bing, [], Elixir}, {:bong, [], Elixir}},
          {:foo, :bar}
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses operators" do
    codes = [
      {~s'''
       1 + 2
       ''', {:+, [], [1, 2]}},
      {~s'''
       1 - 2
       ''', {:-, [], [1, 2]}},
      {~s'''
       1 * 2
       ''', {:*, [], [1, 2]}},
      {~s'''
       1 / 2
       ''', {:/, [], [1, 2]}},
      {~s'''
       1 || foo()
       ''', {:||, [], [1, {:foo, [], []}]}},
      {~s'''
       1 ||| foo()
       ''', {:|||, [], [1, {:foo, [], []}]}},
      {~s'''
       1 or foo()
       ''', {:or, [], [1, {:foo, [], []}]}},
      {~s'''
       1 == foo()
       ''', {:==, [], [1, {:foo, [], []}]}},
      {~s'''
       1 != foo()
       ''', {:!=, [], [1, {:foo, [], []}]}},
      {~s'''
       1 =~ foo()
       ''', {:=~, [], [1, {:foo, [], []}]}},
      {~s'''
       1 === foo()
       ''', {:===, [], [1, {:foo, [], []}]}},
      {~s'''
       1 !== foo()
       ''', {:!==, [], [1, {:foo, [], []}]}},
      {~s'''
       1 < foo()
       ''', {:<, [], [1, {:foo, [], []}]}},
      {~s'''
       1 > foo()
       ''', {:>, [], [1, {:foo, [], []}]}},
      {~s'''
       1 <= foo()
       ''', {:<=, [], [1, {:foo, [], []}]}},
      {~s'''
       1 >= foo()
       ''', {:>=, [], [1, {:foo, [], []}]}},
      {~s'''
       1 |> foo()
       ''', {:|>, [], [1, {:foo, [], []}]}},
      {~s'''
       1 <|> foo()
       ''', {:"<|>", [], [1, {:foo, [], []}]}},
      {~s'''
       1 <<< foo()
       ''', {:<<<, [], [1, {:foo, [], []}]}},
      {~s'''
       1 >>> foo()
       ''', {:>>>, [], [1, {:foo, [], []}]}},
      {~s'''
       1 <<~ foo()
       ''', {:<<~, [], [1, {:foo, [], []}]}},
      {~s'''
       1 ~>> foo()
       ''', {:~>>, [], [1, {:foo, [], []}]}},
      {~s'''
       1 <~ foo()
       ''', {:<~, [], [1, {:foo, [], []}]}},
      {~s'''
       1 ~> foo()
       ''', {:~>, [], [1, {:foo, [], []}]}},
      {~s'''
       1 <~> foo()
       ''', {:<~>, [], [1, {:foo, [], []}]}},
      {~s'''
       1 in foo()
       ''', {:in, [], [1, {:foo, [], []}]}},
      {~s'''
       foo not in bar
       ''',
       {:not, [],
        [
          {:in, [], [{:foo, [], Elixir}, {:bar, [], Elixir}]}
        ]}},
      {~s'''
       1 ^^^ foo()
       ''', {:"^^^", [], [1, {:foo, [], []}]}},
      {~s'''
       1 + 2 * 3 - 2
       ''',
       {:-, [],
        [
          {:+, [], [1, {:*, [], [2, 3]}]},
          2
        ]}},
      {~s'''
       one..two
       ''', {:.., [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
      {~s'''
       one..two//2
       ''', {:"..//", [], [{:one, [], Elixir}, {:two, [], Elixir}, 2]}},
      {~s'''
       one <> two
       ''', {:<>, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
      {~s'''
       one ++ two
       ''', {:++, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
      {~s'''
       one -- two
       ''', {:--, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
      {~s'''
       one +++ two
       ''', {:+++, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
      {~s'''
       one --- two
       ''', {:---, [], [{:one, [], Elixir}, {:two, [], Elixir}]}},
      {~s'''
       one ++ two ++ three
       ''',
       {:++, [],
        [
          {:one, [], Elixir},
          {:++, [], [{:two, [], Elixir}, {:three, [], Elixir}]}
        ]}},
      {~s'''
       @foo
       ''', {:@, [], [{:foo, [], Elixir}]}},
      {~s'''
       !foo
       ''', {:!, [], [{:foo, [], Elixir}]}},
      {~s'''
       not foo
       ''', {:not, [], [{:foo, [], Elixir}]}},
      {~s'''
       ^foo
       ''', {:^, [], [{:foo, [], Elixir}]}},
      {~s'''
       ~~~foo
       ''', {:"~~~", [], [{:foo, [], Elixir}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses setting module attr" do
    codes = [
      {~s'''
       @foo bar()
       ''', {:@, [], [{:foo, [], [{:bar, [], []}]}]}},
      {~s'''
       @foo %{
         foo: :bar
       }
       ''', {:@, [], [{:foo, [], [{:%{}, [], [foo: :bar]}]}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parse do block" do
    codes = [
      {~s'''
       foo do
       end
       ''', {:foo, [], [[do: {:__block__, [], []}]]}},
      {~s'''
       foo do
        "howdy"
        :partner
       end
       ''',
       {:foo, [],
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
       {:foo, [],
        [
          {:arg, [], Elixir},
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
       ''', {:if, [], [{:arg, [], Elixir}, [do: "howdy", else: :partner]]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
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
       {:case, [],
        [
          {:foo, [], Elixir},
          [do: [{:->, [depth: 1], [[{:bar, [], Elixir}], {:bar, [], Elixir}]}]]
        ]}},
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
       {:case, [],
        [
          :foo,
          [
            do: [
              {:->, [depth: 1],
               [
                 [:foo],
                 {:case, [],
                  [
                    {:get, [], [:foo]},
                    [
                      do: [
                        {:->, [depth: 2], [[:FOO], :bar]},
                        {:->, [depth: 2], [[{:_, [], Elixir}], :error]}
                      ]
                    ]
                  ]}
               ]},
              {:->, [depth: 1], [[{:_, [], Elixir}], :error]}
            ]
          ]
        ]}},
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
       {:case, [],
        [
          {:infix, [], Elixir},
          [
            do: [
              {:->, [depth: 1], [[nil], {{:left, [], Elixir}, {:parser, [], Elixir}}]},
              {:->, [depth: 1],
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
              {:->, [depth: 1],
               [
                 [{:_, [], Elixir}],
                 {{:., [], [{:infix, [], Elixir}]}, [], [{:next_token, [], [{:parser, [], Elixir}]}, {:left, [], Elixir}]}
               ]}
            ]
          ]
        ]}}
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
       {:a, [],
        [
          {:b, [],
           [
             {:c, [], Elixir},
             {:d, [], Elixir}
           ]}
        ]}},
      {~s'''
       a b c, d do
       end
       ''',
       {:a, [],
        [
          {:b, [],
           [
             {:c, [], Elixir},
             {:d, [], Elixir}
           ]},
          [do: {:__block__, [], []}]
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses function calls" do
    codes = [
      {~s'''
       foo()
       ''', {:foo, [], []}},
      {~s'''
       foo(arg, arg2)
       ''', {:foo, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       foo(
         arg,
         arg2
       )
       ''', {:foo, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       foo arg, arg2
       ''', {:foo, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       Remote.foo
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], []}},
      {~s'''
       Remote.foo()
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], []}},
      {~s'''
       Remote.foo(arg, arg2)
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       Remote.foo(
         arg,
         arg2
       )
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       Remote.foo arg, arg2
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       :erlang.foo
       ''', {{:., [], [:erlang, :foo]}, [], []}},
      {~s'''
       :erlang.foo()
       ''', {{:., [], [:erlang, :foo]}, [], []}},
      {~s'''
       :erlang.foo(arg, arg2)
       ''', {{:., [], [:erlang, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       :erlang.foo arg, arg2
       ''', {{:., [], [:erlang, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       somevar.foo
       ''', {{:., [], [{:somevar, [], Elixir}, :foo]}, [], []}},
      {~s'''
       somevar.foo()
       ''', {{:., [], [{:somevar, [], Elixir}, :foo]}, [], []}},
      {~s'''
       :elixir_tokenizer.tokenize(String.to_charlist(code), 1, [])
       ''',
       {{:., [], [:elixir_tokenizer, :tokenize]}, [],
        [
          {{:., [], [{:__aliases__, [], [:String]}, :to_charlist]}, [], [{:code, [], Elixir}]},
          1,
          []
        ]}},
      {~s'''
       somevar.foo(arg, arg2)
       ''', {{:., [], [{:somevar, [], Elixir}, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}},
      {~s'''
       somevar.foo arg, arg2
       ''', {{:., [], [{:somevar, [], Elixir}, :foo]}, [], [{:arg, [], Elixir}, {:arg2, [], Elixir}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses anon functions" do
    codes = [
      {~s'''
       fn -> :ok end
       ''', {:fn, [], [{:->, [], [[], :ok]}]}},
      {~s'''
       fn ->
         :ok
       end
       ''', {:fn, [], [{:->, [], [[], :ok]}]}},
      {~s'''
       fn one ->
         one
       end
       ''', {:fn, [], [{:->, [depth: 1], [[{:one, [], Elixir}], {:one, [], Elixir}]}]}},
      {~s'''
       fn
        one ->
         one
       end
       ''', {:fn, [], [{:->, [depth: 1], [[{:one, [], Elixir}], {:one, [], Elixir}]}]}},
      {~s'''
       fn(one) ->
         one
       end
       ''', {:fn, [], [{:->, [depth: 1], [[{:one, [], Elixir}], {:one, [], Elixir}]}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses match operator" do
    codes = [
      {~s'''
       foo = :bar
       ''', {:=, [], [{:foo, [], Elixir}, :bar]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses nil" do
    code = "nil"
    assert Spitfire.parse(code) == nil
  end

  test "parses booleans" do
    code = "false"
    assert Spitfire.parse(code) == false

    code = "true"
    assert Spitfire.parse(code) == true
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
       {:cond, [],
        [
          [
            do: [
              {:->, [depth: 1], [[{:==, [], [{:prefix, [], Elixir}, nil]}], :foo]},
              {:->, [depth: 1], [[true], :bar]}
            ]
          ]
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "|> operator" do
    code = ~S'''
    def parse(code) do
      parser = code |> new() |> next_token() |> next_token()

      parse_program(parser)
    end
    '''

    assert Spitfire.parse(code) ==
             {:def, [],
              [
                {:parse, [], [{:code, [], Elixir}]},
                [
                  do:
                    {:__block__, [],
                     [
                       {:=, [],
                        [
                          {:parser, [], Elixir},
                          {:|>, [],
                           [
                             {:|>, [],
                              [
                                {:|>, [], [{:code, [], Elixir}, {:new, [], []}]},
                                {:next_token, [], []}
                              ]},
                             {:next_token, [], []}
                           ]}
                        ]},
                       {:parse_program, [], [{:parser, [], Elixir}]}
                     ]}
                ]
              ]}
  end

  test "when operator" do
    codes = [
      {~s'''
       foo when is_binary(foo) ->
         :ok
       ''', [{:->, [depth: 0], [[{:when, [], [{:foo, [], Elixir}, {:is_binary, [], [{:foo, [], Elixir}]}]}], :ok]}]},
      {~s'''
       foo when is_binary(foo) ->
         :ok

       bar when is_number(bar) ->
         :ok
       ''',
       [
         {:->, [depth: 0], [[{:when, [], [{:foo, [], Elixir}, {:is_binary, [], [{:foo, [], Elixir}]}]}], :ok]},
         {:->, [depth: 0], [[{:when, [], [{:bar, [], Elixir}, {:is_number, [], [{:bar, [], Elixir}]}]}], :ok]}
       ]},
      {~s'''
       def foo(bar) when is_binary(bar) do
         :ok
       end
       ''',
       {:def, [], [{:when, [], [{:foo, [], [{:bar, [], Elixir}]}, {:is_binary, [], [{:bar, [], Elixir}]}]}, [do: :ok]]}},
      {~s'''
       fn foo when is_binary(foo) ->
         :ok
       end
       ''',
       {:fn, [], [{:->, [depth: 1], [[{:when, [], [{:foo, [], Elixir}, {:is_binary, [], [{:foo, [], Elixir}]}]}], :ok]}]}},
      {~s'''
       fn foo, bar, _baz when is_binary(foo) and bar in [:alice, :bob] ->
         :ok
       end
       ''',
       {:fn, [],
        [
          {:->, [depth: 1],
           [
             [
               {:when, [],
                [
                  {:foo, [], Elixir},
                  {:bar, [], Elixir},
                  {:_baz, [], Elixir},
                  {:and, [],
                   [
                     {:is_binary, [], [{:foo, [], Elixir}]},
                     {:in, [], [{:bar, [], Elixir}, [:alice, :bob]]}
                   ]}
                ]}
             ],
             :ok
           ]}
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "capture operator" do
    codes = [
      {~s'''
       &foo/1
       ''', {:&, [], [{:/, [], [{:foo, [], Elixir}, 1]}]}},
      {~s'''
       &Foo.foo/1
       ''', {:&, [], [{:/, [], [{{:., [], [{:__aliases__, [], [:Foo]}, :foo]}, [], []}, 1]}]}},
      {~s'''
       & &1
       ''', {:&, [], [{:&, [], [1]}]}},
      {~s'''
       &Foo.bar(one, &1)
       ''',
       {:&, [],
        [
          {{:., [], [{:__aliases__, [], [:Foo]}, :bar]}, [], [{:one, [], Elixir}, {:&, [], [1]}]}
        ]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
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
      assert Spitfire.parse(code) == expected
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

    assert Spitfire.parse(code) ==
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

    assert Spitfire.parse(code) ==
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

    assert Spitfire.parse(code) ==
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

    assert Spitfire.parse(code) ==
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
