defmodule SpitfireTest do
  use ExUnit.Case
  doctest Spitfire

  test "parses valid elixir" do
    code = """
    defmodule Foo do
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
                       {:def, [],
                        [
                          {:run, [], [{:arg, [], []}]},
                          [do: {:__block__, [], [{:bar, [], []}, :ok]}]
                        ]}
                     ]}
                ]
              ]}
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

  test "parses anon functions" do
    codes = [
      {~s'''
       fn -> :ok end
       ''', {:fn, [], [{:->, [], [[], {:__block__, [], [:ok]}]}]}},
      {~s'''
       fn ->
         :ok
       end
       ''', {:fn, [], [{:->, [], [[], {:__block__, [], [:ok]}]}]}},
      {~s'''
       fn one ->
         one
       end
       ''', {:fn, [], [{:->, [], [[{:one, [], []}], {:__block__, [], [{:one, [], []}]}]}]}}
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
       one ++ two
       ''', {:++, [], [{:one, [], []}, {:two, [], []}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end

  test "parses lists" do
    codes = [
      {~s'''
       []
       ''', []},
      {~s'''
        [one, :two, "three"]
       ''', [{:one, [], []}, :two, "three"]},
      {~s'''
       [] ++ [bar()]
       ''', {:++, [], [[], [{:bar, [], []}]]}}
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
        %{"foo" => "bar", 1 => 2, :three => :four, [] => [1], %{} => nil, bing => bong, foo: :bar}
       ''',
       {:%{}, [],
        [
          {"foo", "bar"},
          {1, 2},
          {:three, :four},
          {[], [1]},
          {{:%{}, [], []}, nil},
          {{:bing, [], []}, {:bong, [], []}},
          {:foo, :bar}
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
       ''', {:foo, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       foo arg, arg2
       ''', {:foo, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       Remote.foo
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], []}},
      {~s'''
       Remote.foo()
       ''', {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], []}},
      {~s'''
       Remote.foo(arg, arg2)
       ''',
       {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       Remote.foo arg, arg2
       ''',
       {{:., [], [{:__aliases__, [], [:Remote]}, :foo]}, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       :erlang.foo
       ''', {{:., [], [:erlang, :foo]}, [], []}},
      {~s'''
       :erlang.foo()
       ''', {{:., [], [:erlang, :foo]}, [], []}},
      {~s'''
       :erlang.foo(arg, arg2)
       ''', {{:., [], [:erlang, :foo]}, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       :erlang.foo arg, arg2
       ''', {{:., [], [:erlang, :foo]}, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       somevar.foo
       ''', {{:., [], [{:somevar, [], []}, :foo]}, [], []}},
      {~s'''
       somevar.foo()
       ''', {{:., [], [{:somevar, [], []}, :foo]}, [], []}},
      {~s'''
       :elixir_tokenizer.tokenize(String.to_charlist(code), 1, [])
       ''',
       {{:., [], [:elixir_tokenizer, :tokenize]}, [],
        [{{:., [], [{:__aliases__, [], [:String]}, :to_charlist]}, [], [{:code, [], []}]}, 1, []]}},
      {~s'''
       somevar.foo(arg, arg2)
       ''', {{:., [], [{:somevar, [], []}, :foo]}, [], [{:arg, [], []}, {:arg2, [], []}]}},
      {~s'''
       somevar.foo arg, arg2
       ''', {{:., [], [{:somevar, [], []}, :foo]}, [], [{:arg, [], []}, {:arg2, [], []}]}}
    ]

    for {code, expected} <- codes do
      assert Spitfire.parse(code) == expected
    end
  end
end
