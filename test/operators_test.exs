defmodule Spitfire.OperatorsTest do
  @moduledoc """
  Tests for operator precedence and associativity.

  Based on the Elixir operator precedence table (highest to lowest):

  Operator                                       | Associativity
  ---------------------------------------------- | -------------
  `@`                                            | Unary
  `.`                                            | Left
  `+` `-` `!` `^` `not`                          | Unary
  `**`                                           | Left
  `*` `/`                                        | Left
  `+` `-`                                        | Left
  `++` `--` `+++` `---` `..` `<>`                | Right
  `in` `not in`                                  | Left
  `|>` `<<<` `>>>` `<<~` `~>>` `<~` `~>` `<~>`   | Left
  `<` `>` `<=` `>=`                              | Left
  `==` `!=` `=~` `===` `!==`                     | Left
  `&&` `&&&` `and`                               | Left
  `||` `|||` `or`                                | Left
  `=`                                            | Right
  `&`                                            | Unary
  `=>` (valid only inside `%{}`)                 | Right
  `|`                                            | Right
  `::`                                           | Right
  `when`                                         | Right
  `<-` `\\`                                      | Left
  """
  use ExUnit.Case, async: true

  import Spitfire.Assertions, only: [assert_conforms: 1]

  describe "unary @ operator" do
    test "module attribute" do
      assert_conforms("@foo")
    end

    test "module attribute with value" do
      assert_conforms("@foo 1")
    end

    test "nested module attributes" do
      assert_conforms("@foo @bar")
    end

    test "@ has highest precedence" do
      assert_conforms("@foo + 1")
    end

    test "@ with dot access" do
      assert_conforms("@foo.bar")
    end
  end

  describe "unary + and - operators" do
    test "unary plus" do
      assert_conforms("+1")
    end

    test "unary minus" do
      assert_conforms("-1")
    end

    test "unary plus with identifier" do
      assert_conforms("+foo")
    end

    test "unary minus with identifier" do
      assert_conforms("-foo")
    end

    test "double unary minus" do
      assert_conforms("- -1")
    end

    test "unary minus with binary minus" do
      assert_conforms("1 - -2")
    end

    test "unary plus with binary plus" do
      assert_conforms("1 + +2")
    end
  end

  describe "unary ! operator" do
    test "boolean not" do
      assert_conforms("!true")
    end

    test "double negation" do
      assert_conforms("!!true")
    end

    test "! with expression" do
      assert_conforms("!foo")
    end

    test "! has higher precedence than binary operators" do
      assert_conforms("!a && b")
    end

    test "! has higher precedence than ==" do
      assert_conforms("!a == b")
    end
  end

  describe "unary ^ operator (pin)" do
    test "pin operator" do
      assert_conforms("^foo")
    end

    test "pin in pattern match" do
      assert_conforms("^foo = bar")
    end

    test "pin with access" do
      assert_conforms("^foo[0]")
    end

    test "pin with parens" do
      assert_conforms("^foo(0)")
    end
  end

  describe "unary not operator" do
    test "not operator" do
      assert_conforms("not true")
    end

    test "not with expression" do
      assert_conforms("not foo")
    end

    test "not has higher precedence than and" do
      assert_conforms("not a and b")
    end

    test "not has higher precedence than or" do
      assert_conforms("not a or b")
    end
  end

  describe "dot operator - left associativity" do
    test "simple dot access" do
      assert_conforms("foo.bar")
    end

    test "chained dot access - left associative" do
      assert_conforms("foo.bar.baz")
    end

    test "multiple chained dots" do
      assert_conforms("a.b.c.d.e")
    end

    test "dot with function call" do
      assert_conforms("foo.bar()")
    end

    test "chained function calls" do
      assert_conforms("foo.bar().baz()")
    end

    test "dot has higher precedence than +" do
      assert_conforms("foo.bar + 1")
    end

    test "dot has higher precedence than *" do
      assert_conforms("foo.bar * 2")
    end

    test "dot on alias" do
      assert_conforms("Foo.Bar.baz")
    end
  end

  describe "** operator - left associativity" do
    test "simple power" do
      assert_conforms("2 ** 3")
    end

    test "chained power - left associative" do
      # 2 ** 3 ** 4 should be (2 ** 3) ** 4
      assert_conforms("2 ** 3 ** 4")
    end

    test "power has higher precedence than *" do
      assert_conforms("2 * 3 ** 4")
    end

    test "power has higher precedence than +" do
      assert_conforms("1 + 2 ** 3")
    end

    test "power with unary minus" do
      assert_conforms("-2 ** 3")
    end

    test "power with parentheses" do
      assert_conforms("2 ** (3 ** 4)")
    end
  end

  describe "* and / operators - left associativity" do
    test "simple multiplication" do
      assert_conforms("2 * 3")
    end

    test "simple division" do
      assert_conforms("6 / 2")
    end

    test "chained multiplication - left associative" do
      assert_conforms("2 * 3 * 4")
    end

    test "chained division - left associative" do
      assert_conforms("24 / 4 / 2")
    end

    test "mixed * and / - left associative" do
      assert_conforms("2 * 3 / 4 * 5")
    end

    test "* and / have higher precedence than +" do
      assert_conforms("1 + 2 * 3")
    end

    test "* and / have higher precedence than -" do
      assert_conforms("10 - 6 / 2")
    end

    test "* has lower precedence than **" do
      assert_conforms("2 * 3 ** 2")
    end
  end

  describe "+ and - operators - left associativity" do
    test "simple addition" do
      assert_conforms("1 + 2")
    end

    test "simple subtraction" do
      assert_conforms("5 - 3")
    end

    test "chained addition - left associative" do
      assert_conforms("1 + 2 + 3")
    end

    test "chained subtraction - left associative" do
      assert_conforms("10 - 5 - 2")
    end

    test "mixed + and - - left associative" do
      assert_conforms("1 + 2 - 3 + 4")
    end

    test "+ has lower precedence than *" do
      assert_conforms("1 + 2 * 3")
    end

    test "- has lower precedence than /" do
      assert_conforms("10 - 6 / 2")
    end

    test "+ has higher precedence than ++" do
      assert_conforms("a + b ++ c")
    end
  end

  describe "++ operator - right associativity" do
    test "simple concatenation" do
      assert_conforms("[1] ++ [2]")
    end

    test "chained ++ - right associative" do
      # a ++ b ++ c should be a ++ (b ++ c)
      assert_conforms("a ++ b ++ c")
    end

    test "multiple chained ++" do
      assert_conforms("a ++ b ++ c ++ d")
    end

    test "++ has lower precedence than +" do
      assert_conforms("a + b ++ c + d")
    end
  end

  describe "-- operator - right associativity" do
    test "simple subtraction" do
      assert_conforms("[1, 2] -- [1]")
    end

    test "chained -- - right associative" do
      assert_conforms("a -- b -- c")
    end
  end

  describe "+++ operator - right associativity" do
    test "simple +++" do
      assert_conforms("a +++ b")
    end

    test "chained +++ - right associative" do
      assert_conforms("a +++ b +++ c")
    end
  end

  describe "--- operator - right associativity" do
    test "simple ---" do
      assert_conforms("a --- b")
    end

    test "chained --- - right associative" do
      assert_conforms("a --- b --- c")
    end
  end

  describe ".. operator - right associativity" do
    test "simple range" do
      assert_conforms("1..10")
    end

    test "range with step" do
      assert_conforms("1..10//2")
    end

    test "chained .. - right associative" do
      assert_conforms("a..b..c")
    end

    test ".. has lower precedence than +" do
      assert_conforms("1 + 2..3 + 4")
    end
  end

  describe "<> operator - right associativity" do
    test "simple binary concatenation" do
      assert_conforms(~S'"a" <> "b"')
    end

    test "chained <> - right associative" do
      assert_conforms(~S'a <> b <> c')
    end

    test "multiple chained <>" do
      assert_conforms(~S'a <> b <> c <> d')
    end
  end

  describe "mixed right associative list operators" do
    test "++ and -- share right associativity" do
      assert_conforms("a ++ b -- c")
    end

    test "range operator shares precedence with ++" do
      assert_conforms("a ++ b .. c")
    end

    test "<> and ++ share precedence and right associativity" do
      assert_conforms("a <> b ++ c")
    end
  end

  describe "in operator - left associativity" do
    test "simple in" do
      assert_conforms("a in [1, 2, 3]")
    end

    test "in with range" do
      assert_conforms("a in 1..10")
    end

    test "in has lower precedence than ++" do
      assert_conforms("a in b ++ c")
    end

    test "in has higher precedence than |>" do
      assert_conforms("a in b |> c")
    end
  end

  describe "not in operator - left associativity" do
    test "simple not in" do
      assert_conforms("a not in [1, 2, 3]")
    end

    test "not in with range" do
      assert_conforms("a not in 1..10")
    end
  end

  describe "|> operator - left associativity" do
    test "simple pipe" do
      assert_conforms("a |> b")
    end

    test "chained pipe - left associative" do
      assert_conforms("a |> b |> c")
    end

    test "multiple chained pipes" do
      assert_conforms("a |> b |> c |> d |> e")
    end

    test "|> has lower precedence than in" do
      assert_conforms("a in b |> c")
    end

    test "|> has higher precedence than <" do
      assert_conforms("a |> b < c |> d")
    end
  end

  describe "<<< operator - left associativity" do
    test "simple <<<" do
      assert_conforms("a <<< b")
    end

    test "chained <<< - left associative" do
      assert_conforms("a <<< b <<< c")
    end
  end

  describe ">>> operator - left associativity" do
    test "simple >>>" do
      assert_conforms("a >>> b")
    end

    test "chained >>> - left associative" do
      assert_conforms("a >>> b >>> c")
    end
  end

  describe "<<~ operator - left associativity" do
    test "simple <<~" do
      assert_conforms("a <<~ b")
    end

    test "chained <<~ - left associative" do
      assert_conforms("a <<~ b <<~ c")
    end
  end

  describe "~>> operator - left associativity" do
    test "simple ~>>" do
      assert_conforms("a ~>> b")
    end

    test "chained ~>> - left associative" do
      assert_conforms("a ~>> b ~>> c")
    end
  end

  describe "<~ operator - left associativity" do
    test "simple <~" do
      assert_conforms("a <~ b")
    end

    test "chained <~ - left associative" do
      assert_conforms("a <~ b <~ c")
    end
  end

  describe "~> operator - left associativity" do
    test "simple ~>" do
      assert_conforms("a ~> b")
    end

    test "chained ~> - left associative" do
      assert_conforms("a ~> b ~> c")
    end
  end

  describe "<~> operator - left associativity" do
    test "simple <~>" do
      assert_conforms("a <~> b")
    end

    test "chained <~> - left associative" do
      assert_conforms("a <~> b <~> c")
    end
  end

  describe "mixed pipeline family operators" do
    test "operators in the pipeline family stay left associative" do
      assert_conforms("a <<< b |> c ~>> d")
    end
  end

  describe "< operator - left associativity" do
    test "simple less than" do
      assert_conforms("a < b")
    end

    test "chained < - left associative" do
      assert_conforms("a < b < c")
    end

    test "< has lower precedence than |>" do
      assert_conforms("a |> b < c |> d")
    end

    test "< has higher precedence than ==" do
      assert_conforms("a < b == c < d")
    end
  end

  describe "> operator - left associativity" do
    test "simple greater than" do
      assert_conforms("a > b")
    end

    test "chained > - left associative" do
      assert_conforms("a > b > c")
    end
  end

  describe "<= operator - left associativity" do
    test "simple less than or equal" do
      assert_conforms("a <= b")
    end

    test "chained <= - left associative" do
      assert_conforms("a <= b <= c")
    end
  end

  describe ">= operator - left associativity" do
    test "simple greater than or equal" do
      assert_conforms("a >= b")
    end

    test "chained >= - left associative" do
      assert_conforms("a >= b >= c")
    end
  end

  describe "mixed comparison operators" do
    test "< and > mixed - left associative" do
      assert_conforms("a < b > c")
    end

    test "<= and >= mixed - left associative" do
      assert_conforms("a <= b >= c")
    end

    test "all comparison operators mixed" do
      assert_conforms("a < b <= c > d >= e")
    end
  end

  describe "== operator - left associativity" do
    test "simple equality" do
      assert_conforms("a == b")
    end

    test "chained == - left associative" do
      assert_conforms("a == b == c")
    end

    test "== has lower precedence than <" do
      assert_conforms("a < b == c < d")
    end

    test "== has higher precedence than &&" do
      assert_conforms("a == b && c == d")
    end
  end

  describe "!= operator - left associativity" do
    test "simple inequality" do
      assert_conforms("a != b")
    end

    test "chained != - left associative" do
      assert_conforms("a != b != c")
    end
  end

  describe "=~ operator - left associativity" do
    test "simple match" do
      assert_conforms(~S'"hello" =~ ~r/ell/')
    end

    test "chained =~ - left associative" do
      assert_conforms("a =~ b =~ c")
    end
  end

  describe "=== operator - left associativity" do
    test "simple strict equality" do
      assert_conforms("a === b")
    end

    test "chained === - left associative" do
      assert_conforms("a === b === c")
    end
  end

  describe "!== operator - left associativity" do
    test "simple strict inequality" do
      assert_conforms("a !== b")
    end

    test "chained !== - left associative" do
      assert_conforms("a !== b !== c")
    end
  end

  describe "mixed equality operators" do
    test "== and != mixed - left associative" do
      assert_conforms("a == b != c")
    end

    test "=== and !== mixed - left associative" do
      assert_conforms("a === b !== c")
    end

    test "all equality operators mixed" do
      assert_conforms("a == b != c === d !== e =~ f")
    end
  end

  describe "&& operator - left associativity" do
    test "simple and" do
      assert_conforms("a && b")
    end

    test "chained && - left associative" do
      assert_conforms("a && b && c")
    end

    test "&& has lower precedence than ==" do
      assert_conforms("a == b && c == d")
    end

    test "&& has higher precedence than ||" do
      assert_conforms("a && b || c && d")
    end
  end

  describe "&&& operator - left associativity" do
    test "simple &&&" do
      assert_conforms("a &&& b")
    end

    test "chained &&& - left associative" do
      assert_conforms("a &&& b &&& c")
    end
  end

  describe "and operator - left associativity" do
    test "simple and" do
      assert_conforms("a and b")
    end

    test "chained and - left associative" do
      assert_conforms("a and b and c")
    end

    test "and has lower precedence than ==" do
      assert_conforms("a == b and c == d")
    end

    test "and has higher precedence than or" do
      assert_conforms("a and b or c and d")
    end
  end

  describe "mixed AND operators" do
    test "&& and and mixed" do
      assert_conforms("a && b and c")
    end

    test "&&& with && and and" do
      assert_conforms("a &&& b && c and d")
    end
  end

  describe "|| operator - left associativity" do
    test "simple or" do
      assert_conforms("a || b")
    end

    test "chained || - left associative" do
      assert_conforms("a || b || c")
    end

    test "|| has lower precedence than &&" do
      assert_conforms("a && b || c && d")
    end

    test "|| has higher precedence than =" do
      assert_conforms("a = b || c")
    end
  end

  describe "||| operator - left associativity" do
    test "simple |||" do
      assert_conforms("a ||| b")
    end

    test "chained ||| - left associative" do
      assert_conforms("a ||| b ||| c")
    end
  end

  describe "or operator - left associativity" do
    test "simple or" do
      assert_conforms("a or b")
    end

    test "chained or - left associative" do
      assert_conforms("a or b or c")
    end

    test "or has lower precedence than and" do
      assert_conforms("a and b or c and d")
    end
  end

  describe "mixed OR operators" do
    test "|| and or mixed" do
      assert_conforms("a || b or c")
    end

    test "||| with || and or" do
      assert_conforms("a ||| b || c or d")
    end
  end

  describe "= operator - right associativity" do
    test "simple match" do
      assert_conforms("a = b")
    end

    test "chained = - right associative" do
      # a = b = c should be a = (b = c)
      assert_conforms("a = b = c")
    end

    test "multiple chained =" do
      assert_conforms("a = b = c = d")
    end

    test "= has lower precedence than ||" do
      assert_conforms("a = b || c")
    end

    test "= has higher precedence than &" do
      assert_conforms("a = &b/1")
    end

    test "pattern match with tuple" do
      assert_conforms("{a, b} = {1, 2}")
    end

    test "pattern match with list" do
      assert_conforms("[h | t] = [1, 2, 3]")
    end
  end

  describe "& operator - unary" do
    test "capture function" do
      assert_conforms("&foo/1")
    end

    test "capture with module" do
      assert_conforms("&Foo.bar/2")
    end

    test "capture with expression" do
      assert_conforms("&(&1 + 1)")
    end

    test "capture with multiple args" do
      assert_conforms("&(&1 + &2)")
    end

    test "& has lower precedence than =" do
      assert_conforms("f = &foo/1")
    end
  end

  describe "... operator - unary" do
    test "bare ellipsis expression" do
      assert_conforms("...")
    end

    test "ellipsis inside anonymous function body" do
      assert_conforms("fn -> ... end")
    end

    test "= has higher precedence than ..." do
      assert_conforms("... = a = b")
    end

    test "ellipsis keeps inner arithmetic precedence" do
      assert_conforms("... + 1 * 2")
    end

    test "ellipsis can be captured" do
      assert_conforms("&...")
    end

    test "ellipsis as term before infix operator" do
      assert_conforms("... * 1")
    end

    test "ellipsis as term before range" do
      assert_conforms("... .. 1")
    end
  end

  describe "=> operator - right associativity in maps" do
    test "simple map with =>" do
      assert_conforms("%{a => b}")
    end

    test "map with multiple =>" do
      assert_conforms("%{a => b, c => d}")
    end

    test "nested map with =>" do
      assert_conforms("%{a => %{b => c}}")
    end

    test "=> with complex keys" do
      assert_conforms("%{1 + 2 => 3}")
    end

    test "=> with complex values" do
      assert_conforms("%{a => b + c}")
    end
  end

  describe "| operator - right associativity" do
    test "simple cons" do
      assert_conforms("[a | b]")
    end

    test "cons with multiple elements" do
      assert_conforms("[a, b | c]")
    end

    test "map update with |" do
      assert_conforms("%{map | a: 1}")
    end

    test "struct update with |" do
      assert_conforms("%Foo{struct | a: 1}")
    end

    test "| is right associative across repeated operators" do
      assert_conforms("a | b | c")
    end

    test "| has lower precedence than =>" do
      assert_conforms("%{a => b | c}")
    end
  end

  describe ":: operator - right associativity" do
    test "simple type annotation" do
      assert_conforms("a :: integer")
    end

    test "bitstring type" do
      assert_conforms("<<a::8>>")
    end

    test "bitstring with size" do
      assert_conforms("<<a::size(8)>>")
    end

    test "bitstring with multiple specs" do
      assert_conforms("<<a::8, b::binary>>")
    end

    test "chained :: - right associative" do
      assert_conforms("a :: b :: c")
    end

    test ":: has lower precedence than |" do
      assert_conforms("a | b :: c")
    end
  end

  describe "when operator - right associativity" do
    test "simple guard" do
      assert_conforms("def foo(a) when is_integer(a), do: a")
    end

    test "simple guard - bracket" do
      assert_conforms("def foo[a] when is_integer(a), do: a")
    end

    test "simple guard - string" do
      assert_conforms("def \"asd\" when is_integer(a), do: a")

      assert_conforms("def 'asd' when is_integer(a), do: a")

      assert_conforms("def \"\"\"\nasd\n\"\"\" when is_integer(a), do: a")

      assert_conforms("def '''\nasd\n\''' when is_integer(a), do: a")

      assert_conforms("def ~c'asd' when is_integer(a), do: a")

      assert_conforms("def :\"asd\" when is_integer(a), do: a")
    end

    test "multiple guards with and" do
      assert_conforms("def foo(a) when is_integer(a) and a > 0, do: a")
    end

    test "multiple guards with or" do
      assert_conforms("def foo(a) when is_integer(a) or is_float(a), do: a")
    end

    test "chained when - right associative" do
      assert_conforms("a when b when c")
    end

    test "when has lower precedence than ::" do
      assert_conforms("a :: b when c")
    end
  end

  describe "<- operator - left associativity" do
    test "simple generator" do
      assert_conforms("for x <- [1, 2, 3], do: x")
    end

    test "multiple generators" do
      assert_conforms("for x <- xs, y <- ys, do: {x, y}")
    end

    test "with expression" do
      assert_conforms("with {:ok, a} <- foo(), do: a")
    end

    test "<- has lower precedence than when" do
      assert_conforms("for x when is_integer(x) <- xs, do: x")
    end

    test "<- groups left to right when chained" do
      assert_conforms("a <- b <- c")
    end
  end

  describe "\\\\ operator - left associativity" do
    test "simple default argument" do
      assert_conforms("def foo(a \\\\ 1), do: a")
    end

    test "multiple default arguments" do
      assert_conforms("def foo(a \\\\ 1, b \\\\ 2), do: {a, b}")
    end

    test "default with complex expression" do
      assert_conforms("def foo(a \\\\ 1 + 2), do: a")
    end

    test "\\\\ groups left to right when chained" do
      assert_conforms("a \\\\ b \\\\ c")
    end
  end

  describe "precedence interactions" do
    test "arithmetic vs logical" do
      assert_conforms("1 + 2 == 3 and 4 - 1 == 3")
    end

    test "comparison vs logical" do
      assert_conforms("a < b && c > d || e <= f")
    end

    test "pipe vs arithmetic" do
      assert_conforms("1 + 2 |> foo() |> bar() + 3")
    end

    test "all arithmetic operators" do
      assert_conforms("1 + 2 * 3 ** 4 / 5 - 6")
    end

    test "unary and binary operators" do
      assert_conforms("-1 + -2 * -3")
    end

    test "complex expression with many operators" do
      assert_conforms("@foo.bar |> baz() == 1 + 2 * 3 and !flag || default")
    end

    test "range with arithmetic" do
      assert_conforms("1 + 2..3 * 4")
    end

    test "list operators with comparison" do
      assert_conforms("[1] ++ [2] == [1, 2]")
    end

    test "string concat with comparison" do
      assert_conforms(~S'"a" <> "b" == "ab"')
    end

    test "match with pipe" do
      assert_conforms("result = a |> b |> c")
    end

    test "capture with match" do
      assert_conforms("fun = &Foo.bar/2")
    end

    test "deep nesting of operators" do
      assert_conforms("a + b * c ** d / e - f")
    end

    test "boolean expression chain" do
      assert_conforms("a and b or c and d or e")
    end

    test "relaxed boolean chain" do
      assert_conforms("a && b || c && d || e")
    end

    test "mixed boolean operators" do
      assert_conforms("a and b && c or d || e")
    end
  end

  describe "left associativity verification" do
    test "left associative operators group left to right" do
      # For left associative: a op b op c == (a op b) op c
      # The AST should show the leftmost operation as the innermost

      # Multiplication
      assert_conforms("a * b * c")

      # Addition
      assert_conforms("a + b + c")

      # Comparison
      assert_conforms("a < b < c")

      # Logical and
      assert_conforms("a && b && c")

      # Pipe
      assert_conforms("a |> b |> c")
    end
  end

  describe "right associativity verification" do
    test "right associative operators group right to left" do
      # For right associative: a op b op c == a op (b op c)
      # The AST should show the rightmost operation as the innermost

      # List concat
      assert_conforms("a ++ b ++ c")

      # Match
      assert_conforms("a = b = c")

      # Type
      assert_conforms("a :: b :: c")

      # When
      assert_conforms("a when b when c")

      # Range
      assert_conforms("a..b..c")
    end
  end

  describe "nullary .. operator" do
    test "bare range" do
      assert_conforms("..")
    end

    test "nullary range in function call" do
      assert_conforms("Enum.to_list(..)")
    end
  end

  describe "..// operator" do
    test "simple range with step" do
      assert_conforms("1..10//2")
    end

    test "range with negative step" do
      assert_conforms("10..1//-1")
    end

    test "range with step and variables" do
      assert_conforms("a..b//c")
    end

    test "range with step has correct precedence with +" do
      assert_conforms("1 + 2..3 + 4//5")
    end

    test "range with step in comprehension" do
      assert_conforms("for i <- 1..10//2, do: i")
    end
  end

  describe "precedence boundaries: @ (highest) vs . (second highest)" do
    test "@ binds tighter than dot on result" do
      assert_conforms("@foo.bar")
    end

    test "@ with chained dots" do
      assert_conforms("@foo.bar.baz")
    end
  end

  describe "precedence boundaries: . vs unary +/-/!/^/not" do
    test "dot binds tighter than unary minus" do
      assert_conforms("-foo.bar")
    end

    test "dot binds tighter than unary plus" do
      assert_conforms("+foo.bar")
    end

    test "dot binds tighter than unary not" do
      assert_conforms("!foo.bar")
    end

    test "dot binds tighter than not keyword" do
      assert_conforms("not foo.bar")
    end

    test "dot binds tighter than pin" do
      assert_conforms("^foo.bar")
    end
  end

  describe "precedence boundaries: unary +/-/!/^/not vs **" do
    test "unary minus binds tighter than **" do
      assert_conforms("-2 ** 3")
    end

    test "unary plus binds tighter than **" do
      assert_conforms("+2 ** 3")
    end

    test "unary ! with ** operand" do
      assert_conforms("!a ** b")
    end
  end

  describe "precedence boundaries: ** vs * /" do
    test "** binds tighter than *" do
      assert_conforms("2 * 3 ** 4")
    end

    test "** binds tighter than /" do
      assert_conforms("8 / 2 ** 2")
    end

    test "* after ** expression" do
      assert_conforms("2 ** 3 * 4")
    end

    test "/ after ** expression" do
      assert_conforms("2 ** 3 / 4")
    end
  end

  describe "precedence boundaries: * / vs + -" do
    test "* binds tighter than +" do
      assert_conforms("1 + 2 * 3")
    end

    test "/ binds tighter than -" do
      assert_conforms("6 - 4 / 2")
    end

    test "* binds tighter than -" do
      assert_conforms("6 - 2 * 2")
    end

    test "/ binds tighter than +" do
      assert_conforms("1 + 6 / 2")
    end
  end

  describe "precedence boundaries: + - vs ++ -- +++ --- .. <>" do
    test "+ binds tighter than ++" do
      assert_conforms("a + b ++ c + d")
    end

    test "- binds tighter than --" do
      assert_conforms("a - b -- c - d")
    end

    test "+ binds tighter than .." do
      assert_conforms("1 + 2..3 + 4")
    end

    test "+ binds tighter than <>" do
      assert_conforms("a + b <> c + d")
    end

    test "- binds tighter than +++" do
      assert_conforms("a - b +++ c - d")
    end

    test "- binds tighter than ---" do
      assert_conforms("a - b --- c - d")
    end
  end

  describe "precedence boundaries: ++ -- +++ --- .. <> vs in/not in" do
    test "++ binds tighter than in" do
      assert_conforms("a in b ++ c")
    end

    test "-- binds tighter than in" do
      assert_conforms("a in b -- c")
    end

    test ".. binds tighter than in" do
      assert_conforms("a in 1..10")
    end

    test "<> binds tighter than in" do
      assert_conforms("a in b <> c")
    end

    test "++ binds tighter than not in" do
      assert_conforms("a not in b ++ c")
    end
  end

  describe "precedence boundaries: in/not in vs |> <<< >>> <<~ ~>> <~ ~> <~>" do
    test "in binds tighter than |>" do
      assert_conforms("a in b |> c")
    end

    test "not in binds tighter than |>" do
      assert_conforms("a not in b |> c")
    end

    test "in binds tighter than <<<" do
      assert_conforms("a in b <<< c")
    end

    test "in binds tighter than >>>" do
      assert_conforms("a in b >>> c")
    end

    test "in binds tighter than ~>" do
      assert_conforms("a in b ~> c")
    end

    test "in binds tighter than <~" do
      assert_conforms("a in b <~ c")
    end

    test "in binds tighter than <~>" do
      assert_conforms("a in b <~> c")
    end

    test "in binds tighter than <<~" do
      assert_conforms("a in b <<~ c")
    end

    test "in binds tighter than ~>>" do
      assert_conforms("a in b ~>> c")
    end
  end

  describe "precedence boundaries: |> <<< >>> <<~ ~>> <~ ~> <~> vs < > <= >=" do
    test "|> binds tighter than <" do
      assert_conforms("a |> b < c |> d")
    end

    test "|> binds tighter than >" do
      assert_conforms("a |> b > c |> d")
    end

    test "|> binds tighter than <=" do
      assert_conforms("a |> b <= c |> d")
    end

    test "|> binds tighter than >=" do
      assert_conforms("a |> b >= c |> d")
    end

    test "<<< binds tighter than <" do
      assert_conforms("a <<< b < c <<< d")
    end

    test ">>> binds tighter than >" do
      assert_conforms("a >>> b > c >>> d")
    end

    test "~> binds tighter than <" do
      assert_conforms("a ~> b < c ~> d")
    end
  end

  describe "precedence boundaries: < > <= >= vs == != =~ === !==" do
    test "< binds tighter than ==" do
      assert_conforms("a < b == c < d")
    end

    test "> binds tighter than !=" do
      assert_conforms("a > b != c > d")
    end

    test "<= binds tighter than ===" do
      assert_conforms("a <= b === c <= d")
    end

    test ">= binds tighter than !==" do
      assert_conforms("a >= b !== c >= d")
    end

    test "< binds tighter than =~" do
      assert_conforms("a < b =~ c")
    end
  end

  describe "precedence boundaries: == != =~ === !== vs && &&& and" do
    test "== binds tighter than &&" do
      assert_conforms("a == b && c == d")
    end

    test "!= binds tighter than &&&" do
      assert_conforms("a != b &&& c != d")
    end

    test "=== binds tighter than and" do
      assert_conforms("a === b and c === d")
    end

    test "!== binds tighter than &&" do
      assert_conforms("a !== b && c !== d")
    end

    test "=~ binds tighter than and" do
      assert_conforms("a =~ b and c =~ d")
    end
  end

  describe "precedence boundaries: && &&& and vs || ||| or" do
    test "&& binds tighter than ||" do
      assert_conforms("a && b || c && d")
    end

    test "&&& binds tighter than |||" do
      assert_conforms("a &&& b ||| c &&& d")
    end

    test "and binds tighter than or" do
      assert_conforms("a and b or c and d")
    end

    test "&& binds tighter than or" do
      assert_conforms("a && b or c && d")
    end

    test "and binds tighter than ||" do
      assert_conforms("a and b || c and d")
    end
  end

  describe "precedence boundaries: || ||| or vs =" do
    test "|| binds tighter than =" do
      assert_conforms("a = b || c")
    end

    test "||| binds tighter than =" do
      assert_conforms("a = b ||| c")
    end

    test "or binds tighter than =" do
      assert_conforms("a = b or c")
    end

    test "|| on both sides of =" do
      assert_conforms("a || b = c || d")
    end
  end

  describe "precedence boundaries: = vs & ..." do
    test "= binds tighter than &" do
      assert_conforms("f = &foo/1")
    end

    test "& captures result of =" do
      assert_conforms("&(a = b)")
    end
  end

  describe "precedence boundaries: & vs =>" do
    test "& and => in map" do
      assert_conforms("%{&foo/1 => 1}")
    end

    test "=> with capture on both sides" do
      assert_conforms("%{&foo/1 => &bar/2}")
    end
  end

  describe "precedence boundaries: => vs |" do
    test "=> binds tighter than | in map" do
      assert_conforms("%{a => b | c}")
    end

    test "| in map update with =>" do
      assert_conforms("%{map | a => b}")
    end
  end

  describe "precedence boundaries: | vs ::" do
    test "| binds tighter than ::" do
      assert_conforms("a | b :: c")
    end

    test ":: after | in type spec context" do
      assert_conforms("@spec foo(a | b :: c)")
    end

    test ":: after | in type spec context - bracket" do
      assert_conforms("@spec foo[a | b :: c]")
    end

    test ":: after | in type spec context - string" do
      assert_conforms("@spec \"sdc\", (a | b :: c)")

      assert_conforms("@spec 'sdc', (a | b :: c)")

      assert_conforms("@spec :\"sdc\", (a | b :: c)")

      assert_conforms("@spec \"\"\"\nsdc\n\"\"\", (a | b :: c)")

      assert_conforms("@spec '''\nsdc\n''', (a | b :: c)")

      assert_conforms("@spec ~c'sdc', (a | b :: c)")
    end
  end

  describe "precedence boundaries: :: vs when" do
    test ":: binds tighter than when" do
      assert_conforms("a :: b when c")
    end

    test "when with typed parameter" do
      assert_conforms("@spec foo(a :: integer) when a: term")
    end
  end

  describe "precedence boundaries: when vs <- \\\\" do
    test "when binds tighter than <-" do
      assert_conforms("for x when is_integer(x) <- xs, do: x")
    end

    test "when binds tighter than \\\\" do
      assert_conforms("x when true \\\\ default")
    end

    test "<- with when guard" do
      assert_conforms("for x when x > 0 <- list, do: x")
    end
  end

  describe "same precedence level: ++ -- +++ --- .. <>" do
    test "++ and -- at same precedence" do
      assert_conforms("a ++ b -- c")
    end

    test "++ and .. at same precedence" do
      assert_conforms("a ++ 1..10")
    end

    test "-- and <> at same precedence" do
      assert_conforms("a -- b <> c")
    end

    test "+++ and --- at same precedence" do
      assert_conforms("a +++ b --- c")
    end

    test "all list ops mixed" do
      assert_conforms("a ++ b -- c +++ d --- e")
    end

    test "<> and ++ and .." do
      assert_conforms("a <> b ++ 1..10")
    end
  end

  describe "same precedence level: |> <<< >>> <<~ ~>> <~ ~> <~>" do
    test "|> and <<< at same precedence" do
      assert_conforms("a |> b <<< c")
    end

    test "|> and >>> at same precedence" do
      assert_conforms("a |> b >>> c")
    end

    test "<<< and >>> at same precedence" do
      assert_conforms("a <<< b >>> c")
    end

    test "<<~ and ~>> at same precedence" do
      assert_conforms("a <<~ b ~>> c")
    end

    test "<~ and ~> at same precedence" do
      assert_conforms("a <~ b ~> c")
    end

    test "<~> with other arrow ops" do
      assert_conforms("a <~> b |> c ~> d")
    end

    test "all arrow ops mixed" do
      assert_conforms("a |> b <<< c >>> d <<~ e ~>> f <~ g ~> h <~> i")
    end
  end

  describe "same precedence level: < > <= >=" do
    test "< and > at same precedence" do
      assert_conforms("a < b > c")
    end

    test "<= and >= at same precedence" do
      assert_conforms("a <= b >= c")
    end

    test "< and >= at same precedence" do
      assert_conforms("a < b >= c")
    end

    test "> and <= at same precedence" do
      assert_conforms("a > b <= c")
    end

    test "all comparison ops" do
      assert_conforms("a < b > c <= d >= e")
    end
  end

  describe "same precedence level: == != =~ === !==" do
    test "== and != at same precedence" do
      assert_conforms("a == b != c")
    end

    test "=== and !== at same precedence" do
      assert_conforms("a === b !== c")
    end

    test "== and =~ at same precedence" do
      assert_conforms("a == b =~ c")
    end

    test "all equality ops" do
      assert_conforms("a == b != c =~ d === e !== f")
    end
  end

  describe "same precedence level: && &&& and" do
    test "&& and &&& at same precedence" do
      assert_conforms("a && b &&& c")
    end

    test "&& and and at same precedence" do
      assert_conforms("a && b and c")
    end

    test "&&& and and at same precedence" do
      assert_conforms("a &&& b and c")
    end

    test "all AND ops" do
      assert_conforms("a && b &&& c and d")
    end
  end

  describe "same precedence level: || ||| or" do
    test "|| and ||| at same precedence" do
      assert_conforms("a || b ||| c")
    end

    test "|| and or at same precedence" do
      assert_conforms("a || b or c")
    end

    test "||| and or at same precedence" do
      assert_conforms("a ||| b or c")
    end

    test "all OR ops" do
      assert_conforms("a || b ||| c or d")
    end
  end

  describe "same precedence level: <- \\\\" do
    test "<- and \\\\ at same precedence" do
      assert_conforms("a <- b \\\\ c")
    end

    test "\\\\ and <- at same precedence" do
      assert_conforms("a \\\\ b <- c")
    end
  end

  describe "edge cases" do
    test "parentheses override precedence" do
      assert_conforms("(1 + 2) * 3")
    end

    test "nested parentheses" do
      assert_conforms("((1 + 2) * (3 + 4))")
    end

    test "operator with newline" do
      assert_conforms("1 +\n2")
    end

    test "chained operators with newlines" do
      assert_conforms("1 +\n2 *\n3")
    end

    test "pipe with newlines" do
      assert_conforms("a\n|> b\n|> c")
    end

    test "unary operators with parens" do
      assert_conforms("-(1 + 2)")
    end

    test "not with parens" do
      assert_conforms("not (a and b)")
    end

    test "! with parens" do
      assert_conforms("!(a && b)")
    end

    test "access syntax with operators" do
      assert_conforms("foo[a + b]")
    end

    test "function call with operator expression" do
      assert_conforms("foo(a + b, c * d)")
    end

    test "deeply nested arithmetic" do
      assert_conforms("1 + 2 * 3 ** 4 / 5 - 6 + 7 * 8")
    end

    test "mixed unary operators" do
      assert_conforms("- - - -a")
    end

    test "unary not chain" do
      assert_conforms("not not not a")
    end

    test "unary ! chain" do
      assert_conforms("!!!a")
    end

    test "complex pipe chain" do
      assert_conforms("a |> b() |> c(1, 2) |> d.e() |> f")
    end

    test "binary operator after pipe" do
      assert_conforms("a |> b + c")
    end

    test "comparison chain" do
      assert_conforms("1 < 2 <= 3 > 0 >= -1")
    end

    test "match in if condition" do
      assert_conforms("if a = b, do: a")
    end

    test "operator in anonymous function" do
      assert_conforms("fn a, b -> a + b end")
    end

    test "operator in case clause" do
      assert_conforms("case a do\n  x when x > 0 -> x * 2\n  _ -> 0\nend")
    end

    test "multiple matches on same line" do
      assert_conforms("a = b = c = 1")
    end

    test "range in case pattern" do
      assert_conforms("case x do\n  n when n in 1..10 -> :small\n  _ -> :large\nend")
    end
  end

  describe "full precedence chain tests" do
    test "expression using operators from many precedence levels" do
      # @ > . > unary > ** > * > + > ++ > in > |> > < > == > && > || > =
      assert_conforms("result = a || b && c == d < e |> f in g ++ h + i * j ** k")
    end

    test "complex boolean expression with all logical operators" do
      assert_conforms("a and b && c or d || e and f && g or h")
    end

    test "arithmetic with all arithmetic operators" do
      assert_conforms("a ** b * c / d + e - f")
    end

    test "list operations chain" do
      assert_conforms("a ++ b -- c ++ d -- e")
    end

    test "all comparison operators in sequence" do
      assert_conforms("a < b > c <= d >= e == f != g === h !== i =~ j")
    end

    test "@ with full operator chain" do
      assert_conforms("@foo + 1 * 2 == 3 and true")
    end

    test "dot call with full operator chain" do
      assert_conforms("a.b + c.d * e.f == g.h")
    end

    test "capture with complex expression" do
      assert_conforms("&(&1 + &2 * &3)")
    end

    test "match with complex right side" do
      assert_conforms("{a, b} = c ++ d |> e")
    end

    test "guard clause with multiple operators" do
      assert_conforms("def foo(a, b) when is_integer(a) and a > 0 and b < 100 or is_float(a), do: a + b")
    end

    test "type spec with union and constraints" do
      assert_conforms("@spec foo(a :: integer | float, b :: atom) :: boolean when a: number")
    end

    test "comprehension with complex generators and filters" do
      assert_conforms("for x <- xs, x > 0, y <- ys, x + y < 10, do: {x, y}")
    end

    test "with expression with multiple clauses" do
      assert_conforms("with {:ok, a} <- foo(), {:ok, b} <- bar(a), c = a + b, do: c")
    end

    test "bitstring with multiple type specs" do
      assert_conforms("<<a::8, b::16-big-unsigned, c::binary-size(4), rest::binary>>")
    end

    test "map with arrow and expression values" do
      assert_conforms("%{a + b => c * d, e => f || g}")
    end

    test "struct with update and expressions" do
      assert_conforms("%Foo{bar | a: b + c, d: e * f}")
    end

    test "nested data structures with operators" do
      assert_conforms("[a: b + c, d: [e * f, g | h]]")
    end
  end

  describe "regression and tricky cases" do
    test "minus after dot without space" do
      assert_conforms("a.b-c")
    end

    test "plus after dot without space" do
      assert_conforms("a.b+c")
    end

    test "unary minus in function call" do
      assert_conforms("foo(-1)")
    end

    test "unary minus in list" do
      assert_conforms("[-1, -2, -3]")
    end

    test "unary plus in tuple" do
      assert_conforms("{+1, +2}")
    end

    test "not in with complex expression" do
      assert_conforms("a + b not in c ++ d")
    end

    test "double pipe confusion" do
      # || is boolean or, | is cons/union
      assert_conforms("a || [b | c]")
    end

    test "for generator rhs with || [] inside assoc-map list value" do
      assert_conforms(~S(%{"s" => [for x <- y || [] do x end]}))
    end

    test "triple less than" do
      # <<< is custom operator, << is bitstring
      assert_conforms("a <<< b")
    end

    test "power with negative exponent" do
      assert_conforms("2 ** -3")
    end

    test "range with negative bounds" do
      assert_conforms("-10..-1")
    end

    test "range with negative step" do
      assert_conforms("10..1//-1")
    end

    test "capture with arithmetic" do
      assert_conforms("&(&1 + 1)")
    end

    test "pin in match with binary operator" do
      assert_conforms("^a = b + c")
    end

    test "when in anonymous function" do
      assert_conforms("fn x when x > 0 -> x end")
    end

    test "multiple when clauses" do
      assert_conforms("fn x when is_integer(x) when x > 0 -> x end")
    end

    test "default argument with complex expression" do
      assert_conforms("def foo(a \\\\ 1 + 2 * 3), do: a")
    end

    test "access in operator expression" do
      assert_conforms("a[b] + c[d]")
    end

    test "parens in operator expression" do
      assert_conforms("a(b) + c(d)")
    end

    test "dot call on result of operator" do
      assert_conforms("(a + b).c")
    end

    test "operators in sigil" do
      assert_conforms("~w(a + b)")
    end

    test "string interpolation with operators" do
      assert_conforms(~S'"result: #{a + b * c}"')
    end

    test "heredoc with operators" do
      assert_conforms(~s'"""\n\#{a + b}\n"""')
    end

    test "keyword list with operator values" do
      assert_conforms("[a: b + c, d: e * f]")
    end

    test "keyword argument in function call" do
      assert_conforms("foo(a, b: c + d)")
    end

    test "pipe into keyword function" do
      assert_conforms("a |> foo(b: c)")
    end

    test "capture with module function" do
      assert_conforms("&Mod.fun/2")
    end

    test "capture placeholder in operator" do
      assert_conforms("&(&1 <> &2)")
    end
  end
end
