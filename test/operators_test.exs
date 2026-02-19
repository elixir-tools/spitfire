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

  describe "unary @ operator" do
    test "module attribute" do
      code = "@foo"
      assert spitfire_parse(code) == s2q(code)
    end

    test "module attribute with value" do
      code = "@foo 1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "nested module attributes" do
      code = "@foo @bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "@ has highest precedence" do
      code = "@foo + 1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "@ with dot access" do
      code = "@foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "unary + and - operators" do
    test "unary plus" do
      code = "+1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary minus" do
      code = "-1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary plus with identifier" do
      code = "+foo"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary minus with identifier" do
      code = "-foo"
      assert spitfire_parse(code) == s2q(code)
    end

    test "double unary minus" do
      code = "- -1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary minus with binary minus" do
      code = "1 - -2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary plus with binary plus" do
      code = "1 + +2"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "unary ! operator" do
    test "boolean not" do
      code = "!true"
      assert spitfire_parse(code) == s2q(code)
    end

    test "double negation" do
      code = "!!true"
      assert spitfire_parse(code) == s2q(code)
    end

    test "! with expression" do
      code = "!foo"
      assert spitfire_parse(code) == s2q(code)
    end

    test "! has higher precedence than binary operators" do
      code = "!a && b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "! has higher precedence than ==" do
      code = "!a == b"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "unary ^ operator (pin)" do
    test "pin operator" do
      code = "^foo"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pin in pattern match" do
      code = "^foo = bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pin with access" do
      code = "^foo[0]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pin with parens" do
      code = "^foo(0)"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "unary not operator" do
    test "not operator" do
      code = "not true"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not with expression" do
      code = "not foo"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not has higher precedence than and" do
      code = "not a and b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not has higher precedence than or" do
      code = "not a or b"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "dot operator - left associativity" do
    test "simple dot access" do
      code = "foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained dot access - left associative" do
      code = "foo.bar.baz"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple chained dots" do
      code = "a.b.c.d.e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot with function call" do
      code = "foo.bar()"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained function calls" do
      code = "foo.bar().baz()"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot has higher precedence than +" do
      code = "foo.bar + 1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot has higher precedence than *" do
      code = "foo.bar * 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot on alias" do
      code = "Foo.Bar.baz"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "** operator - left associativity" do
    test "simple power" do
      code = "2 ** 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained power - left associative" do
      # 2 ** 3 ** 4 should be (2 ** 3) ** 4
      code = "2 ** 3 ** 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "power has higher precedence than *" do
      code = "2 * 3 ** 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "power has higher precedence than +" do
      code = "1 + 2 ** 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "power with unary minus" do
      code = "-2 ** 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "power with parentheses" do
      code = "2 ** (3 ** 4)"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "* and / operators - left associativity" do
    test "simple multiplication" do
      code = "2 * 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "simple division" do
      code = "6 / 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained multiplication - left associative" do
      code = "2 * 3 * 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained division - left associative" do
      code = "24 / 4 / 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "mixed * and / - left associative" do
      code = "2 * 3 / 4 * 5"
      assert spitfire_parse(code) == s2q(code)
    end

    test "* and / have higher precedence than +" do
      code = "1 + 2 * 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "* and / have higher precedence than -" do
      code = "10 - 6 / 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "* has lower precedence than **" do
      code = "2 * 3 ** 2"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "+ and - operators - left associativity" do
    test "simple addition" do
      code = "1 + 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "simple subtraction" do
      code = "5 - 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained addition - left associative" do
      code = "1 + 2 + 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained subtraction - left associative" do
      code = "10 - 5 - 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "mixed + and - - left associative" do
      code = "1 + 2 - 3 + 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "+ has lower precedence than *" do
      code = "1 + 2 * 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "- has lower precedence than /" do
      code = "10 - 6 / 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "+ has higher precedence than ++" do
      code = "a + b ++ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "++ operator - right associativity" do
    test "simple concatenation" do
      code = "[1] ++ [2]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained ++ - right associative" do
      # a ++ b ++ c should be a ++ (b ++ c)
      code = "a ++ b ++ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple chained ++" do
      code = "a ++ b ++ c ++ d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "++ has lower precedence than +" do
      code = "a + b ++ c + d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "-- operator - right associativity" do
    test "simple subtraction" do
      code = "[1, 2] -- [1]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained -- - right associative" do
      code = "a -- b -- c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "+++ operator - right associativity" do
    test "simple +++" do
      code = "a +++ b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained +++ - right associative" do
      code = "a +++ b +++ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "--- operator - right associativity" do
    test "simple ---" do
      code = "a --- b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained --- - right associative" do
      code = "a --- b --- c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe ".. operator - right associativity" do
    test "simple range" do
      code = "1..10"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with step" do
      code = "1..10//2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained .. - right associative" do
      code = "a..b..c"
      assert spitfire_parse(code) == s2q(code)
    end

    test ".. has lower precedence than +" do
      code = "1 + 2..3 + 4"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<> operator - right associativity" do
    test "simple binary concatenation" do
      code = ~S'"a" <> "b"'
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained <> - right associative" do
      code = ~S'a <> b <> c'
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple chained <>" do
      code = ~S'a <> b <> c <> d'
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "mixed right associative list operators" do
    test "++ and -- share right associativity" do
      code = "a ++ b -- c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range operator shares precedence with ++" do
      code = "a ++ b .. c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<> and ++ share precedence and right associativity" do
      code = "a <> b ++ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "in operator - left associativity" do
    test "simple in" do
      code = "a in [1, 2, 3]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in with range" do
      code = "a in 1..10"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in has lower precedence than ++" do
      code = "a in b ++ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in has higher precedence than |>" do
      code = "a in b |> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "not in operator - left associativity" do
    test "simple not in" do
      code = "a not in [1, 2, 3]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not in with range" do
      code = "a not in 1..10"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "|> operator - left associativity" do
    test "simple pipe" do
      code = "a |> b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained pipe - left associative" do
      code = "a |> b |> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple chained pipes" do
      code = "a |> b |> c |> d |> e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|> has lower precedence than in" do
      code = "a in b |> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|> has higher precedence than <" do
      code = "a |> b < c |> d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<<< operator - left associativity" do
    test "simple <<<" do
      code = "a <<< b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained <<< - left associative" do
      code = "a <<< b <<< c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe ">>> operator - left associativity" do
    test "simple >>>" do
      code = "a >>> b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained >>> - left associative" do
      code = "a >>> b >>> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<<~ operator - left associativity" do
    test "simple <<~" do
      code = "a <<~ b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained <<~ - left associative" do
      code = "a <<~ b <<~ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "~>> operator - left associativity" do
    test "simple ~>>" do
      code = "a ~>> b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained ~>> - left associative" do
      code = "a ~>> b ~>> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<~ operator - left associativity" do
    test "simple <~" do
      code = "a <~ b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained <~ - left associative" do
      code = "a <~ b <~ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "~> operator - left associativity" do
    test "simple ~>" do
      code = "a ~> b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained ~> - left associative" do
      code = "a ~> b ~> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<~> operator - left associativity" do
    test "simple <~>" do
      code = "a <~> b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained <~> - left associative" do
      code = "a <~> b <~> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "mixed pipeline family operators" do
    test "operators in the pipeline family stay left associative" do
      code = "a <<< b |> c ~>> d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "< operator - left associativity" do
    test "simple less than" do
      code = "a < b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained < - left associative" do
      code = "a < b < c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "< has lower precedence than |>" do
      code = "a |> b < c |> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "< has higher precedence than ==" do
      code = "a < b == c < d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "> operator - left associativity" do
    test "simple greater than" do
      code = "a > b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained > - left associative" do
      code = "a > b > c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<= operator - left associativity" do
    test "simple less than or equal" do
      code = "a <= b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained <= - left associative" do
      code = "a <= b <= c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe ">= operator - left associativity" do
    test "simple greater than or equal" do
      code = "a >= b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained >= - left associative" do
      code = "a >= b >= c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "mixed comparison operators" do
    test "< and > mixed - left associative" do
      code = "a < b > c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<= and >= mixed - left associative" do
      code = "a <= b >= c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all comparison operators mixed" do
      code = "a < b <= c > d >= e"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "== operator - left associativity" do
    test "simple equality" do
      code = "a == b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained == - left associative" do
      code = "a == b == c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "== has lower precedence than <" do
      code = "a < b == c < d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "== has higher precedence than &&" do
      code = "a == b && c == d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "!= operator - left associativity" do
    test "simple inequality" do
      code = "a != b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained != - left associative" do
      code = "a != b != c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "=~ operator - left associativity" do
    test "simple match" do
      code = ~S'"hello" =~ ~r/ell/'
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained =~ - left associative" do
      code = "a =~ b =~ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "=== operator - left associativity" do
    test "simple strict equality" do
      code = "a === b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained === - left associative" do
      code = "a === b === c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "!== operator - left associativity" do
    test "simple strict inequality" do
      code = "a !== b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained !== - left associative" do
      code = "a !== b !== c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "mixed equality operators" do
    test "== and != mixed - left associative" do
      code = "a == b != c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=== and !== mixed - left associative" do
      code = "a === b !== c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all equality operators mixed" do
      code = "a == b != c === d !== e =~ f"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "&& operator - left associativity" do
    test "simple and" do
      code = "a && b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained && - left associative" do
      code = "a && b && c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&& has lower precedence than ==" do
      code = "a == b && c == d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&& has higher precedence than ||" do
      code = "a && b || c && d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "&&& operator - left associativity" do
    test "simple &&&" do
      code = "a &&& b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained &&& - left associative" do
      code = "a &&& b &&& c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "and operator - left associativity" do
    test "simple and" do
      code = "a and b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained and - left associative" do
      code = "a and b and c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "and has lower precedence than ==" do
      code = "a == b and c == d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "and has higher precedence than or" do
      code = "a and b or c and d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "mixed AND operators" do
    test "&& and and mixed" do
      code = "a && b and c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&&& with && and and" do
      code = "a &&& b && c and d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "|| operator - left associativity" do
    test "simple or" do
      code = "a || b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained || - left associative" do
      code = "a || b || c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|| has lower precedence than &&" do
      code = "a && b || c && d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|| has higher precedence than =" do
      code = "a = b || c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "||| operator - left associativity" do
    test "simple |||" do
      code = "a ||| b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained ||| - left associative" do
      code = "a ||| b ||| c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "or operator - left associativity" do
    test "simple or" do
      code = "a or b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained or - left associative" do
      code = "a or b or c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "or has lower precedence than and" do
      code = "a and b or c and d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "mixed OR operators" do
    test "|| and or mixed" do
      code = "a || b or c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "||| with || and or" do
      code = "a ||| b || c or d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "= operator - right associativity" do
    test "simple match" do
      code = "a = b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained = - right associative" do
      # a = b = c should be a = (b = c)
      code = "a = b = c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple chained =" do
      code = "a = b = c = d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "= has lower precedence than ||" do
      code = "a = b || c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "= has higher precedence than &" do
      code = "a = &b/1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pattern match with tuple" do
      code = "{a, b} = {1, 2}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pattern match with list" do
      code = "[h | t] = [1, 2, 3]"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "& operator - unary" do
    test "capture function" do
      code = "&foo/1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with module" do
      code = "&Foo.bar/2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with expression" do
      code = "&(&1 + 1)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with multiple args" do
      code = "&(&1 + &2)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "& has lower precedence than =" do
      code = "f = &foo/1"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "... operator - unary" do
    test "bare ellipsis expression" do
      code = "..."
      assert spitfire_parse(code) == s2q(code)
    end

    test "ellipsis inside anonymous function body" do
      code = "fn -> ... end"
      assert spitfire_parse(code) == s2q(code)
    end

    test "= has higher precedence than ..." do
      code = "... = a = b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "ellipsis keeps inner arithmetic precedence" do
      code = "... + 1 * 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "ellipsis can be captured" do
      code = "&..."
      assert spitfire_parse(code) == s2q(code)
    end

    test "ellipsis as term before infix operator" do
      code = "... * 1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "ellipsis as term before range" do
      code = "... .. 1"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "=> operator - right associativity in maps" do
    test "simple map with =>" do
      code = "%{a => b}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "map with multiple =>" do
      code = "%{a => b, c => d}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "nested map with =>" do
      code = "%{a => %{b => c}}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=> with complex keys" do
      code = "%{1 + 2 => 3}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=> with complex values" do
      code = "%{a => b + c}"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "| operator - right associativity" do
    test "simple cons" do
      code = "[a | b]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "cons with multiple elements" do
      code = "[a, b | c]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "map update with |" do
      code = "%{map | a: 1}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "struct update with |" do
      code = "%Foo{struct | a: 1}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "| is right associative across repeated operators" do
      code = "a | b | c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "| has lower precedence than =>" do
      code = "%{a => b | c}"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe ":: operator - right associativity" do
    test "simple type annotation" do
      code = "a :: integer"
      assert spitfire_parse(code) == s2q(code)
    end

    test "bitstring type" do
      code = "<<a::8>>"
      assert spitfire_parse(code) == s2q(code)
    end

    test "bitstring with size" do
      code = "<<a::size(8)>>"
      assert spitfire_parse(code) == s2q(code)
    end

    test "bitstring with multiple specs" do
      code = "<<a::8, b::binary>>"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained :: - right associative" do
      code = "a :: b :: c"
      assert spitfire_parse(code) == s2q(code)
    end

    test ":: has lower precedence than |" do
      code = "a | b :: c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "when operator - right associativity" do
    test "simple guard" do
      code = "def foo(a) when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "simple guard - bracket" do
      code = "def foo[a] when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "simple guard - string" do
      code = "def \"asd\" when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)

      code = "def 'asd' when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)

      code = "def \"\"\"\nasd\n\"\"\" when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)

      code = "def '''\nasd\n\''' when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)

      code = "def ~c'asd' when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)

      code = "def :\"asd\" when is_integer(a), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple guards with and" do
      code = "def foo(a) when is_integer(a) and a > 0, do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple guards with or" do
      code = "def foo(a) when is_integer(a) or is_float(a), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained when - right associative" do
      code = "a when b when c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "when has lower precedence than ::" do
      code = "a :: b when c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "<- operator - left associativity" do
    test "simple generator" do
      code = "for x <- [1, 2, 3], do: x"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple generators" do
      code = "for x <- xs, y <- ys, do: {x, y}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "with expression" do
      code = "with {:ok, a} <- foo(), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<- has lower precedence than when" do
      code = "for x when is_integer(x) <- xs, do: x"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<- groups left to right when chained" do
      code = "a <- b <- c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "\\\\ operator - left associativity" do
    test "simple default argument" do
      code = "def foo(a \\\\ 1), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple default arguments" do
      code = "def foo(a \\\\ 1, b \\\\ 2), do: {a, b}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "default with complex expression" do
      code = "def foo(a \\\\ 1 + 2), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "\\\\ groups left to right when chained" do
      code = "a \\\\ b \\\\ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence interactions" do
    test "arithmetic vs logical" do
      code = "1 + 2 == 3 and 4 - 1 == 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "comparison vs logical" do
      code = "a < b && c > d || e <= f"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pipe vs arithmetic" do
      code = "1 + 2 |> foo() |> bar() + 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all arithmetic operators" do
      code = "1 + 2 * 3 ** 4 / 5 - 6"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary and binary operators" do
      code = "-1 + -2 * -3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "complex expression with many operators" do
      code = "@foo.bar |> baz() == 1 + 2 * 3 and !flag || default"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with arithmetic" do
      code = "1 + 2..3 * 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "list operators with comparison" do
      code = "[1] ++ [2] == [1, 2]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "string concat with comparison" do
      code = ~S'"a" <> "b" == "ab"'
      assert spitfire_parse(code) == s2q(code)
    end

    test "match with pipe" do
      code = "result = a |> b |> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with match" do
      code = "fun = &Foo.bar/2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "deep nesting of operators" do
      code = "a + b * c ** d / e - f"
      assert spitfire_parse(code) == s2q(code)
    end

    test "boolean expression chain" do
      code = "a and b or c and d or e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "relaxed boolean chain" do
      code = "a && b || c && d || e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "mixed boolean operators" do
      code = "a and b && c or d || e"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "left associativity verification" do
    test "left associative operators group left to right" do
      # For left associative: a op b op c == (a op b) op c
      # The AST should show the leftmost operation as the innermost

      # Multiplication
      code = "a * b * c"
      assert spitfire_parse(code) == s2q(code)

      # Addition
      code = "a + b + c"
      assert spitfire_parse(code) == s2q(code)

      # Comparison
      code = "a < b < c"
      assert spitfire_parse(code) == s2q(code)

      # Logical and
      code = "a && b && c"
      assert spitfire_parse(code) == s2q(code)

      # Pipe
      code = "a |> b |> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "right associativity verification" do
    test "right associative operators group right to left" do
      # For right associative: a op b op c == a op (b op c)
      # The AST should show the rightmost operation as the innermost

      # List concat
      code = "a ++ b ++ c"
      assert spitfire_parse(code) == s2q(code)

      # Match
      code = "a = b = c"
      assert spitfire_parse(code) == s2q(code)

      # Type
      code = "a :: b :: c"
      assert spitfire_parse(code) == s2q(code)

      # When
      code = "a when b when c"
      assert spitfire_parse(code) == s2q(code)

      # Range
      code = "a..b..c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "nullary .. operator" do
    test "bare range" do
      code = ".."
      assert spitfire_parse(code) == s2q(code)
    end

    test "nullary range in function call" do
      code = "Enum.to_list(..)"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "..// operator" do
    test "simple range with step" do
      code = "1..10//2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with negative step" do
      code = "10..1//-1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with step and variables" do
      code = "a..b//c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with step has correct precedence with +" do
      code = "1 + 2..3 + 4//5"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with step in comprehension" do
      code = "for i <- 1..10//2, do: i"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: @ (highest) vs . (second highest)" do
    test "@ binds tighter than dot on result" do
      code = "@foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "@ with chained dots" do
      code = "@foo.bar.baz"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: . vs unary +/-/!/^/not" do
    test "dot binds tighter than unary minus" do
      code = "-foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot binds tighter than unary plus" do
      code = "+foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot binds tighter than unary not" do
      code = "!foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot binds tighter than not keyword" do
      code = "not foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot binds tighter than pin" do
      code = "^foo.bar"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: unary +/-/!/^/not vs **" do
    test "unary minus binds tighter than **" do
      code = "-2 ** 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary plus binds tighter than **" do
      code = "+2 ** 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary ! with ** operand" do
      code = "!a ** b"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: ** vs * /" do
    test "** binds tighter than *" do
      code = "2 * 3 ** 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "** binds tighter than /" do
      code = "8 / 2 ** 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "* after ** expression" do
      code = "2 ** 3 * 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "/ after ** expression" do
      code = "2 ** 3 / 4"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: * / vs + -" do
    test "* binds tighter than +" do
      code = "1 + 2 * 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "/ binds tighter than -" do
      code = "6 - 4 / 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "* binds tighter than -" do
      code = "6 - 2 * 2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "/ binds tighter than +" do
      code = "1 + 6 / 2"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: + - vs ++ -- +++ --- .. <>" do
    test "+ binds tighter than ++" do
      code = "a + b ++ c + d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "- binds tighter than --" do
      code = "a - b -- c - d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "+ binds tighter than .." do
      code = "1 + 2..3 + 4"
      assert spitfire_parse(code) == s2q(code)
    end

    test "+ binds tighter than <>" do
      code = "a + b <> c + d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "- binds tighter than +++" do
      code = "a - b +++ c - d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "- binds tighter than ---" do
      code = "a - b --- c - d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: ++ -- +++ --- .. <> vs in/not in" do
    test "++ binds tighter than in" do
      code = "a in b ++ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "-- binds tighter than in" do
      code = "a in b -- c"
      assert spitfire_parse(code) == s2q(code)
    end

    test ".. binds tighter than in" do
      code = "a in 1..10"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<> binds tighter than in" do
      code = "a in b <> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "++ binds tighter than not in" do
      code = "a not in b ++ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: in/not in vs |> <<< >>> <<~ ~>> <~ ~> <~>" do
    test "in binds tighter than |>" do
      code = "a in b |> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not in binds tighter than |>" do
      code = "a not in b |> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than <<<" do
      code = "a in b <<< c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than >>>" do
      code = "a in b >>> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than ~>" do
      code = "a in b ~> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than <~" do
      code = "a in b <~ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than <~>" do
      code = "a in b <~> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than <<~" do
      code = "a in b <<~ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "in binds tighter than ~>>" do
      code = "a in b ~>> c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: |> <<< >>> <<~ ~>> <~ ~> <~> vs < > <= >=" do
    test "|> binds tighter than <" do
      code = "a |> b < c |> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|> binds tighter than >" do
      code = "a |> b > c |> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|> binds tighter than <=" do
      code = "a |> b <= c |> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|> binds tighter than >=" do
      code = "a |> b >= c |> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<<< binds tighter than <" do
      code = "a <<< b < c <<< d"
      assert spitfire_parse(code) == s2q(code)
    end

    test ">>> binds tighter than >" do
      code = "a >>> b > c >>> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "~> binds tighter than <" do
      code = "a ~> b < c ~> d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: < > <= >= vs == != =~ === !==" do
    test "< binds tighter than ==" do
      code = "a < b == c < d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "> binds tighter than !=" do
      code = "a > b != c > d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<= binds tighter than ===" do
      code = "a <= b === c <= d"
      assert spitfire_parse(code) == s2q(code)
    end

    test ">= binds tighter than !==" do
      code = "a >= b !== c >= d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "< binds tighter than =~" do
      code = "a < b =~ c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: == != =~ === !== vs && &&& and" do
    test "== binds tighter than &&" do
      code = "a == b && c == d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "!= binds tighter than &&&" do
      code = "a != b &&& c != d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=== binds tighter than and" do
      code = "a === b and c === d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "!== binds tighter than &&" do
      code = "a !== b && c !== d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=~ binds tighter than and" do
      code = "a =~ b and c =~ d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: && &&& and vs || ||| or" do
    test "&& binds tighter than ||" do
      code = "a && b || c && d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&&& binds tighter than |||" do
      code = "a &&& b ||| c &&& d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "and binds tighter than or" do
      code = "a and b or c and d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&& binds tighter than or" do
      code = "a && b or c && d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "and binds tighter than ||" do
      code = "a and b || c and d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: || ||| or vs =" do
    test "|| binds tighter than =" do
      code = "a = b || c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "||| binds tighter than =" do
      code = "a = b ||| c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "or binds tighter than =" do
      code = "a = b or c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|| on both sides of =" do
      code = "a || b = c || d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: = vs & ..." do
    test "= binds tighter than &" do
      code = "f = &foo/1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "& captures result of =" do
      code = "&(a = b)"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: & vs =>" do
    test "& and => in map" do
      code = "%{&foo/1 => 1}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=> with capture on both sides" do
      code = "%{&foo/1 => &bar/2}"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: => vs |" do
    test "=> binds tighter than | in map" do
      code = "%{a => b | c}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "| in map update with =>" do
      code = "%{map | a => b}"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: | vs ::" do
    test "| binds tighter than ::" do
      code = "a | b :: c"
      assert spitfire_parse(code) == s2q(code)
    end

    test ":: after | in type spec context" do
      code = "@spec foo(a | b :: c)"
      assert spitfire_parse(code) == s2q(code)
    end

    test ":: after | in type spec context - bracket" do
      code = "@spec foo[a | b :: c]"
      assert spitfire_parse(code) == s2q(code)
    end

    test ":: after | in type spec context - string" do
      code = "@spec \"sdc\", (a | b :: c)"
      assert spitfire_parse(code) == s2q(code)

      code = "@spec 'sdc', (a | b :: c)"
      assert spitfire_parse(code) == s2q(code)

      code = "@spec :\"sdc\", (a | b :: c)"
      assert spitfire_parse(code) == s2q(code)

      code = "@spec \"\"\"\nsdc\n\"\"\", (a | b :: c)"
      assert spitfire_parse(code) == s2q(code)

      code = "@spec '''\nsdc\n''', (a | b :: c)"
      assert spitfire_parse(code) == s2q(code)

      code = "@spec ~c'sdc', (a | b :: c)"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: :: vs when" do
    test ":: binds tighter than when" do
      code = "a :: b when c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "when with typed parameter" do
      code = "@spec foo(a :: integer) when a: term"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "precedence boundaries: when vs <- \\\\" do
    test "when binds tighter than <-" do
      code = "for x when is_integer(x) <- xs, do: x"
      assert spitfire_parse(code) == s2q(code)
    end

    test "when binds tighter than \\\\" do
      code = "x when true \\\\ default"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<- with when guard" do
      code = "for x when x > 0 <- list, do: x"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: ++ -- +++ --- .. <>" do
    test "++ and -- at same precedence" do
      code = "a ++ b -- c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "++ and .. at same precedence" do
      code = "a ++ 1..10"
      assert spitfire_parse(code) == s2q(code)
    end

    test "-- and <> at same precedence" do
      code = "a -- b <> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "+++ and --- at same precedence" do
      code = "a +++ b --- c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all list ops mixed" do
      code = "a ++ b -- c +++ d --- e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<> and ++ and .." do
      code = "a <> b ++ 1..10"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: |> <<< >>> <<~ ~>> <~ ~> <~>" do
    test "|> and <<< at same precedence" do
      code = "a |> b <<< c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|> and >>> at same precedence" do
      code = "a |> b >>> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<<< and >>> at same precedence" do
      code = "a <<< b >>> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<<~ and ~>> at same precedence" do
      code = "a <<~ b ~>> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<~ and ~> at same precedence" do
      code = "a <~ b ~> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<~> with other arrow ops" do
      code = "a <~> b |> c ~> d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all arrow ops mixed" do
      code = "a |> b <<< c >>> d <<~ e ~>> f <~ g ~> h <~> i"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: < > <= >=" do
    test "< and > at same precedence" do
      code = "a < b > c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "<= and >= at same precedence" do
      code = "a <= b >= c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "< and >= at same precedence" do
      code = "a < b >= c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "> and <= at same precedence" do
      code = "a > b <= c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all comparison ops" do
      code = "a < b > c <= d >= e"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: == != =~ === !==" do
    test "== and != at same precedence" do
      code = "a == b != c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "=== and !== at same precedence" do
      code = "a === b !== c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "== and =~ at same precedence" do
      code = "a == b =~ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all equality ops" do
      code = "a == b != c =~ d === e !== f"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: && &&& and" do
    test "&& and &&& at same precedence" do
      code = "a && b &&& c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&& and and at same precedence" do
      code = "a && b and c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "&&& and and at same precedence" do
      code = "a &&& b and c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all AND ops" do
      code = "a && b &&& c and d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: || ||| or" do
    test "|| and ||| at same precedence" do
      code = "a || b ||| c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "|| and or at same precedence" do
      code = "a || b or c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "||| and or at same precedence" do
      code = "a ||| b or c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all OR ops" do
      code = "a || b ||| c or d"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "same precedence level: <- \\\\" do
    test "<- and \\\\ at same precedence" do
      code = "a <- b \\\\ c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "\\\\ and <- at same precedence" do
      code = "a \\\\ b <- c"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "edge cases" do
    test "parentheses override precedence" do
      code = "(1 + 2) * 3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "nested parentheses" do
      code = "((1 + 2) * (3 + 4))"
      assert spitfire_parse(code) == s2q(code)
    end

    test "operator with newline" do
      code = "1 +\n2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "chained operators with newlines" do
      code = "1 +\n2 *\n3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pipe with newlines" do
      code = "a\n|> b\n|> c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary operators with parens" do
      code = "-(1 + 2)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not with parens" do
      code = "not (a and b)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "! with parens" do
      code = "!(a && b)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "access syntax with operators" do
      code = "foo[a + b]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "function call with operator expression" do
      code = "foo(a + b, c * d)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "deeply nested arithmetic" do
      code = "1 + 2 * 3 ** 4 / 5 - 6 + 7 * 8"
      assert spitfire_parse(code) == s2q(code)
    end

    test "mixed unary operators" do
      code = "- - - -a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary not chain" do
      code = "not not not a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary ! chain" do
      code = "!!!a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "complex pipe chain" do
      code = "a |> b() |> c(1, 2) |> d.e() |> f"
      assert spitfire_parse(code) == s2q(code)
    end

    test "binary operator after pipe" do
      code = "a |> b + c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "comparison chain" do
      code = "1 < 2 <= 3 > 0 >= -1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "match in if condition" do
      code = "if a = b, do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "operator in anonymous function" do
      code = "fn a, b -> a + b end"
      assert spitfire_parse(code) == s2q(code)
    end

    test "operator in case clause" do
      code = "case a do\n  x when x > 0 -> x * 2\n  _ -> 0\nend"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple matches on same line" do
      code = "a = b = c = 1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range in case pattern" do
      code = "case x do\n  n when n in 1..10 -> :small\n  _ -> :large\nend"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "full precedence chain tests" do
    test "expression using operators from many precedence levels" do
      # @ > . > unary > ** > * > + > ++ > in > |> > < > == > && > || > =
      code = "result = a || b && c == d < e |> f in g ++ h + i * j ** k"
      assert spitfire_parse(code) == s2q(code)
    end

    test "complex boolean expression with all logical operators" do
      code = "a and b && c or d || e and f && g or h"
      assert spitfire_parse(code) == s2q(code)
    end

    test "arithmetic with all arithmetic operators" do
      code = "a ** b * c / d + e - f"
      assert spitfire_parse(code) == s2q(code)
    end

    test "list operations chain" do
      code = "a ++ b -- c ++ d -- e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "all comparison operators in sequence" do
      code = "a < b > c <= d >= e == f != g === h !== i =~ j"
      assert spitfire_parse(code) == s2q(code)
    end

    test "@ with full operator chain" do
      code = "@foo + 1 * 2 == 3 and true"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot call with full operator chain" do
      code = "a.b + c.d * e.f == g.h"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with complex expression" do
      code = "&(&1 + &2 * &3)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "match with complex right side" do
      code = "{a, b} = c ++ d |> e"
      assert spitfire_parse(code) == s2q(code)
    end

    test "guard clause with multiple operators" do
      code = "def foo(a, b) when is_integer(a) and a > 0 and b < 100 or is_float(a), do: a + b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "type spec with union and constraints" do
      code = "@spec foo(a :: integer | float, b :: atom) :: boolean when a: number"
      assert spitfire_parse(code) == s2q(code)
    end

    test "comprehension with complex generators and filters" do
      code = "for x <- xs, x > 0, y <- ys, x + y < 10, do: {x, y}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "with expression with multiple clauses" do
      code = "with {:ok, a} <- foo(), {:ok, b} <- bar(a), c = a + b, do: c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "bitstring with multiple type specs" do
      code = "<<a::8, b::16-big-unsigned, c::binary-size(4), rest::binary>>"
      assert spitfire_parse(code) == s2q(code)
    end

    test "map with arrow and expression values" do
      code = "%{a + b => c * d, e => f || g}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "struct with update and expressions" do
      code = "%Foo{bar | a: b + c, d: e * f}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "nested data structures with operators" do
      code = "[a: b + c, d: [e * f, g | h]]"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  describe "regression and tricky cases" do
    test "minus after dot without space" do
      code = "a.b-c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "plus after dot without space" do
      code = "a.b+c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary minus in function call" do
      code = "foo(-1)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary minus in list" do
      code = "[-1, -2, -3]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "unary plus in tuple" do
      code = "{+1, +2}"
      assert spitfire_parse(code) == s2q(code)
    end

    test "not in with complex expression" do
      code = "a + b not in c ++ d"
      assert spitfire_parse(code) == s2q(code)
    end

    test "double pipe confusion" do
      # || is boolean or, | is cons/union
      code = "a || [b | c]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "for generator rhs with || [] inside assoc-map list value" do
      code = ~S(%{"s" => [for x <- y || [] do x end]})

      assert spitfire_parse(code) == s2q(code)
    end

    test "triple less than" do
      # <<< is custom operator, << is bitstring
      code = "a <<< b"
      assert spitfire_parse(code) == s2q(code)
    end

    test "power with negative exponent" do
      code = "2 ** -3"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with negative bounds" do
      code = "-10..-1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "range with negative step" do
      code = "10..1//-1"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with arithmetic" do
      code = "&(&1 + 1)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pin in match with binary operator" do
      code = "^a = b + c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "when in anonymous function" do
      code = "fn x when x > 0 -> x end"
      assert spitfire_parse(code) == s2q(code)
    end

    test "multiple when clauses" do
      code = "fn x when is_integer(x) when x > 0 -> x end"
      assert spitfire_parse(code) == s2q(code)
    end

    test "default argument with complex expression" do
      code = "def foo(a \\\\ 1 + 2 * 3), do: a"
      assert spitfire_parse(code) == s2q(code)
    end

    test "access in operator expression" do
      code = "a[b] + c[d]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "parens in operator expression" do
      code = "a(b) + c(d)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "dot call on result of operator" do
      code = "(a + b).c"
      assert spitfire_parse(code) == s2q(code)
    end

    test "operators in sigil" do
      code = "~w(a + b)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "string interpolation with operators" do
      code = ~S'"result: #{a + b * c}"'
      assert spitfire_parse(code) == s2q(code)
    end

    test "heredoc with operators" do
      code = ~s'"""\n\#{a + b}\n"""'
      assert spitfire_parse(code) == s2q(code)
    end

    test "keyword list with operator values" do
      code = "[a: b + c, d: e * f]"
      assert spitfire_parse(code) == s2q(code)
    end

    test "keyword argument in function call" do
      code = "foo(a, b: c + d)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "pipe into keyword function" do
      code = "a |> foo(b: c)"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture with module function" do
      code = "&Mod.fun/2"
      assert spitfire_parse(code) == s2q(code)
    end

    test "capture placeholder in operator" do
      code = "&(&1 <> &2)"
      assert spitfire_parse(code) == s2q(code)
    end
  end

  defp spitfire_parse(code, _options \\ []) do
    case Spitfire.parse(code) do
      {:ok, ast} -> {:ok, ast}
      {:error, _ast, _errors} -> {:error, :parse_error}
      {:error, :no_fuel_remaining} -> {:error, :no_fuel_remaining}
    end
  end

  defp s2q(code, opts \\ []) do
    Code.string_to_quoted(
      code,
      Keyword.merge([columns: true, token_metadata: true, emit_warnings: false], opts)
    )
  end
end
