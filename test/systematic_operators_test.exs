defmodule Spitfire.SystematicOperatorsTest do
  use ExUnit.Case, async: true

  # =============================================================================
  # Operator Classifications
  # =============================================================================
  #
  # Based on Elixir operator precedence table (highest to lowest):
  #
  # Operator                                       | Associativity
  # ---------------------------------------------- | -------------
  # `@`                                            | Unary
  # `.`                                            | Left
  # `+` `-` `!` `^` `not` `~~~`                    | Unary
  # `**`                                           | Left
  # `*` `/`                                        | Left
  # `+` `-`                                        | Left
  # `++` `--` `+++` `---` `..` `<>`                | Right
  # `^^^`                                          | Left
  # `in` `not in`                                  | Left
  # `|>` `<<<` `>>>` `<<~` `~>>` `<~` `~>` `<~>`   | Left
  # `<` `>` `<=` `>=`                              | Left
  # `==` `!=` `=~` `===` `!==`                     | Left
  # `&&` `&&&` `and`                               | Left
  # `||` `|||` `or`                                | Left
  # `=`                                            | Right
  # `&`, `...`                                     | Unary
  # `=>` (valid only inside `%{}`)                 | Right
  # `|`                                            | Right
  # `::`                                           | Right
  # `when`                                         | Right
  # `<-` `\\`                                      | Left
  # =============================================================================

  # Unary operators (note: @ and & have special syntax requirements)
  @unary_ops ~w(@ + - ! ^ not & ... ~~~)a

  # Unary operators that can be freely combined with binary operators
  @simple_unary_ops ~w(+ - ! ^ not ~~~)a

  # Binary operators organized by precedence level
  @binary_ops [
    :.,
    :**,
    :*,
    :/,
    :+,
    :-,
    :++,
    :--,
    :+++,
    :---,
    :..,
    :<>,
    :"^^^",
    :in,
    :"not in",
    :|>,
    :<<<,
    :>>>,
    :<<~,
    :~>>,
    :<~,
    :~>,
    :<~>,
    :<,
    :>,
    :<=,
    :>=,
    :==,
    :!=,
    :=~,
    :===,
    :!==,
    :&&,
    :&&&,
    :and,
    :||,
    :|||,
    :or,
    :=,
    :|,
    :"::",
    :when,
    :<-,
    :\\
  ]

  # Binary operators that work with simple variable operands
  @simple_binary_ops [
    :**,
    :*,
    :/,
    :+,
    :-,
    :++,
    :--,
    :+++,
    :---,
    :..,
    :<>,
    :"^^^",
    :in,
    :"not in",
    :|>,
    :<<<,
    :>>>,
    :<<~,
    :~>>,
    :<~,
    :~>,
    :<~>,
    :<,
    :>,
    :<=,
    :>=,
    :==,
    :!=,
    :=~,
    :===,
    :!==,
    :&&,
    :&&&,
    :and,
    :||,
    :|||,
    :or,
    :=,
    :|,
    :"::",
    :when,
    :<-,
    :\\
  ]

  # Right-associative binary operators
  @right_assoc_ops ~w(++ -- +++ --- .. <> = | :: when)a

  # Left-associative binary operators
  @left_assoc_ops ~w(** * / + - ^^^ in |> <<< >>> <<~ ~>> <~ ~> <~> < > <= >= == != =~ === !== && &&& and || ||| or <- \\)a

  # 3 expression kinds: matched_expr, unmatched_expr and no_parens_expr
  @expressions [
    :matched,
    :unmatched,
    :no_parens
  ]

  defp gen_expr(:matched, name) do
    "#{String.capitalize(name)}Matched.#{name}_matched(#{name}_matched_arg)"
  end

  defp gen_expr(:unmatched, name) do
    "#{String.capitalize(name)}Unmatched.#{name}_unmatched do #{name}_unmatched_some end"
  end

  defp gen_expr(:no_parens, name) do
    "#{String.capitalize(name)}NoParens.#{name}_no_parens #{name}_no_parens_arg1, #{name}_no_parens_arg2"
  end

  # Helper to convert atom to string representation
  defp op_to_string(:"not in"), do: "not in"
  defp op_to_string(op), do: Atom.to_string(op)

  defp unary_op_to_string(:not), do: "not "
  defp unary_op_to_string(op), do: Atom.to_string(op)

  defp s2q(code) do
    Code.string_to_quoted(code, columns: true, token_metadata: true, emit_warnings: false)
  rescue
    _e -> {:error, :reference_parser_crash}
  end

  describe "systematic operator combinations" do
    test "binary - binary combinations (a op1 b op2 c)" do
      failures =
        for op1 <- @binary_ops,
            op2 <- @binary_ops,
            expr_a <- @expressions -- [:no_parens],
            expr_b <- @expressions -- [:no_parens],
            expr_c <- @expressions do
          s_op1 = op_to_string(op1)
          s_op2 = op_to_string(op2)

          code =
            "#{gen_expr(expr_a, "a")} #{s_op1} #{gen_expr(expr_b, "b")} #{s_op2} #{gen_expr(expr_c, "c")}"

          check(code)
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures |> Enum.take(5), pretty: true, limit: :infinity)}"
    end

    test "unary - binary combinations (op1 a op2 b)" do
      failures =
        for op1 <- @unary_ops,
            op2 <- @binary_ops,
            expr_a <- @expressions -- [:no_parens],
            expr_b <- @expressions do
          s_op1 = unary_op_to_string(op1)
          s_op2 = op_to_string(op2)

          code = "#{s_op1}#{gen_expr(expr_a, "a")} #{s_op2} #{gen_expr(expr_b, "b")}"

          check(code)
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "binary - unary combinations (a op1 op2 b)" do
      failures =
        for op1 <- @binary_ops,
            op2 <- @unary_ops,
            expr_a <- @expressions -- [:no_parens],
            expr_b <- @expressions do
          s_op1 = op_to_string(op1)
          s_op2 = unary_op_to_string(op2)

          code = "#{gen_expr(expr_a, "a")} #{s_op1} #{s_op2}#{gen_expr(expr_b, "b")}"

          check(code)
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures |> Enum.take(5), pretty: true, limit: :infinity)}"
    end

    test "ternary range (a..b//c) combinations" do
      failures =
        for op <- @binary_ops,
            expr_a <- @expressions -- [:no_parens],
            expr_b <- @expressions -- [:no_parens],
            expr_c <- @expressions -- [:no_parens],
            expr_d <- @expressions -- [] do
          s_op = op_to_string(op)

          code1 =
            "#{gen_expr(expr_a, "a")} #{s_op} #{gen_expr(expr_b, "b")}..#{gen_expr(expr_c, "c")}//#{gen_expr(expr_d, "d")}"

          code2 =
            "#{gen_expr(expr_a, "a")}..#{gen_expr(expr_b, "b")} #{s_op} #{gen_expr(expr_c, "c")}//#{gen_expr(expr_d, "d")}"

          code3 =
            "#{gen_expr(expr_a, "a")}..#{gen_expr(expr_b, "b")}//#{gen_expr(expr_c, "c")} #{s_op} #{gen_expr(expr_d, "d")}"

          [
            check(code1),
            check(code2),
            check(code3)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "ternary range with unary operators" do
      failures =
        for op <- @unary_ops,
            expr_a <- @expressions -- [:no_parens],
            expr_b <- @expressions -- [:no_parens],
            expr_c <- @expressions -- [] do
          s_op = unary_op_to_string(op)

          [
            check(
              "#{s_op}#{gen_expr(expr_a, "a")}..#{gen_expr(expr_b, "b")}//#{gen_expr(expr_c, "c")}"
            ),
            check(
              "#{gen_expr(expr_a, "a")}..#{s_op}#{gen_expr(expr_b, "b")}//#{gen_expr(expr_c, "c")}"
            ),
            check(
              "#{gen_expr(expr_a, "a")}..#{gen_expr(expr_b, "b")}//#{s_op}#{gen_expr(expr_c, "c")}"
            )
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "list update binary (a | b op c) combinations" do
      failures =
        for op <- @binary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [],
            expr_c <- @expressions -- [] do
          s_op = op_to_string(op)

          # Inside tl
          code1 =
            "[#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} #{s_op} #{gen_expr(expr_c, "c")}]"

          # Inside hd
          code2 =
            "[#{gen_expr(expr_a, "a")} #{s_op} #{gen_expr(expr_b, "b")} | #{gen_expr(expr_c, "c")}]"

          [
            check(code1),
            check(code2)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      all_failures = failures

      assert all_failures == [],
             "Failed combinations: #{inspect(all_failures, pretty: true, limit: :infinity)}"
    end

    test "list update unary (a | op b) combinations" do
      failures =
        for op <- @unary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [] do
          s_op = unary_op_to_string(op)

          # Inside tl
          code1 =
            "[#{gen_expr(expr_a, "a")} | #{s_op}#{gen_expr(expr_b, "b")}]"

          # Inside hd
          code2 =
            "[#{s_op}#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")}]"

          [
            check(code1),
            check(code2)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      all_failures = failures

      assert all_failures == [],
             "Failed combinations: #{inspect(all_failures, pretty: true, limit: :infinity)}"
    end

    test "stab binary (a -> b op c) combinations" do
      failures =
        for op <- @binary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [],
            expr_c <- @expressions -- [] do
          s_op = op_to_string(op)

          # Inside tl
          code1 =
            "(#{gen_expr(expr_a, "a")} -> #{gen_expr(expr_b, "b")} #{s_op} #{gen_expr(expr_c, "c")})"

          # Inside hd
          code2 =
            "(#{gen_expr(expr_a, "a")} #{s_op} #{gen_expr(expr_b, "b")} -> #{gen_expr(expr_c, "c")})"

          [
            check(code1),
            check(code2)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      all_failures = failures

      assert all_failures == [],
             "Failed combinations: #{inspect(all_failures, pretty: true, limit: :infinity)}"
    end

    test "stab unary (a -> op b) combinations" do
      failures =
        for op <- @unary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [] do
          s_op = unary_op_to_string(op)

          # Inside tl
          code1 =
            "(#{gen_expr(expr_a, "a")} -> #{s_op}#{gen_expr(expr_b, "b")})"

          # Inside hd
          code2 =
            "(#{s_op}#{gen_expr(expr_a, "a")} -> #{gen_expr(expr_b, "b")})"

          [
            check(code1),
            check(code2)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      all_failures = failures

      assert all_failures == [],
             "Failed combinations: #{inspect(all_failures, pretty: true, limit: :infinity)}"
    end

    test "map update (a | b => c op d) combinations" do
      failures =
        for op <- @binary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [],
            expr_c <- @expressions -- [],
            expr_d <- @expressions -- [] do
          s_op = op_to_string(op)

          # Inside values
          code1 =
            "%{#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} => #{gen_expr(expr_c, "c")} #{s_op} #{gen_expr(expr_d, "d")}}"

          # Inside keys
          code2 =
            "%{#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} #{s_op} #{gen_expr(expr_c, "c")} => #{gen_expr(expr_d, "d")}}"

          # Inside struct
          code3 =
            "%{#{gen_expr(expr_a, "a")} #{s_op} #{gen_expr(expr_b, "b")} | #{gen_expr(expr_c, "c")} => #{gen_expr(expr_d, "d")}}"

          # Inside assoc
          code4 =
            "%{#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} #{s_op} #{gen_expr(expr_c, "c")}}"

          [
            check(code1),
            check(code2),
            check(code3),
            check(code4)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      all_failures = failures

      assert all_failures == [],
             "Failed combinations: #{inspect(all_failures, pretty: true, limit: :infinity)}"
    end

    test "map update unary (a | b => op c) combinations" do
      failures =
        for op <- @unary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [],
            expr_c <- @expressions -- [] do
          s_op = unary_op_to_string(op)

          # Inside values
          code1 =
            "%{#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} => #{s_op}#{gen_expr(expr_c, "c")}}"

          # Inside keys
          code2 =
            "%{#{gen_expr(expr_a, "a")} | #{s_op}#{gen_expr(expr_b, "b")} => #{gen_expr(expr_c, "c")}}"

          # Inside struct
          code3 =
            "%{#{s_op}#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} => #{gen_expr(expr_c, "c")}}"

          # Inside assoc
          code4 =
            "%{#{gen_expr(expr_a, "a")} | #{s_op}#{gen_expr(expr_b, "b")}}"

          [
            check(code1),
            check(code2),
            check(code3),
            check(code4)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      all_failures = failures

      assert all_failures == [],
             "Failed combinations: #{inspect(all_failures, pretty: true, limit: :infinity)}"
    end

    test "ternary range between two binary operators (a op1 b..c//d op2 e)" do
      failures =
        for op1 <- @binary_ops,
            op2 <- @binary_ops,
            expr_a <- @expressions -- [:no_parens],
            expr_b <- @expressions -- [:no_parens],
            expr_c <- @expressions -- [:no_parens],
            expr_d <- @expressions -- [:no_parens],
            expr_e <- @expressions -- [] do
          s1 = op_to_string(op1)
          s2 = op_to_string(op2)

          check(
            "#{gen_expr(expr_a, "a")} #{s1} #{gen_expr(expr_b, "b")}..#{gen_expr(expr_c, "c")}//#{gen_expr(expr_d, "d")} #{s2} #{gen_expr(expr_e, "e")}"
          )
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "map update between two binary operators (a op1 %{m | k => v} op2 b)" do
      failures =
        for op1 <- @binary_ops, op2 <- @binary_ops do
          s1 = op_to_string(op1)
          s2 = op_to_string(op2)

          check("a #{s1} %{m | k => v} #{s2} b")
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "map update with unary operators" do
      failures =
        for op <- @simple_unary_ops,
            expr_a <- @expressions -- [],
            expr_b <- @expressions -- [],
            expr_c <- @expressions -- [] do
          s_op = unary_op_to_string(op)

          [
            check(
              "%{#{s_op}#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} => #{gen_expr(expr_c, "c")}}"
            ),
            check(
              "%{#{gen_expr(expr_a, "a")} | #{s_op}#{gen_expr(expr_b, "b")} => #{gen_expr(expr_c, "c")}}"
            ),
            check(
              "%{#{gen_expr(expr_a, "a")} | #{gen_expr(expr_b, "b")} => #{s_op}#{gen_expr(expr_c, "c")}}"
            )
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  defp check(code) do
    case s2q(code) do
      {:ok, expected} ->
        case Spitfire.parse(code) do
          {:ok, actual} ->
            if actual != expected, do: {code, expected, actual}, else: nil

          {:error, _} ->
            {code, expected, :error}
        end

      {:error, _} ->
        nil
    end
  end

  # =============================================================================
  # Operators with Literals
  # =============================================================================
  # The repro 34 case "not \"\" <> \"\"" shows that operators with literal
  # operands can behave differently than with variables.

  describe "operators with string literals" do
    test "unary operators with string binary operators" do
      failures =
        for op1 <- @simple_unary_ops, op2 <- [:++, :--, :<>, :..] do
          s_op1 = op_to_string(op1)
          s_op2 = op_to_string(op2)

          [
            check(~s'#{s_op1} "" #{s_op2} ""'),
            check(~s'#{s_op1} "foo" #{s_op2} "bar"'),
            check(~s'#{s_op1} a #{s_op2} "bar"'),
            check(~s'#{s_op1} "foo" #{s_op2} b')
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "binary operators with mixed literals and variables" do
      literals = [~s'""', ~s'"foo"', "1", "1.0", ":atom", "'c'", "[]", "{}"]

      failures =
        for lit <- literals, op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("a #{s_op} #{lit}"),
            check("#{lit} #{s_op} a"),
            check("#{lit} #{s_op} #{lit}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Operators with do/end Blocks
  # =============================================================================
  # Many repro cases involve case/try/with/quote/fn expressions

  describe "operators with do/end blocks" do
    @do_blocks [
      "case a do\n  _ -> b\nend",
      "try do\n  a\nrescue\n  _ -> b\nend",
      "with a <- b, do: c",
      "for x <- xs, do: x",
      "quote do\n  a\nend",
      "fn -> a end",
      "fn x -> x end",
      "if a, do: b, else: c",
      "cond do\n  true -> a\nend",
      "receive do\n  _ -> a\nend"
    ]

    test "unary operators with do/end blocks followed by binary operator" do
      failures =
        for unary <- @simple_unary_ops, block <- @do_blocks, binary <- @simple_binary_ops do
          s_unary = unary_op_to_string(unary)
          s_binary = op_to_string(binary)

          code = "#{s_unary}#{block} #{s_binary} a"
          check(code)
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "do/end blocks followed by binary operators" do
      failures =
        for block <- @do_blocks, op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("#{block} #{s_op} a"),
            check("a #{s_op} #{block}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "do/end blocks with operators inside" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("case a #{s_op} b do\n  _ -> c\nend"),
            check("case a do\n  x #{s_op} y -> z\nend"),
            check("fn x -> x #{s_op} y end"),
            check("quote do\n  a #{s_op} b\nend"),
            check("for x <- a #{s_op} b, do: x")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Triple Operator Combinations (4 operands)
  # =============================================================================
  # Tests precedence when three binary operators interact

  describe "triple binary operator combinations" do
    # Use a representative subset to avoid combinatorial explosion
    @precedence_representatives [
      # highest among simple binary
      :**,
      # mult/div level
      :*,
      # add/sub level
      :+,
      # right-associative list ops
      :++,
      # binary concat
      :<>,
      # pipe
      :|>,
      # comparison
      :<,
      # equality
      :==,
      # logical and
      :&&,
      # logical or
      :||,
      # match
      :=
    ]

    test "a op1 b op2 c op3 d" do
      failures =
        for op1 <- @precedence_representatives,
            op2 <- @precedence_representatives,
            op3 <- @precedence_representatives do
          s1 = op_to_string(op1)
          s2 = op_to_string(op2)
          s3 = op_to_string(op3)

          check("a #{s1} b #{s2} c #{s3} d")
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "unary op1 a op2 b op3 c" do
      failures =
        for unary <- @simple_unary_ops,
            op1 <- @precedence_representatives,
            op2 <- @precedence_representatives do
          s_unary = unary_op_to_string(unary)
          s1 = op_to_string(op1)
          s2 = op_to_string(op2)

          check("#{s_unary}a #{s1} b #{s2} c")
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Chained Unary Operators
  # =============================================================================

  describe "chained unary operators" do
    test "double unary operators" do
      failures =
        for op1 <- @simple_unary_ops, op2 <- @simple_unary_ops do
          s1 = unary_op_to_string(op1)
          s2 = unary_op_to_string(op2)

          [
            check("#{s1}#{s2}a"),
            check("#{s1}#{s2}a + b")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "triple unary operators" do
      failures =
        for op1 <- @simple_unary_ops, op2 <- @simple_unary_ops, op3 <- @simple_unary_ops do
          s1 = unary_op_to_string(op1)
          s2 = unary_op_to_string(op2)
          s3 = unary_op_to_string(op3)

          check("#{s1}#{s2}#{s3}a")
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Associativity Tests
  # =============================================================================
  # Verify that chained same-operator expressions parse correctly

  describe "associativity verification" do
    test "right-associative operators chain correctly" do
      failures =
        for op <- @right_assoc_ops do
          s_op = op_to_string(op)

          [
            check("a #{s_op} b #{s_op} c"),
            check("a #{s_op} b #{s_op} c #{s_op} d")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "left-associative operators chain correctly" do
      failures =
        for op <- @left_assoc_ops, op not in [:"not in"] do
          s_op = op_to_string(op)

          [
            check("a #{s_op} b #{s_op} c"),
            check("a #{s_op} b #{s_op} c #{s_op} d")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # String Interpolation with Operators
  # =============================================================================

  describe "string interpolation with operators" do
    test "operators inside interpolation" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check(~s'"foo\#{a #{s_op} b}bar"'),
            check(~s'"foo\#{a}bar" #{s_op} "baz"'),
            check(~s'a #{s_op} "foo\#{b}bar"')
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "heredocs with operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check(~s'"""\nfoo \#{a #{s_op} b}\n"""'),
            check(~s'a #{s_op} """\nfoo\n"""')
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "charlists with interpolation and operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check(~s|'foo\#{a #{s_op} b}bar'|),
            check(~s|'foo' #{s_op} a|)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Data Structures with Operators
  # =============================================================================

  describe "operators in data structures" do
    test "operators in lists" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("[a #{s_op} b]"),
            check("[a #{s_op} b, c #{s_op} d]"),
            check("[a | b #{s_op} c]"),
            check("[a #{s_op} b | c]")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "operators in tuples" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("{a #{s_op} b}"),
            check("{a #{s_op} b, c #{s_op} d}"),
            check("{a, b #{s_op} c}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "operators in maps" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("%{a: b #{s_op} c}"),
            check("%{a #{s_op} b => c}"),
            check("%{a => b #{s_op} c}"),
            check("%{a #{s_op} b => c #{s_op} d}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "operators in structs" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("%Foo{a: b #{s_op} c}"),
            check("%Foo{s | a: b #{s_op} c}"),
            check("%Foo{a #{s_op} b | c: d}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "operators in keyword lists" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("[a: b #{s_op} c]"),
            check("[a: b #{s_op} c, d: e #{s_op} f]"),
            check("foo(a: b #{s_op} c)")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Function Capture with Operators
  # =============================================================================

  describe "function capture with operators" do
    test "capture with operator expressions" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("&(&1 #{s_op} a)"),
            check("&(&1 #{s_op} &2)"),
            check("&(a #{s_op} &1)"),
            check("f = &(&1 #{s_op} 1)")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "capture followed by binary operator" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("&foo/1 #{s_op} a"),
            check("a #{s_op} &foo/1"),
            check("&Foo.bar/2 #{s_op} a")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Module Attribute with Operators
  # =============================================================================

  describe "module attribute with operators" do
    test "@attr with binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("@foo #{s_op} a"),
            check("a #{s_op} @foo"),
            check("@foo a #{s_op} b")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "@attr with do/end blocks" do
      failures =
        for block <- @do_blocks do
          [
            check("@foo #{block}"),
            check("@foo #{block}..1"),
            check("@foo #{block}..1//2")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Bitstring with Type Operators
  # =============================================================================

  describe "bitstring with type operators" do
    test "basic bitstring type specs" do
      failures =
        [
          check("<<a::8>>"),
          check("<<a::size(8)>>"),
          check("<<a::binary>>"),
          check("<<a::8, b::binary>>"),
          check("<<a::size(n)>>"),
          check("<<a::size(n)-binary>>"),
          check("<<a::size(n)-unsigned-big>>")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "bitstring with operators inside" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("<<a #{s_op} b>>"),
            check("<<a::size(n #{s_op} m)>>"),
            check("<<a #{s_op} b, c::8>>")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "bitstring with operators outside" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("<<a::8>> #{s_op} b"),
            check("a #{s_op} <<b::8>>")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Guard Expressions (when)
  # =============================================================================

  describe "guard expressions" do
    test "when with binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("def foo(a) when a #{s_op} b, do: a"),
            check("fn a when a #{s_op} b -> a end"),
            check("case a do\n  x when x #{s_op} y -> x\nend")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "when with multiple guards" do
      failures =
        for op1 <- [:and, :or, :&&, :||], op2 <- [:and, :or, :&&, :||] do
          s1 = op_to_string(op1)
          s2 = op_to_string(op2)

          [
            check("def foo(a) when a > 0 #{s1} a < 10 #{s2} a != 5, do: a"),
            check("fn a when a > 0 #{s1} a < 10 -> a end")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Comprehension Generators
  # =============================================================================

  describe "comprehension generators" do
    test "<- with binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("for x <- a #{s_op} b, do: x"),
            check("for x <- xs, x #{s_op} y, do: x"),
            check("with {:ok, x} <- a #{s_op} b, do: x")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Default Arguments
  # =============================================================================

  describe "default arguments" do
    test "\\\\ with binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("def foo(a \\\\ b #{s_op} c), do: a"),
            check("def foo(a \\\\ 1 #{s_op} 2 #{s_op} 3), do: a")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Sigils with Operators
  # =============================================================================

  describe "sigils with operators" do
    test "sigils followed by binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("~r/foo/ #{s_op} a"),
            check("a #{s_op} ~r/bar/"),
            check("~s(foo) #{s_op} ~s(bar)"),
            check("~w(a b c) #{s_op} list")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "sigils with interpolation and operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check(~s|~s"foo\#{a #{s_op} b}bar"|),
            check(~s|~s"foo" #{s_op} a|)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Dot Operator Combinations
  # =============================================================================

  describe "dot operator combinations" do
    test "dot with binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("a.b #{s_op} c"),
            check("a #{s_op} b.c"),
            check("a.b #{s_op} c.d"),
            check("Foo.bar #{s_op} Baz.qux"),
            check("Foo.bar() #{s_op} a"),
            check("a #{s_op} Foo.bar()"),
            check("foo.bar(a #{s_op} b)")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "quoted function names with operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check(~s|Foo."bar"() #{s_op} a|),
            check(~s|a #{s_op} Foo."bar"()|),
            check(~s|Foo."bar"(a #{s_op} b)|)
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Access Syntax with Operators
  # =============================================================================

  describe "access syntax with operators" do
    test "access with binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("a[b] #{s_op} c"),
            check("a #{s_op} b[c]"),
            check("a[b #{s_op} c]"),
            check("a[b] #{s_op} c[d]")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Parenthesized Expressions with Operators
  # =============================================================================

  describe "parenthesized expressions with operators" do
    test "parentheses override precedence" do
      failures =
        for op1 <- @precedence_representatives, op2 <- @precedence_representatives do
          s1 = op_to_string(op1)
          s2 = op_to_string(op2)

          [
            check("(a #{s1} b) #{s2} c"),
            check("a #{s1} (b #{s2} c)"),
            check("(a #{s1} b) #{s2} (c #{s1} d)")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Range with Two Ternary Operators
  # =============================================================================

  describe "two ternary operators (range..//step and map update)" do
    test "range inside map" do
      failures =
        [
          check("%{a => 1..10//2}"),
          check("%{1..10//2 => a}"),
          check("%{m | a => 1..10//2}"),
          check("%{m | a: 1..10//2}")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "map inside range" do
      failures =
        [
          check("%{a: 1}..%{b: 2}"),
          check("%{a: 1}..%{b: 2}//1"),
          check("1..%{a: 2}//3")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "complex range and map combinations" do
      failures =
        for op <- @precedence_representatives do
          s_op = op_to_string(op)

          [
            check("1..10//2 #{s_op} %{a: b}"),
            check("%{a: b} #{s_op} 1..10//2"),
            check("%{1..2 => 3..4//5}"),
            check("a..b//c #{s_op} %{d | e => f}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "binary operators between two ternaries" do
      failures =
        for op <- @binary_ops do
          s_op = op_to_string(op)

          [
            check("a..b//c #{s_op} d..e//f"),
            check("a..b//c #{s_op} %{m | k => v}"),
            check("%{m | k => v} #{s_op} a..b//c"),
            check("%{m | k => v} #{s_op} %{p | q => r}")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Special Cases from Repro Tests
  # =============================================================================

  describe "special cases from repro tests" do
    test "unary with right-associative operators" do
      failures =
        for unary <- @simple_unary_ops, op <- [:++, :--, :<>, :..] do
          s_unary = unary_op_to_string(unary)
          s_op = op_to_string(op)

          [
            check("#{s_unary}a #{s_op} b"),
            check("#{s_unary}a #{s_op} b #{s_op} c"),
            check("a #{s_op} #{s_unary}b"),
            check("a #{s_op} #{s_unary}b #{s_op} c")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "unary with pipe operators" do
      failures =
        for unary <- @simple_unary_ops do
          s_unary = unary_op_to_string(unary)

          [
            check("#{s_unary}a |> b"),
            check("#{s_unary}a |> b |> c"),
            check("a |> #{s_unary}b"),
            check("#{s_unary}a |> b < c"),
            check("#{s_unary}a |> b == c")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "unary with comparison and logical operators" do
      failures =
        for unary <- @simple_unary_ops,
            comp <- [:<, :>, :<=, :>=, :==, :!=, :===, :!==],
            logical <- [:&&, :||, :and, :or] do
          s_unary = unary_op_to_string(unary)
          s_comp = op_to_string(comp)
          s_logical = op_to_string(logical)

          [
            check("#{s_unary}a #{s_comp} b"),
            check("#{s_unary}a #{s_comp} b #{s_logical} c"),
            check("a #{s_comp} #{s_unary}b #{s_logical} c")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "range with step and other operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("a #{s_op} b..c//d"),
            check("a..b//c #{s_op} d"),
            check("a #{s_op} b..c #{s_op} d//e"),
            check("a..b #{s_op} c//d")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "capture with do/end blocks" do
      failures =
        [
          check("&case a do\n  _ -> b\nend"),
          check("f = &case a do\n  _ -> b\nend"),
          check("&fn -> a end"),
          check("&quote do\n  a\nend")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "@ with call and do/end blocks" do
      failures =
        [
          check("@foo Foo.bar(case a do\n  _ -> b\nend)"),
          check("@foo Foo.bar(try do\n  a\nend)"),
          check("@foo Foo.bar(quote do\n  a\nend)"),
          check("@foo case a do\n  _ -> b\nend..c"),
          check("@foo try do\n  a\nend..b//c")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Pipe into Data Structures
  # =============================================================================

  describe "pipe into data structures" do
    test "|> with data structure destinations" do
      failures =
        [
          check("a |> {b, c}"),
          check("a |> [b, c]"),
          check("a |> %{b: c}"),
          check("a |> {b..c}"),
          check("a |> {b, c..d//e}"),
          check("Foo.bar() |> {a..b, c}")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Complex Nested Expressions
  # =============================================================================

  describe "complex nested expressions" do
    test "multiple levels of nesting" do
      failures =
        [
          check("[[a + b, c * d], e | f]"),
          check("{[a: b + c], {d, e * f}}"),
          check("%{a: [b | c], d: {e, f + g}}"),
          check("case a + b do\n  x when x > 0 -> [y | z]\nend"),
          check("for x <- a..b//c, y <- d..e, do: {x + y, x * y}")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "deeply nested operators" do
      failures =
        [
          check("(a + (b * (c ** d)))"),
          check("((a + b) * (c + d))"),
          check("a || (b && (c || d))"),
          check("a = b = c = d + e"),
          check("[a | [b | [c | d]]]")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Capture Operator followed by Binary Operators
  # =============================================================================

  describe "capture followed by binary operators" do
    test "&(expr) followed by all binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("&(a + 1) #{s_op} b"),
            check("&foo/1 #{s_op} b"),
            check("&Foo.bar/2 #{s_op} b"),
            check("b #{s_op} &(a + 1)"),
            check("b #{s_op} &foo/1")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "&(expr) followed by range operators" do
      failures =
        [
          check("&(a + 1)..b"),
          check("&(a + 1)..b//c"),
          check("a..&(b + 1)"),
          check("a..&(b + 1)//c"),
          check("&foo/1..b..c"),
          check("&foo/1..b//c")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "&(expr) with keyword list followed by binary operators" do
      failures =
        for op <- [:++, :--, :|>, :in, :<>, :==, :&&, :||] do
          s_op = op_to_string(op)

          [
            check("&([a: 1] + 1) #{s_op} b"),
            check("&(['one': :ok] + 1) #{s_op} b")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "&(expr) with struct/map followed by binary operators" do
      failures =
        for op <- [:++, :--, :|>, :in, :<>, :==, :&&, :||] do
          s_op = op_to_string(op)

          [
            check("&(%{a: 1} + 1) #{s_op} b"),
            check("&(%Foo{a: 1} + 1) #{s_op} b"),
            check("&({a, b} + 1) #{s_op} c")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Nested Captures
  # =============================================================================

  describe "nested captures" do
    test "double nested capture" do
      failures =
        [
          check("&(&(a + 1) + 1)"),
          check("&(&(0 + 1) + 1)"),
          check("&(&1 + &(&2 + 1))"),
          check("&(&(&1 + 1) + 1)")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "nested capture with struct" do
      failures =
        [
          check("&(&(%Foo{a: b} + 1) + 1)"),
          check("&(&(%{a: 1} + 1) + 1)"),
          check("&(&([a: 1] + 1) + 1)")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "nested capture inside interpolation" do
      failures =
        [
          check(~s'"foo\#{&(&(0 + 1) + 1)}bar"'),
          check(~s'~s"""\\nfoo \#{&(&(0 + 1) + 1)} bar\\n"""')
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Capture with Char Literals
  # =============================================================================

  describe "capture with char literals" do
    test "&(?x + n) patterns" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("&(?a + 1)"),
            check("&(?a + 1) #{s_op} b"),
            check("b #{s_op} &(?a + 1)"),
            check("&(?a #{s_op} ?b)")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "capture with char literal in map" do
      failures =
        [
          check("%{a => &(?h + 1)}"),
          check("%{&(?h + 1) => a}"),
          check("%{a => &(?h + 1) !== b}"),
          check("foo(%{a => &(?h + 1) !== b})")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # fn Expressions followed by Binary Operators
  # =============================================================================

  describe "fn expressions followed by binary operators" do
    test "fn -> expr end followed by all binary operators" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("fn -> a end #{s_op} b"),
            check("fn x -> x end #{s_op} b"),
            check("fn x, y -> x + y end #{s_op} b"),
            check("a #{s_op} fn -> b end")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "fn followed by &&& and then capture with pipe" do
      failures =
        [
          check("fn a -> a end &&& b"),
          check("fn a -> a end &&& &(a + 1)"),
          check("fn a -> a end &&& &(a + 1) |> b"),
          check("fn a -> a end &&& &(a + 1) |> b |> c")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Unary + do/end Blocks followed by Range with Step
  # =============================================================================

  describe "unary with do/end blocks followed by range" do
    test "unary op do/end block followed by range" do
      failures =
        for unary <- @simple_unary_ops do
          s_unary = unary_op_to_string(unary)

          [
            check("#{s_unary}case a do\n  _ -> b\nend..c"),
            check("#{s_unary}case a do\n  _ -> b\nend..c//d"),
            check("#{s_unary}try do\n  a\nend..b"),
            check("#{s_unary}try do\n  a\nend..b//c"),
            check("#{s_unary}with a <- b, do: c end..d//e"),
            check("#{s_unary}quote do\n  a\nend..b//c")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "unary + case followed by pipe and power operators" do
      failures =
        [
          check("+case a do\n  _ -> b\nend |> c"),
          check("+case a do\n  _ -> b\nend |> c ** d"),
          check("+case a do\n  _ -> b\nend |> c ** d >>> e"),
          check("-case a do\n  _ -> b\nend |> c ** d")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Char Literals Piped to Data Structures
  # =============================================================================

  describe "char literals with pipe to data structures" do
    test "char literal piped to struct/map" do
      failures =
        [
          check("?a |> %Foo{}"),
          check("?a |> %Foo{b: c}"),
          check("?a |> %{b: c}"),
          check("'M' |> %Foo{}"),
          check("'M' |> %Foo{a: b}")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "charlist piped to struct in function call" do
      failures =
        [
          check("foo('M' |> %Baz{})"),
          check("Foo.bar('M' |> %Baz{a: b})"),
          check("Foo.bar('M' |> %Baz{a: b, c: d})")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Range inside Bitstring
  # =============================================================================

  describe "range inside bitstring" do
    test "char literal range in bitstring" do
      failures =
        [
          check("<<?a..b>>"),
          check("<<?a..foo>>"),
          check("<<a..?b>>"),
          check("<<?a..:ok>>"),
          check("<<a..b, c::8>>")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "complex expressions in bitstring" do
      failures =
        [
          check("<<a + b>>"),
          check("<<a + b, c::binary>>"),
          check("<<a |> b>>"),
          check("<<a..b//c>>")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Unary Operators inside Bitstring
  # =============================================================================

  describe "unary operators inside bitstring" do
    test "unary with do/end block inside bitstring" do
      failures =
        for unary <- @simple_unary_ops do
          s_unary = unary_op_to_string(unary)

          [
            check("<<#{s_unary}a>>"),
            check("<<#{s_unary}a, b::8>>"),
            check("<<a, #{s_unary}b>>")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "unary + do/end blocks inside bitstring" do
      failures =
        [
          check("<<+quote do\n  a\nend>>"),
          check("<<-case a do\n  _ -> b\nend>>"),
          check("<<+quote do\n  a\nend, b::8>>"),
          check("{:ok, <<+quote do\n  a\nend>>}")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Complex Interpolation with Capture and Pipe
  # =============================================================================

  describe "complex interpolation with capture and pipe" do
    test "capture with pipe inside interpolation" do
      failures =
        [
          check(~s'"foo\#{&(a |> b)}bar"'),
          check(~s'"foo\#{&(a |> b + 1)}bar"'),
          check(~s'"foo\#{&(a |> b + 1) |> c}bar"'),
          check("'foo\#{&(a |> b + 1)}bar'"),
          check("'foo\#{&(0 |> Foo + 1) |> %{a: b}}bar'")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "capture followed by in and fn with pipe" do
      failures =
        [
          check("&(a + 1) in b"),
          check("&(a + 1) in fn -> b end"),
          check("&(a + 1) in fn -> b end |> c"),
          check("&(a + 1) in fn -> b end |> quote do\n  c\nend")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Heredoc Charlists with Complex Interpolation
  # =============================================================================

  describe "heredoc charlists with operators" do
    test "capture with heredoc charlist followed by range" do
      failures =
        [
          check("&('''\nfoo\n''' + 1)"),
          check("&('''\nfoo\n''' + 1)..b"),
          check("&('''\nfoo\n''' + 1)..b//c"),
          check("a..&('''\nfoo\n''' + 1)")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "heredoc charlist with interpolation containing operators" do
      failures =
        [
          check("'''\nfoo \#{a + b}\n'''"),
          check("'''\nfoo \#{a |> b}\n'''"),
          check("'''\nfoo \#{&(a + 1)}\n'''"),
          check("'''\nfoo \#{%{a: b}}\n'''")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Combination of Multiple Complex Patterns
  # =============================================================================

  describe "combination of multiple complex patterns" do
    test "fn in capture with operators" do
      failures =
        [
          check("&({a, b} + 1) in fn -> c end"),
          check("fn a -> &(b + 1) end"),
          check("fn a -> &(b + 1) in c end")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "atoms as quoted keys with operators" do
      failures =
        [
          check("%{:'ok' => a + b}"),
          check("%{:'ok' + a => b}"),
          check("<<:'ok' + a>>"),
          check("[:'ok': a + b]")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "with expression inside capture" do
      failures =
        [
          check("&(with a <- b, do: c)"),
          check("&(with a <- b, do: c + d)"),
          check("&(with a <- b, do: c) |> d")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Range with Keyword List as Operand
  # =============================================================================

  describe "range with keyword list operand" do
    test "range with keyword list as right operand" do
      failures =
        [
          check("a..['key': b]"),
          check("a..b..['key': c]"),
          check("a..['key': b]//c"),
          check("a..['do': b, 'else': c]"),
          check("\"\" <> \"foo\"..['do': bar]")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "range with keyword list in case expression" do
      failures =
        [
          check("case a..['key': b] do\n  _ -> c\nend"),
          check("case \"\" <> \"foo\"..['do': bar] do\n  _ -> c\nend"),
          check("case a..b..['key': c]//d do\n  _ -> e\nend")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "range with heredoc in keyword list" do
      failures =
        [
          check("a..['key': \"\"\"\nfoo\n\"\"\"]"),
          check("case a..['do': \"\"\"\nfoo\n\"\"\"] do\n  _ -> b\nend")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Pipe Inside Function Call Arguments
  # =============================================================================

  describe "pipe inside function call arguments" do
    test "charlist piped to struct/map in function call" do
      failures =
        [
          check("foo('M' |> a)"),
          check("foo('M' |> %{a: b})"),
          check("foo('M' |> %Foo{a: b})"),
          check("foo('abc' |> %{\"ok\": :err})"),
          check("Foo.bar('M' |> %{a: b})")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "pipe with heredoc in struct inside function call" do
      failures =
        [
          check("foo(a |> %{b: \"\"\"\nfoo\n\"\"\"})"),
          check("foo('M' |> %Baz{\"ok\": \"\"\"\nfoo\n\"\"\"})"),
          check("Foo.bar('M' |> %{a: \"\"\"\nfoo \#{b} bar\n\"\"\"})")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "multiple pipes in function call arguments" do
      failures =
        [
          check("foo(a |> b |> c)"),
          check("foo(a |> b, c |> d)"),
          check("foo('M' |> a |> b)"),
          check("Foo.bar(a |> %{b: c}, d |> e)")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "pipe with operators in function call" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          [
            check("foo(a |> b #{s_op} c)"),
            check("foo(a #{s_op} b |> c)")
          ]
        end
        |> List.flatten()
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Struct/Map with Quoted String Keys and Heredoc Values
  # =============================================================================

  describe "struct/map with quoted string keys" do
    test "struct with quoted string key and heredoc value" do
      failures =
        [
          check("%Foo{\"ok\": a}"),
          check("%Foo{\"ok\": \"\"\"\nfoo\n\"\"\"}"),
          check("%{\"ok\": \"\"\"\nfoo \#{a} bar\n\"\"\"}"),
          check("foo(%Baz{\"ok\": \"\"\"\nfoo\n\"\"\"})")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "pipe into struct with heredoc" do
      failures =
        [
          check("a |> %Foo{b: \"\"\"\nfoo\n\"\"\"}"),
          check("a |> %{\"ok\": \"\"\"\nfoo\n\"\"\"}"),
          check("'M' |> %Baz{\"ok\": \"\"\"\nfoo \#{a} bar\n\"\"\"})")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end

  # =============================================================================
  # Case Expression with Complex First Argument
  # =============================================================================

  describe "case with complex first argument" do
    test "case with binary operator expression" do
      failures =
        for op <- @simple_binary_ops do
          s_op = op_to_string(op)

          check("case a #{s_op} b do\n  _ -> c\nend")
        end
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "case with string concatenation and range" do
      failures =
        [
          check("case \"\" <> \"foo\" do\n  _ -> a\nend"),
          check("case a..b do\n  _ -> c\nend"),
          check("case \"\" <> \"foo\"..a do\n  _ -> b\nend"),
          check("case a..b//c do\n  _ -> d\nend")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end

    test "case with range and keyword list" do
      failures =
        [
          check("case a..[b: c] do\n  _ -> d\nend"),
          check("case \"\" <> \"foo\"..[do: a] do\n  _ -> b\nend"),
          check("case a..['key': \"\"\"\nfoo\n\"\"\"] do\n  _ -> b\nend")
        ]
        |> Enum.reject(&is_nil/1)

      assert failures == [],
             "Failed combinations: #{inspect(failures, pretty: true, limit: :infinity)}"
    end
  end
end
