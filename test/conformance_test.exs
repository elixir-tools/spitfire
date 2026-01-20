defmodule Spitfire.ConformanceTest do
  @moduledoc """
  Conformance tests comparing Spitfire output against `Code.string_to_quoted/2`.

  These tests verify that the parser produces identical AST to the reference Elixir parser
  for valid inputs. The test structure mirrors `elixir_parser.yrl` nonterminals.
  """
  use ExUnit.Case, async: true

  # =============================================================================
  # TERMINALS - Basic building blocks that are valid as standalone expressions
  # =============================================================================

  describe "terminals - literals" do
    test "true" do
      assert_conforms("true")
    end

    test "false" do
      assert_conforms("false")
    end

    test "nil" do
      assert_conforms("nil")
    end
  end

  describe "terminals - integers (int)" do
    test "decimal integer" do
      assert_conforms("1")
      assert_conforms("42")
      assert_conforms("1_000_000")
    end

    test "negative integer" do
      assert_conforms("-1")
      assert_conforms("-42")
    end

    test "binary integer" do
      assert_conforms("0b101")
      assert_conforms("0b1010_1010")
    end

    test "octal integer" do
      assert_conforms("0o777")
      assert_conforms("0o12_34")
    end

    test "hexadecimal integer" do
      assert_conforms("0xDEAD")
      assert_conforms("0xBEEF")
      assert_conforms("0xdead_beef")
      assert_conforms("0xFF")
    end
  end

  describe "terminals - floats (flt)" do
    test "simple float" do
      assert_conforms("1.0")
      assert_conforms("3.14")
      assert_conforms("0.5")
    end

    test "float with underscores" do
      assert_conforms("1_000.000_001")
    end

    test "float with exponent" do
      assert_conforms("1.0e10")
      assert_conforms("1.0E10")
      assert_conforms("1.0e-10")
      assert_conforms("1.0e+10")
    end

    test "negative float" do
      assert_conforms("-1.0")
      assert_conforms("-3.14e-10")
    end
  end

  describe "terminals - character literals (char)" do
    test "simple character" do
      assert_conforms("?a")
      assert_conforms("?Z")
      assert_conforms("?0")
    end

    test "escaped character" do
      assert_conforms("?\\n")
      assert_conforms("?\\t")
      assert_conforms("?\\r")
      assert_conforms("?\\s")
      assert_conforms("?\\\\")
    end

    test "special characters" do
      assert_conforms("? ")
      assert_conforms("?!")
      assert_conforms("??")
    end
  end

  describe "terminals - atoms (atom)" do
    test "simple atom" do
      assert_conforms(":foo")
      assert_conforms(":bar")
      assert_conforms(":baz")
    end

    test "atom with underscore" do
      assert_conforms(":foo_bar")
      assert_conforms(":_private")
    end

    test "atom with numbers" do
      assert_conforms(":foo123")
      assert_conforms(":v1")
    end

    test "atom ending with ? or !" do
      assert_conforms(":foo?")
      assert_conforms(":bar!")
    end

    test "atom with @" do
      assert_conforms(":foo@bar")
    end

    test "operator atoms" do
      assert_conforms(":<<>>")
      assert_conforms(":%{}")
      assert_conforms(":%")
      assert_conforms(":{}")
      assert_conforms(":..//")
    end

    test "operators as atoms" do
      # 3 char

      # unary_op
      assert_conforms(":~~~")
      # comp_op
      assert_conforms(":===")
      assert_conforms(":!==")
      # and_op
      assert_conforms(":&&&")
      # or_op
      assert_conforms(":|||")
      # arrow_op
      assert_conforms(":<<<")
      assert_conforms(":>>>")
      assert_conforms(":~>>")
      assert_conforms(":<<~")
      assert_conforms(":<~>")
      assert_conforms(":<|>")
      # xor_op
      assert_conforms(":^^^")
      # concat_op
      assert_conforms(":+++")
      assert_conforms(":---")
      # ellipsis_op
      assert_conforms(":...")

      # 2 char

      # type_op
      assert_conforms(":::")
      # comp_op
      assert_conforms(":==")
      assert_conforms(":!=")
      assert_conforms(":=~")
      # rel_op
      assert_conforms(":>=")
      assert_conforms(":<=")
      # and_op
      assert_conforms(":&&")
      # or_op
      assert_conforms(":||")
      # arrow_op
      assert_conforms(":|>")
      assert_conforms(":~>")
      assert_conforms(":<~")
      # in_match_op
      assert_conforms(":<-")
      assert_conforms(":\\\\")
      # concat_op
      assert_conforms(":++")
      assert_conforms(":--")
      # power_op
      assert_conforms(":**")
      # stab_op
      assert_conforms(":->")
      # range_op
      assert_conforms(":..")

      # 1 char

      # at_op
      assert_conforms(":@")
      # unary_op
      assert_conforms(":!")
      assert_conforms(":^")
      # capture_op
      assert_conforms(":&")
      # dual_op
      assert_conforms(":+")
      assert_conforms(":-")
      # mult_op
      assert_conforms(":*")
      assert_conforms(":/")
      # rel_op
      assert_conforms(":<")
      assert_conforms(":>")
      # match_op
      assert_conforms(":=")
      # pipe_op
      assert_conforms(":|")
      # dot
      assert_conforms(":.")
    end

    test "special atoms" do
      assert_conforms(":true")
      assert_conforms(":false")
      assert_conforms(":nil")
    end
  end

  describe "terminals - quoted atoms (atom_quoted)" do
    test "quoted atom with spaces" do
      assert_conforms(~s(:"foo bar"))
    end

    test "quoted atom with special characters" do
      assert_conforms(~s(:"foo-bar"))
      assert_conforms(~s(:"foo.bar"))
    end

    test "quoted atom with escapes" do
      assert_conforms(~s(:"foo\\nbar"))
      assert_conforms(~s(:"foo\\tbar"))
    end

    test "empty quoted atom" do
      assert_conforms(~s(:""))
    end

    test "quoted atom with unicode" do
      assert_conforms(~s(:"hello\\u0041"))
    end

    test "single-quoted atom" do
      assert_conforms(~s(:'foo bar'))
      assert_conforms(~s(:'hello world'))
    end

    test "quoted atom interpolated" do
      assert_conforms(~s(:"hello\#{abc}foo"))
      assert_conforms(":\"foo\#{}\"")
      assert_conforms(":\"foo\#{''}\"")
    end
  end

  describe "terminals - strings" do
    test "empty bin string" do
      assert_conforms(~s(""))
    end

    test "empty list string" do
      assert_conforms(~s(''))
    end

    test "bin string" do
      assert_conforms(~s("foo"))
    end

    test "list string" do
      assert_conforms(~s('foo'))
    end

    test "bin string with nl" do
      assert_conforms(~s("fo\\no"))
    end

    test "list string with nl" do
      assert_conforms(~s('fo\\no'))
    end

    test "bin string interpolated" do
      assert_conforms(~s("fo\#{bar}o"))
    end

    test "list string interpolated" do
      assert_conforms(~s('fo\#{bar}o'))
    end
  end

  describe "terminals - heredocs" do
    test "empty bin heredocs" do
      assert_conforms(~s("""\n\\\n"""))
    end

    test "empty list heredocs" do
      assert_conforms(~s('''\n\\\n'''))
    end

    test "bin heredocs" do
      assert_conforms(~s("""\nfoo\n"""))
    end

    test "list heredocs" do
      assert_conforms(~s('''\nfoo\n'''))
    end

    test "bin heredocs with indent" do
      assert_conforms(~s("""\n  foo\n  """))
    end

    test "list heredocs with indent" do
      assert_conforms(~s('''\n  foo\n  '''))
    end

    test "bin heredocs with nl" do
      assert_conforms(~s("""\nfo\\no\n"""))
    end

    test "list heredocs with nl" do
      assert_conforms(~s('''\nfo\\no\n'''))
    end

    test "bin heredocs interpolated" do
      assert_conforms(~s("""\nfo\#{bar}o\n"""))
    end

    test "list heredocs interpolated" do
      assert_conforms(~s('''\nfo\#{bar}o\n'''))
    end
  end

  describe "terminals - sigils" do
    test "empty bin sigils" do
      assert_conforms(~s(~x""))
    end

    test "bin sigils" do
      assert_conforms(~s(~x"foo"))
    end

    test "bin sigils with modifiers" do
      assert_conforms(~s(~x"foo"abc))
    end

    test "bin sigils with nl" do
      assert_conforms(~s(~x"fo\\no"))
    end

    test "bin sigils interpolated" do
      assert_conforms(~s(~x"fo\#{bar}o"))
    end

    test "empty bin heredoc sigils" do
      assert_conforms(~s(~x"""\n\\\n"""))
    end

    test "bin heredoc sigils" do
      assert_conforms(~s(~x"""\nfoo\n"""))
    end

    test "bin heredoc sigils with modifiers" do
      assert_conforms(~s(~x"""\nfoo\n"""abc))
    end

    test "bin heredoc sigils with indent" do
      assert_conforms(~s(~x"""\n  foo\n  """))
    end

    test "bin heredoc sigils with nl" do
      assert_conforms(~s(~x"""\nfo\\no\n"""))
    end

    test "bin heredoc sigils interpolated" do
      assert_conforms(~s(~x"""\nfo\#{bar}o\n"""))
    end
  end

  test "nested grammar in interpolation" do
    assert_conforms("'asd\#{}ppp'")
    assert_conforms("'asd\#{;}ppp'")
    assert_conforms("'asd\#{\n}ppp'")
    assert_conforms("'asd\#{\n;}ppp'")
    assert_conforms("'asd\#{a;}ppp'")
    assert_conforms("'asd\#{;a}ppp'")
    assert_conforms("'asd\#{a;b}ppp'")
  end

  describe "terminals - identifiers (identifier)" do
    test "simple identifier" do
      assert_conforms("foo")
      assert_conforms("bar")
      assert_conforms("baz")
    end

    test "identifier with underscore" do
      assert_conforms("foo_bar")
      assert_conforms("_private")
      assert_conforms("__special__")
    end

    test "identifier with numbers" do
      assert_conforms("foo123")
      assert_conforms("v1")
      assert_conforms("a1b2c3")
    end

    test "identifier ending with ? or !" do
      assert_conforms("foo?")
      assert_conforms("bar!")
    end

    test "underscore identifier" do
      assert_conforms("_")
      assert_conforms("_foo")
    end

    test "reserved words used as identifiers" do
      # These are identifiers that happen to look like they could be keywords
      # but are valid as standalone identifiers
      assert_conforms("do_something")
      assert_conforms("end_time")
      assert_conforms("after_hook")
    end
  end

  describe "terminals - aliases (alias)" do
    test "simple alias" do
      assert_conforms("Foo")
      assert_conforms("Bar")
      assert_conforms("MyModule")
    end

    test "nested alias" do
      assert_conforms("Foo.Bar")
      assert_conforms("Foo.Bar.Baz")
      assert_conforms("My.Deeply.Nested.Module")
    end

    test "alias with numbers" do
      assert_conforms("Foo123")
      assert_conforms("V1")
      assert_conforms("MyModule2")
    end

    test "Elixir prefix" do
      assert_conforms("Elixir.Foo")
      assert_conforms("Elixir.Foo.Bar")
    end
  end

  describe "terminals - standalone operators" do
    test "concat" do
      assert_conforms("..")
    end

    test "ellipsis" do
      assert_conforms("...")
    end
  end

  # =============================================================================
  # MAIN FLOW OF EXPRESSIONS
  # From elixir_parser.yrl lines 99-114
  # =============================================================================

  describe "grammar - empty input" do
    test "empty string" do
      # grammar -> '$empty' : {'__block__', [], []}.
      assert {:ok, result} = Spitfire.parse("")
      assert {:__block__, _, []} = result
    end
  end

  describe "grammar - eoe (end of expression)" do
    test "single semicolon" do
      # grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
      assert_conforms(";")
    end

    test "single newline" do
      assert_conforms("\n")
    end

    test "newline followed by semicolon" do
      assert_conforms("\n;")
    end
  end

  describe "grammar - expr_list" do
    test "single expression" do
      # grammar -> expr_list : build_block(reverse('$1')).
      assert_conforms("1")
      assert_conforms("foo")
      assert_conforms(":atom")
    end

    test "multiple expressions separated by semicolon" do
      # expr_list -> expr_list eoe expr : ['$3' | annotate_eoe('$2', '$1')].
      assert_conforms("1;2")
      assert_conforms("1;2;3")
    end

    test "multiple expressions separated by newline" do
      assert_conforms("1\n2")
      assert_conforms("1\n2\n3")
    end

    test "multiple expressions with mixed separators" do
      assert_conforms("1\n;2")
      assert_conforms("1\n;2\n;3")
    end
  end

  describe "grammar - eoe expr_list (leading eoe)" do
    test "semicolon before expression" do
      # grammar -> eoe expr_list : build_block(reverse('$2')).
      assert_conforms(";1")
      assert_conforms(";1;2")
    end

    test "newline before expression" do
      assert_conforms("\n1")
      assert_conforms("\n1\n2")
    end

    test "newline and semicolon before expression" do
      assert_conforms("\n;1")
      assert_conforms("\n;1\n;2")
    end
  end

  describe "grammar - expr_list eoe (trailing eoe)" do
    test "expression followed by semicolon" do
      # grammar -> expr_list eoe : build_block(reverse(annotate_eoe('$2', '$1'))).
      assert_conforms("1;")
      assert_conforms("1;2;")
    end

    test "expression followed by newline" do
      assert_conforms("1\n")
      assert_conforms("1\n2\n")
    end

    test "expression followed by mixed eoe" do
      assert_conforms("1\n;")
      assert_conforms("2\n;1\n;")
    end
  end

  describe "grammar - eoe expr_list eoe (both leading and trailing)" do
    test "semicolons on both sides" do
      # grammar -> eoe expr_list eoe : build_block(reverse(annotate_eoe('$3', '$2'))).
      assert_conforms(";1;")
      assert_conforms(";1;2;")
    end

    test "newlines on both sides" do
      assert_conforms("\n1\n")
      assert_conforms("\n1\n2\n")
    end

    test "mixed eoe on both sides" do
      assert_conforms("\n;1\n;")
      assert_conforms("\n;1\n;2\n;")
    end
  end

  describe "expr dispatch" do
    test "expr -> matched_expr" do
      # expr -> matched_expr : '$1'.
      # Simple binary operation is matched
      assert_conforms("1 + 2")
      assert_conforms("a * b")
      assert_conforms(";a * b")
      assert_conforms("a * b\n")
    end

    test "expr -> no_parens_expr" do
      # expr -> no_parens_expr : '$1'.
      # Call with multiple args, no parens
      assert_conforms("foo 1, 2")
      assert_conforms("bar :a, :b, :c")
      assert_conforms(";foo 1, 2")
      assert_conforms("foo 1, 2\n")
    end

    test "expr -> unmatched_expr" do
      # expr -> unmatched_expr : '$1'.
      # Block expression (do/end)
      assert_conforms("if true do\n:ok\nend")
      assert_conforms(";if true do\n:ok\nend")
      assert_conforms("if true do\n:ok\nend\n")
    end

    test "combinations" do
      assert_conforms("1 + 2; foo 1, 2\n if true do\n:ok\nend")
      assert_conforms("if true do\n:ok\nend; 1 + 2\n; foo 1, 2")
      assert_conforms("foo 1, 2; if true do\n:ok\nend\n 1 + 2")
    end
  end

  # =============================================================================
  # MATCHED_EXPR
  # From elixir_parser.yrl lines 155-161
  # =============================================================================

  describe "matched_expr - binary operations (matched_op_expr)" do
    test "match_op_eol (=)" do
      # matched_expr -> matched_expr matched_op_expr
      # matched_op_expr -> match_op_eol matched_expr
      assert_conforms("1 = 2")
      assert_conforms("a = b")
      assert_conforms("x = 1 = 2")
      assert_conforms("1 =\n2")
    end

    test "dual_op_eol (+, -)" do
      assert_conforms("1 + 2")
      assert_conforms("1 - 2")
      assert_conforms("1 + 2 + 3")
      assert_conforms("1 +\n2")
    end

    test "mult_op_eol (*, /)" do
      assert_conforms("1 * 2")
      assert_conforms("1 / 2")
      assert_conforms("1 * 2 * 3")
      assert_conforms("1 *\n2")
    end

    test "power_op_eol (**)" do
      assert_conforms("2 ** 3")
      assert_conforms("2 ** 3 ** 4")
      assert_conforms("2 **\n3")
    end

    test "concat_op_eol (<>, ++, --)" do
      assert_conforms("a <> b")
      assert_conforms("a ++ b")
      assert_conforms("a +++ b")
      assert_conforms("a --- b")
      assert_conforms("a -- b -- c")
      assert_conforms("a <>\nb")
    end

    test "range_op_eol (..)" do
      assert_conforms("1 .. 10")
      assert_conforms("a .. b .. c")
      assert_conforms("1 ..\n10")
    end

    test "ternary_op_eol (// - default)" do
      assert_conforms("1..a // b")
      assert_conforms("1..nil // :default")
      assert_conforms("1..a //\nb")
    end

    test "xor_op_eol (^^^)" do
      assert_conforms("1 ^^^ 2")
      assert_conforms("a ^^^ b ^^^ c")
      assert_conforms("1 ^^^\n2")
    end

    test "and_op_eol (&&, &&&, and)" do
      assert_conforms("true && false")
      assert_conforms("a &&& b &&& c")
      assert_conforms("true and false")
      assert_conforms("a &&\nb")
    end

    test "or_op_eol (||, |||, or)" do
      assert_conforms("true || false")
      assert_conforms("a ||| b ||| c")
      assert_conforms("true or false")
      assert_conforms("a ||\nb")
    end

    test "in_op_eol (in)" do
      assert_conforms("a in b")
      assert_conforms("a in b in c")
      assert_conforms("a in\n b")
      assert_conforms("a\\\nin\\\nb")
    end

    test "in_op_eol (not in)" do
      assert_conforms("a not in b")
      assert_conforms("a not in b not in c")
      assert_conforms("a not in\n b")
      assert_conforms("a not\\\nin\n b")
    end

    test "not in rewrite" do
      assert_conforms("not a in b")
      assert_conforms("not\na in\nb")
      assert_conforms("!a in b")
    end

    test "in_match_op_eol (<-, \\\\)" do
      assert_conforms("a <- b")
      assert_conforms("-a <- -b")
      assert_conforms("a \\\\ b")
      assert_conforms("a <- b <- c")
      assert_conforms("a <-\nb")
    end

    test "type_op_eol (::)" do
      assert_conforms("a :: integer")
      assert_conforms("foo :: atom")
      assert_conforms("foo :: atom :: bar")
      assert_conforms("a ::\ninteger")
    end

    test "when_op_eol (when)" do
      assert_conforms("a when is_atom(a)")
      assert_conforms("x when y")
      assert_conforms("x when y when z")
      assert_conforms("a when\nb")
    end

    test "pipe_op_eol (|)" do
      assert_conforms("a | b")
      assert_conforms("a | b | c")
      assert_conforms("a |\nb")
    end

    test "comp_op_eol (==, !=, =~, ===, !==)" do
      assert_conforms("1 == 2")
      assert_conforms("1 != 2")
      assert_conforms("a =~ b")
      assert_conforms("1 === 2")
      assert_conforms("1 === 2 === 3")
      assert_conforms("1 !== 2")
      assert_conforms("1 ==\n2")
    end

    test "rel_op_eol (<, >, <=, >=)" do
      assert_conforms("1 < 2")
      assert_conforms("1 > 2")
      assert_conforms("1 <= 2")
      assert_conforms("1 <= 2 <= 3")
      assert_conforms("1 >= 2")
      assert_conforms("1 <\n2")
    end

    test "arrow_op_eol (|>, ~>, <~, etc.)" do
      assert_conforms("a |> b")
      assert_conforms("a ~> b")
      assert_conforms("a <~ b")
      assert_conforms("a <~> b")
      assert_conforms("a <<~ b")
      assert_conforms("a ~>> b")
      assert_conforms("a ~>> b ~>> c")
      assert_conforms("a <<< b")
      assert_conforms("a >>> b")
      assert_conforms("a <|> b")
      assert_conforms("a |>\nb")
    end

    test "matched_expr matched_op_expr - arrow_op_eol no_parens_one_expr" do
      # no_parens_one_expr -> dot_op_identifier call_args_no_parens_one
      # no_parens_one_expr -> dot_identifier call_args_no_parens_one
      # call_args_no_parens_one -> call_args_no_parens_kw
      # call_args_no_parens_one -> matched_expr
      # dot_identifier -> identifier
      # dot_identifier -> matched_expr dot_op identifier
      # dot_op_identifier -> op_identifier
      # dot_op_identifier -> matched_expr dot_op op_identifier
      # call_args_no_parens_kw -> call_args_no_parens_kw_expr
      # call_args_no_parens_kw -> call_args_no_parens_kw_expr ',' call_args_no_parens_kw
      # call_args_no_parens_kw_expr -> kw_eol matched_expr
      # call_args_no_parens_kw_expr -> kw_eol no_parens_expr

      # identifier matched_expr
      assert_conforms("1 |> a 2")

      # op_identifier matched_expr
      assert_conforms("1 |> a -2")

      # identifier call_args_no_parens_kw
      assert_conforms("1 |> a x: 2")
      assert_conforms("1 |> a 'x': 2")
      assert_conforms("1 |> a 'x': 2, y: 3")
      assert_conforms("1 |> a x: 2, 'y': 3")
      assert_conforms("1 |> a 'x': 2, 'y': 3")
      assert_conforms("1 |> a x: foo bar, baz")
      assert_conforms("1 |> a 'x': foo bar, baz")
      assert_conforms("1 |> a 'x': 2, y: foo bar, baz")
      assert_conforms("1 |> a x: 2, 'y': foo bar, baz")
      assert_conforms("1 |> a 'x': 2, 'y': foo bar, baz")

      # matched_expr dot_op identifier matched_expr
      assert_conforms("1 |> a.b 2")

      # matched_expr dot_op op_identifier matched_expr
      assert_conforms("1 |> a.b -2")

      # matched_expr dot_op identifier call_args_no_parens_kw
      assert_conforms("1 |> a.b x: 2")
      assert_conforms("1 |> a.b 'x': 2")
      assert_conforms("1 |> a.b x: 2, y: 1")
      assert_conforms("1 |> a.b 'x': 2, y: 1")
      assert_conforms("1 |> a.b x: 2, 'y': 1")
      assert_conforms("1 |> a.b x: foo bar, baz")
      assert_conforms("1 |> a.b 'x': foo bar, baz")
      assert_conforms("1 |> a.b x: 2, y: foo bar, baz")
      assert_conforms("1 |> a.b 'x': 2, y: foo bar, baz")
      assert_conforms("1 |> a.b x: 2, 'y': foo bar, baz")
    end
  end

  describe "matched_expr - unary operations" do
    test "unary_op_eol (!, ^, not, ~~~)" do
      # matched_expr -> unary_op_eol matched_expr
      assert_conforms("!true")
      assert_conforms("!\ntrue")
      assert_conforms("not false")
      assert_conforms("not\nfalse")
      assert_conforms("^x")
      assert_conforms("^\nx")
      assert_conforms("~~~1")
      assert_conforms("~~~\n1")
    end

    test "dual_op as unary (+, -)" do
      assert_conforms("-1")
      assert_conforms("-\n1")
      assert_conforms("+1")
      assert_conforms("+\n1")
    end

    # Note: //foo has special parsing (splits into two / ops) - skip for now
    test "ternary_op as unary (//)" do
      assert_conforms("//foo")
      assert_conforms("//\nfoo")
    end

    test "at_op_eol (@)" do
      # matched_expr -> at_op_eol matched_expr
      assert_conforms("@foo")
      assert_conforms("@\nfoo")
    end

    test "capture_op_eol (&)" do
      # matched_expr -> capture_op_eol matched_expr
      assert_conforms("&foo")
      assert_conforms("&Mod.fun/1")
      assert_conforms("&\nfoo")
    end

    test "ellipsis_op (...)" do
      # matched_expr -> ellipsis_op matched_expr
      assert_conforms("...foo")
    end
  end

  describe "matched_expr - no_parens_one_expr" do
    test "identifier with single matched arg" do
      # no_parens_one_expr -> dot_identifier call_args_no_parens_one
      assert_conforms("foo 1")
      assert_conforms("bar :atom")
    end

    test "dot identifier with single matched arg" do
      assert_conforms("foo.bar 1")
      assert_conforms("a.b.c :x")
    end

    test "op_identifier with single matched arg" do
      # Unary-looking calls like `a -1` are op_identifier
      assert_conforms("a -1")
      assert_conforms("a +1")
    end

    test "dot_op_identifier with single matched arg" do
      # Unary-looking calls like `a -1` are op_identifier
      assert_conforms("foo.bar -1")
      assert_conforms("foo.bar.baz +1")
    end

    test "identifier with keyword arg" do
      # call_args_no_parens_one -> call_args_no_parens_kw
      assert_conforms("foo x: 1")
      assert_conforms("foo 'x': 1")
      assert_conforms("bar a: 1, b: 2")
      assert_conforms("bar 'a': 1, b: 2")
      assert_conforms("bar a: 1, 'b': 2")
      assert_conforms("bar 'a': 1, 'b': 2")
      assert_conforms("foo x: bar t, r")
      assert_conforms("foo 'x': bar t, r")
      assert_conforms("bar a: 1, b: bar t, r")
      assert_conforms("bar 'a': 1, b: bar t, r")
      assert_conforms("bar a: 1, 'b': bar t, r")
      assert_conforms("bar 'a': 1, 'b': bar t, r")
    end

    test "dot_identifier with keyword arg" do
      # call_args_no_parens_one -> call_args_no_parens_kw
      assert_conforms("foo.bar x: 1")
      assert_conforms("foo.bar.baz a: 1, b: 2")
      assert_conforms("foo.bar 'x': 1")
      assert_conforms("foo.bar.baz 'a': 1, b: 2")
      assert_conforms("foo.bar.baz a: 1, 'b': 2")
      assert_conforms("foo.bar.baz 'a': 1, 'b': 2")
      assert_conforms("foo.bar x: bar t, r")
      assert_conforms("foo.bar.baz a: 1, b: bar t, r")
      assert_conforms("foo.bar 'x': bar t, r")
      assert_conforms("foo.bar.baz 'a': 1, b: bar t, r")
      assert_conforms("foo.bar.baz a: 1, 'b': bar t, r")
      assert_conforms("foo.bar.baz 'a': 1, 'b': bar t, r")
    end
  end

  describe "matched_expr - sub_matched_expr" do
    test "no_parens_zero_expr - bare identifier" do
      # no_parens_zero_expr -> dot_identifier
      assert_conforms("foo")
      assert_conforms("bar")
    end

    test "no_parens_zero_expr - dot identifier" do
      # matched_expr dot_op identifier
      assert_conforms("foo.bar")
      assert_conforms("a.b.c")
      assert_conforms("2.b")
      assert_conforms("1.2.b")
      assert_conforms(":asd.b")
    end

    test "no_parens_zero_expr - bare do_identifier" do
      # no_parens_zero_expr -> dot_identifier
      assert_conforms("foo do\n:ok\nend")
      assert_conforms("bar do\n:ok\nend")
    end

    test "no_parens_zero_expr - dot do_identifier" do
      # matched_expr dot_op identifier
      assert_conforms("foo.bar do\n:ok\nend")
      assert_conforms("a.b.c do\n:ok\nend")
      assert_conforms("2.b do\n:ok\nend")
      assert_conforms("1.2.b do\n:ok\nend")
      assert_conforms(":asd.b do\n:ok\nend")
    end

    test "range_op nullary (..)" do
      # sub_matched_expr -> range_op : build_nullary_op('$1')
      assert_conforms("..")
    end

    test "ellipsis_op nullary (...)" do
      # sub_matched_expr -> ellipsis_op : build_nullary_op('$1')
      assert_conforms("...")
    end
  end

  describe "matched_expr - access_expr" do
    test "bracket_expr - access syntax" do
      # access_expr -> bracket_expr
      # bracket_expr -> dot_bracket_identifier bracket_arg
      # bracket_expr -> access_expr bracket_arg
      # dot_bracket_identifier -> bracket_identifier
      # dot_bracket_identifier -> matched_expr dot_op bracket_identifier
      assert_conforms("foo[:bar]")
      assert_conforms("foo[\n:bar]")
      assert_conforms("foo[:bar\n]")
      assert_conforms("foo[:bar,]")
      assert_conforms("foo[0]")
      assert_conforms("foo[a b]")
      assert_conforms("foo[if a do\n:ok\nend]")
      assert_conforms("foo[a: 1]")
      assert_conforms("foo['a': 1]")
      assert_conforms("foo[a: 1,]")
      assert_conforms("foo[a: 1, b: 2]")
      assert_conforms("foo['a': 1, b: 2]")
      assert_conforms("foo[a: 1, 'b': 2]")
      assert_conforms("foo['a': 1, 'b': 2]")
      assert_conforms("foo[:a][:b]")
      assert_conforms("foo.bar[0]")
      assert_conforms("foo.\nbar[0]")
      assert_conforms("1.bar[0]")
      assert_conforms("foo.bar[0][:ok]")
    end

    test "bracket_at_expr - @ with access" do
      # bracket_at_expr -> at_op_eol access_expr bracket_arg
      assert_conforms("@foo[1]")
      assert_conforms("@\nfoo[1]")
      assert_conforms("@foo.bar[:key]")
      assert_conforms("@foo[1][:ok]")
      assert_conforms("@foo.bar[1][:ok]")
    end

    test "capture_int" do
      # access_expr -> capture_int int
      assert_conforms("&1")
      assert_conforms("&2")
      assert_conforms("&42")
    end

    test "fn expression" do
      # access_expr -> fn_eoe stab_eoe 'end'
      # stab -> stab_expr
      # stab -> stab eoe stab_expr

      assert_conforms("fn 1 -> 2 end")
      assert_conforms("fn 1 -> 2\n end")
      assert_conforms("fn 1 -> 2; end")
      assert_conforms("fn 1 -> 2\n; end")

      assert_conforms("fn;1 -> 2 end")
      assert_conforms("fn\n1 -> 2 end")
      assert_conforms("fn\n;1 -> 2 end")

      assert_conforms("fn 1 -> 2\n3 -> 4 end")
      assert_conforms("fn 1 -> 2;3 -> 4 end")
      assert_conforms("fn 1 -> 2\n;3 -> 4 end")

      assert_conforms("fn 1 -> ;fs end")

      # stab_expr -> expr
      # this case is an error
      # stab_expr -> stab_op_eol_and_expr
      # stab_op_eol_and_expr -> stab_op_eol expr
      # stab_op_eol_and_expr -> stab_op_eol
      assert_conforms("fn -> foo() end")
      assert_conforms("fn -> if a do\n:ok\nend end")
      assert_conforms("fn -> foo 1, 2, 3 end")
      assert_conforms("fn ->\nfoo() end")
      assert_conforms("fn ->\nif a do\n:ok\nend end")
      assert_conforms("fn ->\nfoo 1, 2, 3 end")
      assert_conforms("fn -> end")
      assert_conforms("fn ->\nend")
      # stab_expr -> empty_paren stab_op_eol_and_expr
      assert_conforms("fn () -> foo() end")
      assert_conforms("fn (\n) -> foo() end")
      assert_conforms("fn () -> if a do\n:ok\nend end")
      assert_conforms("fn () -> foo 1, 2, 3 end")
      assert_conforms("fn () ->\nfoo() end")
      assert_conforms("fn () ->\nif a do\n:ok\nend end")
      assert_conforms("fn () ->\nfoo 1, 2, 3 end")
      assert_conforms("fn () -> end")
      assert_conforms("fn () ->\nend")
      # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
      assert_conforms("fn () when x -> foo() end")
      assert_conforms("fn () when bar() -> foo() end")
      assert_conforms("fn () when if a do\n:ok\nend -> foo() end")
      assert_conforms("fn () when bar 1, 2, 3 -> foo() end")
      assert_conforms("fn (\n) when x -> foo() end")
      assert_conforms("fn () when x -> if a do\n:ok\nend end")
      assert_conforms("fn () when x -> foo 1, 2, 3 end")
      assert_conforms("fn () when x ->\nfoo() end")
      assert_conforms("fn () when x ->\nif a do\n:ok\nend end")
      assert_conforms("fn () when x ->\nfoo 1, 2, 3 end")
      assert_conforms("fn () when x -> end")
      assert_conforms("fn () when x ->\nend")
      # stab_expr -> call_args_no_parens_all stab_op_eol_and_expr
      # call_args_no_parens_all -> call_args_no_parens_one
      # call_args_no_parens_all -> call_args_no_parens_ambig
      # call_args_no_parens_all -> call_args_no_parens_many

      # call_args_no_parens_one -> call_args_no_parens_kw
      # call_args_no_parens_one -> matched_expr
      assert_conforms("fn x -> foo() end")
      assert_conforms("fn 1 -> foo() end")
      assert_conforms("fn g() -> foo() end")

      assert_conforms("fn x: 1 -> foo() end")
      assert_conforms("fn 'x': 1 -> foo() end")
      assert_conforms("fn x: 1, y: :ok -> foo() end")
      assert_conforms("fn x: 1, 'y': :ok -> foo() end")
      assert_conforms("fn 'x': 1, y: :ok -> foo() end")
      assert_conforms("fn x: 1,\ny: :ok -> foo() end")
      assert_conforms("fn x: 1, y:\n:ok -> foo() end")
      assert_conforms("fn x: q() -> foo() end")
      assert_conforms("fn x: bar 1, 2 -> foo() end")

      # call_args_no_parens_ambig -> no_parens_expr
      assert_conforms("fn x 1, 2, 3 -> foo() end")

      # call_args_no_parens_many -> matched_expr ',' call_args_no_parens_kw
      assert_conforms("fn x, a: 1 -> foo() end")
      assert_conforms("fn x,\na: 1 -> foo() end")
      assert_conforms("fn x(), a: 1 -> foo() end")
      assert_conforms("fn x, \na: 1 -> foo() end")
      assert_conforms("fn x, a:\n 1 -> foo() end")
      assert_conforms("fn x, 'a': 1 -> foo() end")
      assert_conforms("fn x, a: b c, d -> foo() end")
      # call_args_no_parens_many -> call_args_no_parens_comma_expr
      # call_args_no_parens_comma_expr -> matched_expr ',' call_args_no_parens_expr
      # call_args_no_parens_comma_expr -> call_args_no_parens_comma_expr ',' call_args_no_parens_expr
      # call_args_no_parens_expr -> matched_expr
      assert_conforms("fn x, y -> foo() end")
      assert_conforms("fn x,\ny -> foo() end")
      assert_conforms("fn x(), y() -> foo() end")
      assert_conforms("fn x(), y(), 1 -> foo() end")
      # call_args_no_parens_many -> call_args_no_parens_comma_expr ',' call_args_no_parens_kw
      assert_conforms("fn x, y: 1 -> foo() end")
      assert_conforms("fn x, y(), z: 1 -> foo() end")
      assert_conforms("fn x, y(),\nz: 1 -> foo() end")
      assert_conforms("fn x, y(), z:\n1 -> foo() end")
      assert_conforms("fn x, y(), 'z': 1 -> foo() end")
      assert_conforms("fn x, y(), z: a b, c -> foo() end")

      # stab_expr -> stab_parens_many stab_op_eol_and_expr
      # stab_parens_many -> open_paren call_args_no_parens_kw close_paren
      assert_conforms("fn (a: 1) -> foo() end")
      assert_conforms("fn (a:\n1, b: :ok) -> foo() end")
      assert_conforms("fn ('a': 1) -> foo() end")
      assert_conforms("fn ('a\#{a}': 1) -> foo() end")
      assert_conforms("fn (a: b c, d) -> foo() end")
      # stab_parens_many -> open_paren call_args_no_parens_many close_paren
      assert_conforms("fn (x, a: 1) -> foo() end")
      assert_conforms("fn (\nx, a: 1) -> foo() end")
      assert_conforms("fn (x, y(), a: 1\n) -> foo() end")
      assert_conforms("fn (x,\na: 1) -> foo() end")
      assert_conforms("fn (x(), a: 1) -> foo() end")
      assert_conforms("fn (x, \na: 1) -> foo() end")
      assert_conforms("fn (x, a:\n 1) -> foo() end")
      assert_conforms("fn (x(), 'a': 1) -> foo() end")

      # stab_expr -> stab_parens_many when_op expr stab_op_eol_and_expr
      assert_conforms("fn (a: 1) when x() -> foo() end")
      assert_conforms("fn ('a': 1) when x() -> foo() end")
      assert_conforms("fn (a: 1) when if z do\n:ok\nend -> foo() end")
      assert_conforms("fn (a: 1) when x 4, 5, 6 -> foo() end")
      assert_conforms("fn (a:\n1, b: :ok) when x() -> foo() end")
      assert_conforms("fn (x, a: 1) when x() -> foo() end")
      assert_conforms("fn (x, 'a': 1) when x() -> foo() end")
      assert_conforms("fn (\nx, a: 1) when x() -> foo() end")

      assert_conforms("fn 1 -> ;-> end")
    end

    test "paren stab" do
      # access_expr -> open_paren stab_eoe ')'
      # access_expr -> open_paren ';' stab_eoe ')'

      # stab -> stab_expr
      # stab -> stab eoe stab_expr

      assert_conforms("(1 -> 2)")
      assert_conforms("(;1 -> 2)")
      assert_conforms("(1 -> 2\n)")
      assert_conforms("(;1 -> 2\n)")
      assert_conforms("(1 -> 2;)")
      assert_conforms("(;1 -> 2;)")
      assert_conforms("(1 -> 2\n;)")
      assert_conforms("(;1 -> 2\n;)")

      assert_conforms("(1 -> 2\n3 -> 4)")
      assert_conforms("(1 -> 2;3 -> 4)")
      assert_conforms("(1 -> 2\n;3 -> 4)")

      # stab_expr -> expr
      assert_conforms("(1)")
      assert_conforms("(foo)")
      assert_conforms("(foo())")
      assert_conforms("(if a do\n:ok\nend)")
      assert_conforms("(foo 1, 2, 3)")
      # stab_expr -> stab_op_eol_and_expr
      # stab_op_eol_and_expr -> stab_op_eol expr
      # stab_op_eol_and_expr -> stab_op_eol
      assert_conforms("(-> foo())")
      assert_conforms("(-> if a do\n:ok\nend)")
      assert_conforms("(-> foo 1, 2, 3)")
      assert_conforms("(->\nfoo())")
      assert_conforms("(->\nif a do\n:ok\nend)")
      assert_conforms("(->\nfoo 1, 2, 3)")
      assert_conforms("(->)")
      assert_conforms("(->\n)")
      # stab_expr -> empty_paren stab_op_eol_and_expr
      assert_conforms("(() -> foo())")
      assert_conforms("((\n) -> foo())")
      assert_conforms("(() -> if a do\n:ok\nend)")
      assert_conforms("(() -> foo 1, 2, 3)")
      assert_conforms("(() ->\nfoo())")
      assert_conforms("(() ->\nif a do\n:ok\nend)")
      assert_conforms("(() ->\nfoo 1, 2, 3)")
      assert_conforms("(() ->)")
      assert_conforms("(() ->\n)")
      # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
      assert_conforms("(() when x -> foo())")
      assert_conforms("(() when bar() -> foo())")
      assert_conforms("(() when if a do\n:ok\nend -> foo())")
      assert_conforms("(() when bar 1, 2, 3 -> foo())")
      assert_conforms("((\n) when x -> foo())")
      assert_conforms("(() when x -> if a do\n:ok\nend)")
      assert_conforms("(() when x -> foo 1, 2, 3)")
      assert_conforms("(() when x ->\nfoo())")
      assert_conforms("(() when x ->\nif a do\n:ok\nend)")
      assert_conforms("(() when x ->\nfoo 1, 2, 3)")
      assert_conforms("(() when x ->)")
      assert_conforms("(() when x ->\n)")
      # stab_expr -> call_args_no_parens_all stab_op_eol_and_expr
      # call_args_no_parens_all -> call_args_no_parens_one
      # call_args_no_parens_all -> call_args_no_parens_ambig
      # call_args_no_parens_all -> call_args_no_parens_many

      # call_args_no_parens_one -> call_args_no_parens_kw
      # call_args_no_parens_one -> matched_expr
      assert_conforms("(x -> foo())")
      assert_conforms("(1 -> foo())")
      assert_conforms("(g() -> foo())")

      assert_conforms("(x: 1 -> foo())")
      assert_conforms("('x': 1 -> foo())")
      assert_conforms("(x: 1, y: :ok -> foo())")
      assert_conforms("(x: 1, 'y': :ok -> foo())")
      assert_conforms("(x: 1,\ny: :ok -> foo())")
      assert_conforms("(x: 1, y:\n:ok -> foo())")
      assert_conforms("(x: q() -> foo())")
      assert_conforms("(x: bar 1, 2 -> foo())")

      # call_args_no_parens_ambig -> no_parens_expr
      assert_conforms("(x 1, 2, 3 -> foo())")

      # call_args_no_parens_many -> matched_expr ',' call_args_no_parens_kw
      assert_conforms("(x, a: 1 -> foo())")
      assert_conforms("(x, 'a': 1 -> foo())")
      assert_conforms("(x, a: b c, d -> foo())")
      assert_conforms("(x,\na: 1 -> foo())")
      assert_conforms("(x(), a: 1 -> foo())")
      assert_conforms("(x, \na: 1 -> foo())")
      assert_conforms("(x, a:\n 1 -> foo())")
      # call_args_no_parens_many -> call_args_no_parens_comma_expr
      # call_args_no_parens_comma_expr -> matched_expr ',' call_args_no_parens_expr
      # call_args_no_parens_comma_expr -> call_args_no_parens_comma_expr ',' call_args_no_parens_expr
      # call_args_no_parens_expr -> matched_expr
      assert_conforms("(x, y -> foo())")
      assert_conforms("(x,\ny -> foo())")
      assert_conforms("(x(), y() -> foo())")
      assert_conforms("(x(), y(), 1 -> foo())")
      # call_args_no_parens_many -> call_args_no_parens_comma_expr ',' call_args_no_parens_kw
      assert_conforms("(x, y: 1 -> foo())")
      assert_conforms("(x, 'y': 1 -> foo())")
      assert_conforms("(x, y: a b, c -> foo())")
      assert_conforms("(x, y(), z: 1 -> foo())")
      assert_conforms("(x, y(),\nz: 1 -> foo())")
      assert_conforms("(x, y(), z:\n1 -> foo())")

      # stab_expr -> stab_parens_many stab_op_eol_and_expr
      # stab_parens_many -> open_paren call_args_no_parens_kw close_paren
      assert_conforms("((a: 1) -> foo())")
      assert_conforms("(('a': 1) -> foo())")
      assert_conforms("(('a\#{s}': 1) -> foo())")
      assert_conforms("((a:\n1, b: :ok) -> foo())")
      assert_conforms("((a: b c, d) -> foo())")
      # stab_parens_many -> open_paren call_args_no_parens_many close_paren
      assert_conforms("((x, a: 1) -> foo())")
      assert_conforms("((\nx, a: 1) -> foo())")
      assert_conforms("((x, y(), a: 1\n) -> foo())")
      assert_conforms("((x,\na: 1) -> foo())")
      assert_conforms("((x(), a: 1) -> foo())")
      assert_conforms("((x, \na: 1) -> foo())")
      assert_conforms("((x, a:\n 1) -> foo())")

      # stab_expr -> stab_parens_many when_op expr stab_op_eol_and_expr
      assert_conforms("((a: 1) when x() -> foo())")
      assert_conforms("((a: 1) when if z do\n:ok\nend -> foo())")
      assert_conforms("((a: 1) when x 4, 5, 6 -> foo())")
      assert_conforms("((a:\n1, b: :ok) when x() -> foo())")
      assert_conforms("((x, a: 1) when x() -> foo())")
      assert_conforms("((\nx, a: 1) when x() -> foo())")
    end

    test "paren with semicolon" do
      # access_expr -> open_paren ';' stab_eoe ')'
      # access_expr -> open_paren ';' close_paren
      assert_conforms("(;)")
      assert_conforms("(\n;)")
      assert_conforms("(;\n)")

      assert_conforms("(;1)")
      assert_conforms("(;d)")
      assert_conforms("(;!e)")
      assert_conforms("(\n;1)")
      assert_conforms("(;1\n)")
      assert_conforms("(;e;)")
      assert_conforms("(;l;f)")
    end

    test "empty_paren" do
      # access_expr -> empty_paren
      assert_conforms("()")
      assert_conforms("(\n)")
    end

    test "parenthesized expression" do
      assert_conforms("(1)")
      assert_conforms("(1 + 2)")
      assert_conforms("((1))")
    end

    test "list" do
      # access_expr -> list
      assert_conforms("[]")
      assert_conforms("[\n]")
      assert_conforms("[1]")
      assert_conforms("[1, 2, 3]")
      assert_conforms("[1, 2, 3,]")
      assert_conforms("[1, 2, 3\n]")
      assert_conforms("[a: 1]")
      assert_conforms("[\na: 1]")
      assert_conforms("[a: 1,]")
      assert_conforms("[a: 1\n]")
      assert_conforms("[1, a: 1]")
      assert_conforms("[1, if a do\n:ok\nend]")
      assert_conforms("[1, if a do\n:ok\nend, x: y]")
    end

    test "tuple" do
      # access_expr -> tuple

      # tuple -> open_curly '}'
      # tuple -> open_curly container_args close_curly
      assert_conforms("{}")
      assert_conforms("{\n}")
      assert_conforms("{1, 2}")
      assert_conforms("{\n1, 2}")
      assert_conforms("{1, 2\n}")
      assert_conforms("{1, 2,}")

      assert_conforms("{1, if a do\n:ok\nend, 3}")
      assert_conforms("{foo, bar: baz}")
      assert_conforms("{foo, bar: baz\n}")
    end

    test "map" do
      # access_expr -> map
      # map -> map_op map_args
      assert_conforms("%{}")
      assert_conforms("%{\n}")
      assert_conforms("%{a: 1}")
      assert_conforms("%{'a': 1}")
      assert_conforms("%{'a\#{foo}': 1}")
      assert_conforms("%{a: 1,}")
      assert_conforms("%{a: 1,\n}")

      # assoc_expr -> matched_expr assoc_op_eol matched_expr
      assert_conforms("%{:a => 1}")
      assert_conforms("%{:a =>\n1}")
      # assoc_expr -> unmatched_expr assoc_op_eol unmatched_expr
      assert_conforms("%{if a do\n:ok\nend => unless b do\n:error\nend}")
      assert_conforms("%{if a do\n:ok\nend =>\nunless b do\n:error\nend}")
      # assoc_expr -> matched_expr assoc_op_eol unmatched_expr
      assert_conforms("%{:a => if a do\n:ok\nend}")
      assert_conforms("%{:a =>\nif a do\n:ok\nend}")
      # assoc_expr -> unmatched_expr assoc_op_eol matched_expr
      assert_conforms("%{if a do\n:ok\nend => 1}")
      assert_conforms("%{if a do\n:ok\nend =>\n1}")
      # assoc_expr -> map_base_expr
      # map_base_expr -> sub_matched_expr
      assert_conforms("%{x}")
      assert_conforms("%{1}")
      assert_conforms("%{:ok}")
      assert_conforms("%{foo()}")
      # map_base_expr -> at_op_eol map_base_expr
      assert_conforms("%{@foo}")
      assert_conforms("%{@\nfoo}")
      # map_base_expr -> unary_op_eol map_base_expr
      assert_conforms("%{!foo}")
      assert_conforms("%{!\nfoo}")
      assert_conforms("%{-foo}")
      assert_conforms("%{-\nfoo}")

      assert_conforms("%{//foo}")
      assert_conforms("%{//\nfoo}")

      # map_base_expr -> ellipsis_op map_base_expr
      assert_conforms("%{...x}")
      assert_conforms("%{...1}")

      assert_conforms("%{x,}")
      assert_conforms("%{x\n}")

      assert_conforms("%{x, foo: 1}")
      assert_conforms("%{x, 'foo': 1}")
      assert_conforms("%{x, foo: 1,}")
      assert_conforms("%{x, foo: 1\n}")

      assert_conforms("%{a: 1, b: 2}")
      assert_conforms("%{1 => 2}")

      assert_conforms("%{:a => 1, b: 2}")
    end

    test "struct" do
      # map -> '%' map_base_expr map_args
      # map -> '%' map_base_expr eol map_args
      assert_conforms("%Foo{}")
      assert_conforms("%Foo{\n}")
      assert_conforms("%Foo{a: 1}")
      assert_conforms("%Foo{a: 1,}")
      assert_conforms("%Foo{a: 1,\n}")

      # assoc_expr -> matched_expr assoc_op_eol matched_expr
      assert_conforms("%Foo{:a => 1}")
      assert_conforms("%Foo{:a =>\n1}")
      # assoc_expr -> unmatched_expr assoc_op_eol unmatched_expr
      assert_conforms("%Foo{if a do\n:ok\nend => unless b do\n:error\nend}")
      assert_conforms("%Foo{if a do\n:ok\nend =>\nunless b do\n:error\nend}")
      # assoc_expr -> matched_expr assoc_op_eol unmatched_expr
      assert_conforms("%Foo{:a => if a do\n:ok\nend}")
      assert_conforms("%Foo{:a =>\nif a do\n:ok\nend}")
      # assoc_expr -> unmatched_expr assoc_op_eol matched_expr
      assert_conforms("%Foo{if a do\n:ok\nend => 1}")
      assert_conforms("%Foo{if a do\n:ok\nend =>\n1}")
      # assoc_expr -> map_base_expr
      # map_base_expr -> sub_matched_expr
      assert_conforms("%Foo{x}")
      assert_conforms("%Foo{1}")
      assert_conforms("%Foo{:ok}")
      assert_conforms("%Foo{foo()}")
      # map_base_expr -> at_op_eol map_base_expr
      assert_conforms("%Foo{@foo}")
      assert_conforms("%Foo{@\nfoo}")
      # map_base_expr -> unary_op_eol map_base_expr
      assert_conforms("%Foo{!foo}")
      assert_conforms("%Foo{!\nfoo}")
      assert_conforms("%Foo{-foo}")
      assert_conforms("%Foo{-\nfoo}")

      assert_conforms("%Foo{//foo}")
      assert_conforms("%Foo{//\nfoo}")

      # map_base_expr -> ellipsis_op map_base_expr
      assert_conforms("%Foo{...x}")
      assert_conforms("%Foo{...1}")

      assert_conforms("%Foo{x,}")
      assert_conforms("%Foo{x\n}")

      assert_conforms("%Foo{x, foo: 1}")
      assert_conforms("%Foo{x, foo: 1,}")
      assert_conforms("%Foo{x, foo: 1\n}")

      assert_conforms("%Foo{a: 1, b: 2}")
      assert_conforms("%Foo{1 => 2}")

      assert_conforms("%Foo{:a => 1, b: 2}")

      assert_conforms("%x{}")
      assert_conforms("%@foo{}")
      assert_conforms("%@\nfoo{}")
      assert_conforms("%-foo{}")
      assert_conforms("%-\nfoo{}")
      assert_conforms("%!foo{}")
      assert_conforms("%!\nfoo{}")

      assert_conforms("%//foo{}")
      assert_conforms("%//\nfoo{}")

      assert_conforms("%...foo{}")
      assert_conforms("%''{}")
      assert_conforms("%\"\"{}")
      assert_conforms("%0[l]{}")
      assert_conforms("%nil{}")
      assert_conforms("%__no_struct_base__{}")
      assert_conforms("%:__no_struct_base__{}")
      assert_conforms("%!(){}")
      assert_conforms("%@[]{}")
      assert_conforms("% (){}")
      assert_conforms("%@0.a{}")
      assert_conforms("%@o.f{}")
      assert_conforms("%^t.d{}")
      assert_conforms("%@+a.i{}")
    end

    test "map update" do
      assert_conforms("%{map | :a => 1}")
      assert_conforms("%{map |\n:a => 1}")
      assert_conforms("%{map | :a => 1,}")
      assert_conforms("%{map | :a => 1\n}")
      assert_conforms("%{\nmap | :a => 1}")
      assert_conforms("%{if foo do\n:ok\nend | :a => 1, b => 2}")

      assert_conforms("%{map | a: 1}")
      assert_conforms("%{\nmap | a: 1}")
      assert_conforms("%{map | a: 1,}")
      assert_conforms("%{map | a: 1\n}")
      assert_conforms("%{if foo do\n:ok\nend | a: 1, b: 2}")
      assert_conforms("%{if foo do\n:ok\nend | a: 1, 'b': 2}")
      assert_conforms("%{if foo do\n:ok\nend | :a => 1, b: 2}")
      assert_conforms("%{if foo do\n:ok\nend | :a => 1, 'b': 2}")
      assert_conforms("%{'' | x: y}")
    end

    test "not a map update" do
      assert_conforms("%{n&n=>1|e}")
    end

    test "struct update" do
      assert_conforms("%Foo{map | :a => 1}")
      assert_conforms("%Foo{map |\n:a => 1}")
      assert_conforms("%Foo{map | :a => 1,}")
      assert_conforms("%Foo{map | :a => 1\n}")
      assert_conforms("%Foo{\nmap | :a => 1}")
      assert_conforms("%Foo{if foo do\n:ok\nend | :a => 1, b => 2}")

      assert_conforms("%foo{map | a: 1}")
      assert_conforms("%foo{\nmap | a: 1}")
      assert_conforms("%foo{map | a: 1,}")
      assert_conforms("%foo{map | a: 1\n}")
      assert_conforms("%foo{if foo do\n:ok\nend | a: 1, b: 2}")
      assert_conforms("%foo{if foo do\n:ok\nend | :a => 1, b: 2}")
      assert_conforms("%foo{if foo do\n:ok\nend | :a => 1, 'b': 2}")
    end

    test "bitstring" do
      # access_expr -> bitstring
      assert_conforms("<<>>")
      assert_conforms("<<\n>>")
      assert_conforms("<<1, 2>>")
      assert_conforms("<<\n1, 2>>")
      assert_conforms("<<1, 2\n>>")
      assert_conforms("<<1, 2,>>")

      assert_conforms("<<1, if a do\n:ok\nend, 3>>")
      assert_conforms("<<foo, bar: baz>>")
      assert_conforms("<<foo, bar: baz\n>>")
      assert_conforms("<<foo, 'bar': baz>>")
      assert_conforms("<<foo, bar: baz, boom: bang>>")
      assert_conforms("<<foo, bar: baz, 'boom': bang>>")
      assert_conforms("<<foo, 'bar': baz, boom: bang>>")
      assert_conforms("<<foo, 'bar': baz, 'boom': bang>>")
      assert_conforms("<<x::binary>>")
    end

    test "dot_alias" do
      # access_expr -> dot_alias

      # dot_alias -> alias
      assert_conforms("Foo")
      # dot_alias -> matched_expr dot_op alias
      assert_conforms("Foo.Bar")
      assert_conforms("foo.Bar")
    end

    test "dot_alias tuple call" do
      # access_expr -> dot_alias

      # dot_alias -> matched_expr dot_op open_curly '}'
      assert_conforms("Foo.{}")
      assert_conforms("Foo.{\n}")
      assert_conforms("foo.{}")
      assert_conforms("Foo.Bar.{}")

      # dot_alias -> matched_expr dot_op open_curly container_args close_curly

      # container_args allow any number of matched_expr, unmatched_expr optionally followed by kw_data

      assert_conforms("Foo.{a}")
      assert_conforms("Foo.{A}")
      assert_conforms("Foo.{A\n}")

      assert_conforms("Foo.{A,}")
      assert_conforms("Foo.{A,\n}")

      assert_conforms("Foo.{A, b.C}")

      assert_conforms("Foo.{A, foo: x}")
      assert_conforms("Foo.{A, 'foo': x}")

      assert_conforms("Foo.{A, foo: x, bar: y}")
      assert_conforms("Foo.{A, foo: x, 'bar': y}")
      assert_conforms("Foo.{A, 'foo': x, bar: y}")
      assert_conforms("Foo.{A, 'foo': x, 'bar': y}")

      assert_conforms("Foo.{A, if a do\n:ok\nend}")

      assert_conforms("Foo.{A, if a do\n:ok\nend,\nB}")
    end

    test "parens_call" do
      # access_expr -> parens_call
      # parens_call -> dot_call_identifier call_args_parens

      # dot_call_identifier -> dot_paren_identifier
      # dot_call_identifier -> matched_expr dot_call_op

      # dot_paren_identifier -> paren_identifier
      # dot_paren_identifier -> matched_expr dot_op paren_identifier

      # dot_op -> '.'
      # dot_op -> '.' eol
      assert_conforms("foo()")
      assert_conforms("foo(\n)")
      assert_conforms("foo(1)")
      assert_conforms("foo(\n1)")
      assert_conforms("foo(1\n)")
      assert_conforms("foo.(1)")
      assert_conforms("1.(1)")
      assert_conforms("1.\n(1)")
      assert_conforms("foo(1, 2)")
      assert_conforms("foo.bar()")
      assert_conforms("foo.\nbar()")
      assert_conforms("foo.bar(1)")
    end

    test "parens_call no_parens_expr" do
      # only single no_parens_expr allowed
      assert_conforms("foo(bar 1, 2, 3)")
      assert_conforms("foo(\nbar 1, 2, 3)")
      assert_conforms("foo(bar 1, 2, 3\n)")
    end

    test "parens_call unmatched_expr" do
      assert_conforms("foo(if a do\n:ok\nend)")
      assert_conforms("foo(1, if a do\n:ok\nend)")
      assert_conforms("foo(if a do\n:ok\nend, 1)")
    end

    test "parens_call kw_call" do
      assert_conforms("foo(bar: 1)")
      assert_conforms("foo(\nbar: 1)")
      assert_conforms("foo(bar: 1\n)")
      assert_conforms("foo(bar: 1,)")
      assert_conforms("foo(bar: 1, baz: :ok)")

      assert_conforms("foo(x, bar: 1)")
      assert_conforms("foo(\nx, bar: 1)")
      assert_conforms("foo(\nx, bar: 1\n)")

      assert_conforms("foo(if a do\n:ok\nend, bar: 1)")
    end

    test "nested parens_call" do
      # access_expr -> parens_call
      # parens_call -> dot_call_identifier call_args_parens call_args_parens
      assert_conforms("foo()()")
      assert_conforms("foo.()()")
      assert_conforms("foo(1)(2)")
      assert_conforms("foo.bar()()")
      assert_conforms("1.bar()()")
      assert_conforms("1.bar(if a do\n:ok\nend)(bar 1, 2, 3)")
    end

    test "chained parens_call" do
      assert_conforms("@i.().o")
    end
  end

  # =============================================================================
  # UNMATCHED_EXPR
  # From elixir_parser.yrl lines 163-171
  # Expressions that contain do/end blocks
  # =============================================================================

  describe "unmatched_expr - binary operations with unmatched RHS" do
    test "matched_expr unmatched_op_expr" do
      # unmatched_expr -> matched_expr unmatched_op_expr : build_op('$1', '$2').
      # All binary ops with do/end block as RHS

      # match_op_eol unmatched_expr
      assert_conforms("1 = if true do\n:ok\nend")
      assert_conforms("1 =\nif true do\n:ok\nend")

      # dual_op_eol unmatched_expr
      assert_conforms("1 + if true do\n:ok\nend")
      assert_conforms("1 +\nif true do\n:ok\nend")

      # mult_op_eol unmatched_expr
      assert_conforms("1 * if true do\n:ok\nend")
      assert_conforms("1 *\nif true do\n:ok\nend")

      # power_op_eol unmatched_expr
      assert_conforms("1 ** if true do\n:ok\nend")
      assert_conforms("1 **\nif true do\n:ok\nend")

      # concat_op_eol unmatched_expr
      assert_conforms("a <> if true do\n:x\nend")
      assert_conforms("a <>\nif true do\n:x\nend")

      # range_op_eol unmatched_expr
      assert_conforms("a .. if true do\n:x\nend")
      assert_conforms("a ..\nif true do\n:x\nend")

      # ternary_op_eol unmatched_expr
      assert_conforms("a..b//if true do\n:x\nend")
      assert_conforms("a..b//\nif true do\n:x\nend")

      # and_op_eol unmatched_expr
      assert_conforms("true && if true do\n:ok\nend")
      assert_conforms("a and\nif true do\n:ok\nend")

      # or_op_eol unmatched_expr
      assert_conforms("false || if true do\n:ok\nend")
      assert_conforms("a or\nif true do\n:ok\nend")

      # in_op_eol unmatched_expr
      assert_conforms("1 in if true do\n[1]\nend")
      assert_conforms("1 in\nif true do\n[1]\nend")

      assert_conforms("1 not in if true do\n[1]\nend")
      assert_conforms("1 not in\nif true do\n[1]\nend")

      # in_match_op_eol unmatched_expr
      assert_conforms("1 <- if true do\n[1]\nend")
      assert_conforms("1 <-\nif true do\n[1]\nend")

      # type_op_eol unmatched_expr
      assert_conforms("1 :: if true do\n[1]\nend")
      assert_conforms("1 ::\nif true do\n[1]\nend")

      # when_op_eol unmatched_expr
      assert_conforms("1 when if true do\n[1]\nend")
      assert_conforms("1 when\nif true do\n[1]\nend")

      # pipe_op_eol unmatched_expr
      assert_conforms("1 | if true do\n1\nend")
      assert_conforms("1 |\nif true do\n1\nend")

      # comp_op_eol unmatched_expr
      assert_conforms("1 == if true do\n1\nend")
      assert_conforms("1 ==\nif true do\n1\nend")

      # rel_op_eol unmatched_expr
      assert_conforms("1 < if true do\n2\nend")
      assert_conforms("1 <\nif true do\n2\nend")

      # arrow_op_eol unmatched_expr
      assert_conforms("a |> if true do\n:ok\nend")
      assert_conforms("a |>\nif true do\n:ok\nend")
    end

    test "unmatched_expr matched_op_expr" do
      # unmatched_expr -> unmatched_expr matched_op_expr : build_op('$1', '$2').
      # do/end block followed by binary op with matched expr

      assert_conforms("if true do\n:ok\nend + 1")
      assert_conforms("if true do\n:ok\nend *\n2")
      assert_conforms("if true do\n:ok\nend == :ok")
      assert_conforms("if true do\n:ok\nend |>\nfoo")
    end

    test "unmatched_expr unmatched_op_expr" do
      # unmatched_expr -> unmatched_expr unmatched_op_expr : build_op('$1', '$2').
      # do/end block with do/end block on the right

      assert_conforms("if true do\n:ok\nend + if false do\n:error\nend")
      assert_conforms("if true do\n:ok\nend ==\nif false do\n:error\nend")
    end

    test "unmatched_expr no_parens_op_expr" do
      # unmatched_expr -> unmatched_expr no_parens_op_expr
      # do/end block followed by no-parens call as RHS
      # Triggers warn_no_parens_after_do_op

      assert_conforms("if true do\n:ok\nend + foo 1, 2")
      assert_conforms("if true do\n:ok\nend ==\nbar :a, :b")
    end
  end

  describe "unmatched_expr - unary operations with expr RHS" do
    test "unary_op_eol expr" do
      # unmatched_expr -> unary_op_eol expr : build_unary_op('$1', '$2').
      # Unary op applied to any expr (including unmatched)

      assert_conforms("!if true do\n:ok\nend")
      assert_conforms("!\nif true do\n:ok\nend")
      assert_conforms("not if true do\ntrue\nend")
      assert_conforms("-if true do\n1\nend")
      assert_conforms("-\nif true do\n1\nend")
      assert_conforms("//if true do\n1\nend")
      assert_conforms("//\nif true do\n1\nend")
    end

    test "at_op_eol expr" do
      # unmatched_expr -> at_op_eol expr : build_unary_op('$1', '$2').
      assert_conforms("@if true do\n:ok\nend")
      assert_conforms("@\nif true do\n:ok\nend")
    end

    test "capture_op_eol expr" do
      # unmatched_expr -> capture_op_eol expr : build_unary_op('$1', '$2').
      assert_conforms("&if true do\n:ok\nend")
      assert_conforms("&\nif true do\n:ok\nend")
    end

    test "ellipsis_op expr" do
      # unmatched_expr -> ellipsis_op expr : build_unary_op('$1', '$2').
      assert_conforms("...if true do\n:ok\nend")
    end
  end

  describe "unmatched_expr - block_expr" do
    test "dot_call_identifier call_args_parens do_block" do
      assert_conforms("foo.() do\n:ok\nend")
      assert_conforms("foo() do\n:ok\nend")
      assert_conforms("foo.bar() do\n:ok\nend")
    end

    test "dot_call_identifier call_args_parens call_args_parens do_block" do
      assert_conforms("foo.()() do\n:ok\nend")
      assert_conforms("foo()() do\n:ok\nend")
      assert_conforms("foo.bar()() do\n:ok\nend")
    end

    test "dot_do_identifier do_block" do
      assert_conforms("foo do\n:ok\nend")
      assert_conforms("foo.bar do\n:ok\nend")
    end

    test "dot_op_identifier call_args_no_parens_all do_block" do
      assert_conforms("foo -1 do\n:ok\nend")
      assert_conforms("foo.bar -1 do\n:ok\nend")
    end

    test "dot_identifier call_args_no_parens_all do_block" do
      assert_conforms("foo abc do\n:ok\nend")
      assert_conforms("foo.bar abc do\n:ok\nend")

      assert_conforms("foo abc, a: b do\n:ok\nend")
      assert_conforms("foo.bar abc, a: b do\n:ok\nend")
    end

    test "do_block empty" do
      # do_block -> do_eoe 'end'
      assert_conforms("foo do end")
      assert_conforms("foo do;end")
      assert_conforms("foo do\nend")
      assert_conforms("foo do\n;end")
    end

    test "do_block stab_eoe" do
      # do_block -> do_eoe stab_eoe 'end'
    end

    test "do_block block_list" do
      # do_block -> do_eoe block_list 'end'
      # block_list -> block_item
      # block_list -> block_item block_list
      # block_item -> block_eoe stab_eoe
      # block_item -> block_eoe
      # block_eoe -> block_identifier
      # block_eoe -> block_identifier eoe
      assert_conforms("foo do after end")
      assert_conforms("foo do else end")
      assert_conforms("foo do catch end")
      assert_conforms("foo do rescue end")

      assert_conforms("foo do after end")
      assert_conforms("foo do;after;end")
      assert_conforms("foo do\nafter\nend")
      assert_conforms("foo do\n;after\n;end")

      assert_conforms("foo do after else end")

      assert_conforms("foo do after bar() end")
      assert_conforms("foo do after bar 1, 2, 3 end")
      assert_conforms("foo do after bar do\n:ok\nend end")

      assert_conforms("foo do after end")
      assert_conforms("foo do after -> end")
      assert_conforms("foo do after -> bar() end")
      assert_conforms("foo do after bar() -> end")
      assert_conforms("foo do after (a) -> bar 1, 2, 3 end")
      assert_conforms("foo do after (a) when x -> bar do\n:ok\nend end")
    end

    test "do_block stab_eoe block_list" do
      # do_block -> do_eoe stab_eoe block_list 'end'
      assert_conforms("foo do x after end")
      assert_conforms("foo do x 1, 2, 3 after end")
      assert_conforms("foo do x do\n:ok\nend after end")
      assert_conforms("foo do -> after end")
      assert_conforms("foo do bar -> after end")
      assert_conforms("foo do -> bar after end")
      assert_conforms("foo do (bar) -> baz after end")
      assert_conforms("foo do (bar) -> baz after end")
      assert_conforms("foo do (bar) when x -> baz after end")
    end

    test "do/end blocks" do
      # unmatched_expr -> block_expr : '$1'.
      # block_expr -> dot_do_identifier do_block
      assert_conforms("if true do\nend")
      assert_conforms("if true do\n:ok\nend")
      assert_conforms("if true do\n:ok\nelse\n:error\nend")
    end

    test "unless block" do
      assert_conforms("unless false do\n:ok\nend")
      assert_conforms("unless false do\n:ok\nelse\n:error\nend")
    end

    test "case block" do
      assert_conforms("case x do\n1 -> :one\n2 -> :two\nend")
      assert_conforms("case x do\n_ -> :any\nend")
    end

    test "case with guards containing no-parens calls - issue: no-parens call precedence violation" do
      # No-parens calls in guards must stop before the stab operator (->)
      # The stab_op has low precedence and should NOT be consumed by the no-parens call args
      assert_conforms("case x do y when foo 1 + 2 -> :ok end")
      assert_conforms("case x do y when bar a, b -> :ok end")
      assert_conforms("case x do y when baz 1 -> :ok end")
      assert_conforms("case x do y when baz x: 1 -> :ok end")
      assert_conforms("case x do y when baz 'x': 1 -> :ok end")
      assert_conforms("case x do y when baz x: 1, y: 2 -> :ok end")
      assert_conforms("case x do y when baz x: 1, 'y': 2 -> :ok end")
      assert_conforms("case x do y when baz 'x': 1, y: 2 -> :ok end")

      assert_conforms("case x do y when baz a, x: 1 -> :ok end")
      assert_conforms("case x do y when baz a, 'x': 1 -> :ok end")
    end

    test "cond block" do
      assert_conforms("cond do\ntrue -> :ok\nend")
      assert_conforms("cond do\na > 0 -> :positive\na < 0 -> :negative\ntrue -> :zero\nend")
    end

    test "receive block" do
      assert_conforms("receive do\n:msg -> :ok\nend")
      assert_conforms("receive do\n:msg -> :ok\nafter\n1000 -> :timeout\nend")
    end

    test "try block" do
      assert_conforms("try do\n:ok\nrescue\n_ -> :error\nend")
      assert_conforms("try do\n:ok\ncatch\n_ -> :caught\nend")
      assert_conforms("try do\n:ok\nafter\n:cleanup\nend")
    end

    test "with block" do
      assert_conforms("with {:ok, x} <- foo() do\nx\nend")
      assert_conforms("with {:ok, x} <- foo() do\nx\nelse\n_ -> :error\nend")
    end

    test "for comprehension" do
      assert_conforms("for x <- [1, 2, 3] do\nx\nend")
      assert_conforms("for x <- xs, y <- ys do\n{x, y}\nend")
      assert_conforms("for x <- xs, x > 0 do\nx\nend")
    end

    test "block_expr with parens call" do
      # block_expr -> dot_call_identifier call_args_parens do_block
      assert_conforms("foo() do\nend")
      assert_conforms("foo(1) do\n:ok\nend")
      assert_conforms("foo.bar() do\n:ok\nend")
    end

    test "block_expr with no_parens call" do
      # block_expr -> dot_identifier call_args_no_parens_all do_block
      assert_conforms("foo 1 do\nend")
      assert_conforms("foo 1, 2 do\n:ok\nend")
      assert_conforms("foo.bar 1 do\n:ok\nend")
    end

    test "nested parens with do block" do
      # block_expr -> dot_call_identifier call_args_parens call_args_parens do_block
      assert_conforms("foo()() do\nend")
      assert_conforms("foo(1)(2) do\n:ok\nend")
    end

    test "quoted no_parens call do block" do
      assert_conforms("o?n.\"fr\" do :ok end")
    end
  end

  # =============================================================================
  # NO_PARENS_EXPR
  # From elixir_parser.yrl lines 173-179
  # Expressions that are calls without parentheses
  # =============================================================================

  describe "no_parens_expr - binary operations with no_parens RHS" do
    test "matched_expr no_parens_op_expr" do
      # no_parens_expr -> matched_expr no_parens_op_expr : build_op('$1', '$2').
      # Matched expr followed by operator with no-parens call as RHS

      # match_op_eol no_parens_expr
      assert_conforms("1 = foo 2, 3")
      assert_conforms("a =\nbar :x, :y")

      # dual_op_eol no_parens_expr
      assert_conforms("1 + foo 2, 3")
      assert_conforms("a -\nbar :x, :y")

      # mult_op_eol no_parens_expr
      assert_conforms("1 * foo 2, 3")
      assert_conforms("a /\nbar :x, :y")

      # power_op_eol no_parens_expr
      assert_conforms("1 ** foo 2, 3")
      assert_conforms("a **\nbar :x, :y")

      # concat_op_eol no_parens_expr
      assert_conforms("1 ++ foo 2, 3")
      assert_conforms("a --\nbar :x, :y")

      # ternary_op_eol no_parens_expr
      assert_conforms("1..2//foo 2, 3")
      assert_conforms("1..2//\nfoo 2, 3")

      # xor_op_eol no_parens_expr
      assert_conforms("1 ^^^ foo 2, 3")
      assert_conforms("a ^^^\nbar :x, :y")

      # and_op_eol no_parens_expr
      assert_conforms("true && foo 1, 2")
      assert_conforms("a and\nbar :x, :y")

      # or_op_eol no_parens_expr
      assert_conforms("false || foo 1, 2")
      assert_conforms("a or\nbar :x, :y")

      # in_op_eol no_parens_expr
      assert_conforms("false in foo 1, 2")
      assert_conforms("a in\nbar :x, :y")

      assert_conforms("a not in bar :x, :y")
      assert_conforms("a not in\nbar :x, :y")

      # in_match_op_eol no_parens_expr
      assert_conforms("1 <- foo 2, 3")
      assert_conforms("a <-\nbar :x, :y")

      # type_op_eol no_parens_expr
      assert_conforms("1 :: foo 2, 3")
      assert_conforms("a ::\nbar :x, :y")

      # when_op_eol no_parens_expr
      assert_conforms("false when foo 1, 2")
      assert_conforms("a when\nbar :x, :y")

      # pipe_op_eol no_parens_expr
      assert_conforms("1 | foo 2, 3")
      assert_conforms("a |\nbar :x, :y")

      # comp_op_eol no_parens_expr
      assert_conforms("1 == foo 2, 3")
      assert_conforms("a !=\nbar :x, :y")

      # rel_op_eol no_parens_expr
      assert_conforms("1 < foo 2, 3")
      assert_conforms("a >=\nbar :x, :y")

      # arrow_op_eol no_parens_expr
      assert_conforms("a |> foo 1, 2")
      assert_conforms("a <<<\nfoo 1, 2")
    end

    test "when_op_eol call_args_no_parens_kw" do
      # no_parens_op_expr -> when_op_eol call_args_no_parens_kw
      # Allow when (and only when) with keywords
      assert_conforms("x when foo: 1")
      assert_conforms("x when foo: 1, bar: 2")
      assert_conforms("x when 'foo': 1")
      assert_conforms("x when 'foo': 1, bar: 2")
      assert_conforms("x when foo: 1, 'bar': 2")
      assert_conforms("x when 'foo': 1, 'bar': 2")

      # call_args_no_parens_kw allows no_parens_expr as last item
      assert_conforms("x when foo: bar 1, 2")
      assert_conforms("x when 'foo': bar 1, 2")
      assert_conforms("x when abc: 1, foo: bar 1, 2")
      assert_conforms("x when 'abc': 1, foo: bar 1, 2")
      assert_conforms("x when abc: 1, 'foo': bar 1, 2")
    end
  end

  describe "no_parens_expr - unary operations with no_parens RHS" do
    test "unary_op_eol no_parens_expr" do
      # no_parens_expr -> unary_op_eol no_parens_expr : build_unary_op('$1', '$2').
      assert_conforms("!foo 1, 2")
      assert_conforms("!\nfoo 1, 2")
      assert_conforms("not bar :a, :b")
      assert_conforms("-foo 1, 2")
      assert_conforms("-\nfoo 1, 2")
      assert_conforms("//foo 1, 2")
      assert_conforms("//\nfoo 1, 2")
    end

    test "at_op_eol no_parens_expr" do
      # no_parens_expr -> at_op_eol no_parens_expr : build_unary_op('$1', '$2').
      assert_conforms("@foo 1, 2")
      assert_conforms("@\nfoo 1, 2")
    end

    test "capture_op_eol no_parens_expr" do
      # no_parens_expr -> capture_op_eol no_parens_expr : build_unary_op('$1', '$2').
      assert_conforms("&foo 1, 2")
      assert_conforms("&\nfoo 1, 2")
    end

    test "ellipsis_op no_parens_expr" do
      # no_parens_expr -> ellipsis_op no_parens_expr : build_unary_op('$1', '$2').
      assert_conforms("...foo 1, 2")
    end
  end

  describe "no_parens_expr - no_parens_one_ambig_expr" do
    test "nested no_parens call (arity 1 with ambiguous inner)" do
      # no_parens_one_ambig_expr -> dot_identifier call_args_no_parens_ambig
      # This is the `foo bar 1, 2` pattern interpreted as `foo(bar(1, 2))`
      assert_conforms("foo bar 1, 2")
      assert_conforms("outer inner 1, 2, 3")
      assert_conforms("a.b c 1, 2")
    end

    test "op_identifier with ambiguous args" do
      # no_parens_one_ambig_expr -> dot_op_identifier call_args_no_parens_ambig
      assert_conforms("foo -bar 1, 2")
    end

    test "op_identifier with ambiguous args op_identifier" do
      assert_conforms("foo t e")
      assert_conforms("foo t +e")
    end

    test "op_identifier with ambiguous args keyword" do
      assert_conforms("foo t x: 1")
      assert_conforms("foo t 'x': 1")
      assert_conforms("foo t x: 1, y: 2")
      assert_conforms("foo t 'x': 1, y: 2")
      assert_conforms("foo t x: 1, 'y': 2")

      assert_conforms("foo t 0, x: 1")
      assert_conforms("foo t 0, 'x': 1")
      assert_conforms("foo t 0, x: 1, y: 2")
      assert_conforms("foo t 0, 'x': 1, y: 2")
      assert_conforms("foo t 0, x: 1, 'y': 2")
    end

    test "op_identifier with ambiguous args do" do
      assert_conforms("foo t a do :ok end")
      assert_conforms("foo t +a do :ok end")
    end
  end

  describe "no_parens_expr - no_parens_many_expr" do
    test "call with multiple args" do
      # no_parens_many_expr -> dot_identifier call_args_no_parens_many_strict
      assert_conforms("foo 1, 2")
      assert_conforms("foo 1, 2, 3")
      assert_conforms("foo :a, :b, :c, :d")
    end

    test "call with keyword args" do
      # call_args_no_parens_many -> matched_expr ',' call_args_no_parens_kw
      assert_conforms("foo 1, a: 2")
      assert_conforms("foo 1, 'a': 2")
      assert_conforms("foo 1, 2, a: 3, b: 4")
      assert_conforms("foo 1, 2, a: 3, 'b': 4")
      assert_conforms("foo 1, 2, 'a': 3, b: 4")
      assert_conforms("foo 1, 2, 'a': 3, 'b': 4")

      # no_parens_expr as last item
      assert_conforms("foo 1, a: foo x, y")
      assert_conforms("foo 1, 'a': foo x, y")
      assert_conforms("foo 1, 2, a: 3, b: foo x, y")
      assert_conforms("foo 1, 2, a: 3, 'b': foo x, y")
      assert_conforms("foo 1, 2, 'a': 3, b: foo x, y")
      assert_conforms("foo 1, 2, 'a': 3, 'b': foo x, y")
    end

    test "dot call with multiple args" do
      assert_conforms("foo.bar 1, 2")
      assert_conforms("a.b.c 1, 2, 3")
    end

    test "op_identifier call with multiple args" do
      # no_parens_many_expr -> dot_op_identifier call_args_no_parens_many_strict
      assert_conforms("foo -1, 2")
      assert_conforms("foo +1, -2")

      assert_conforms("foo.bar -1, 2")
      assert_conforms("foo.bar +1, -2")
    end
  end

  # =============================================================================
  # STAB AND FN EXPRESSIONS
  # From elixir_parser.yrl lines 344-366
  # =============================================================================

  describe "stab expressions" do
    test "fn with stab clauses" do
      assert_conforms("fn -> :ok end")
      assert_conforms("fn x -> x end")
      assert_conforms("fn x, y -> x + y end")
      assert_conforms("fn x -> x\ny -> y end")
      assert_conforms("fn\nx -> x\ny -> y\nend")
    end

    test "fn with guards" do
      assert_conforms("fn x when is_integer(x) -> x end")
      assert_conforms("fn x when x > 0 -> :positive\nx -> :non_positive end")
    end

    test "fn with pattern matching" do
      assert_conforms("fn {a, b} -> a + b end")
      assert_conforms("fn %{key: value} -> value end")
      assert_conforms("fn %{'key': value} -> value end")
      assert_conforms("fn [h | t] -> {h, t} end")
    end

    test "parenthesized stab in fn" do
      assert_conforms("fn () -> :ok end")
      assert_conforms("fn () when true -> :ok end")
      assert_conforms("fn (a, b) -> a + b end")
      assert_conforms("fn (a, b) when a > b -> a end")
    end

    test "stab in parentheses" do
      assert_conforms("(->)")
      assert_conforms("(x -> x)")
      assert_conforms("(x, y -> x + y)")
      assert_conforms("(x when x > 0 -> x)")
    end

    test "multiple stab clauses in parentheses" do
      assert_conforms("(x -> x\ny -> y)")
      assert_conforms("(1 -> :one\n2 -> :two)")
    end
  end

  # =============================================================================
  # DO/END BLOCKS
  # From elixir_parser.yrl lines 320-329
  # =============================================================================

  describe "do_block structure" do
    test "empty do block" do
      assert_conforms("foo do\nend")
      assert_conforms("foo do end")
    end

    test "do block with single expression" do
      assert_conforms("foo do\n:ok\nend")
    end

    test "do block with multiple expressions" do
      assert_conforms("foo do\n1\n2\n3\nend")
    end

    test "do block with stab" do
      assert_conforms("foo do\nx -> x\nend")
      assert_conforms("foo do\nx -> x\ny -> y\nend")
    end

    test "do block with else" do
      assert_conforms("if true do\n:ok\nelse\n:error\nend")
    end

    test "do block with catch" do
      assert_conforms("try do\n:ok\ncatch\n:exit, _ -> :caught\nend")
    end

    test "do block with rescue" do
      assert_conforms("try do\n:ok\nrescue\ne in RuntimeError -> e\nend")
    end

    test "do block with after" do
      assert_conforms("try do\n:ok\nafter\n:cleanup\nend")
      assert_conforms("receive do\n:msg -> :ok\nafter\n1000 -> :timeout\nend")
    end

    test "do block with multiple clauses" do
      assert_conforms("try do\n:ok\nrescue\ne -> e\ncatch\n_ -> :caught\nafter\n:cleanup\nend")
    end
  end

  # =============================================================================
  # CONTAINERS
  # From elixir_parser.yrl lines 591-658
  # =============================================================================

  describe "containers - lists" do
    test "empty list" do
      assert_conforms("[]")
    end

    test "list with elements" do
      assert_conforms("[1]")
      assert_conforms("[1, 2]")
      assert_conforms("[1, 2, 3]")
    end

    test "list with trailing comma" do
      assert_conforms("[1,]")
      assert_conforms("[1, 2,]")
    end

    test "list with keyword tail" do
      assert_conforms("[a: 1]")
      assert_conforms("['a': 1]")
      assert_conforms("[a: 1, b: 2]")
      assert_conforms("['a': 1, b: 2]")
      assert_conforms("[a: 1, 'b': 2]")
      assert_conforms("[1, a: 1]")
      assert_conforms("[1, 'a': 1]")
      assert_conforms("[1, 2, a: 1, b: 2]")
      assert_conforms("[1, 2, 'a': 1, b: 2]")
      assert_conforms("[1, 2, a: 1, 'b': 2]")
    end

    test "list with keyword tail interpolated" do
      assert_conforms("[\"foo\#{}\": 1]")
      assert_conforms("[\"foo\#{bar}\": 1]")
      assert_conforms("[\"foo\#{l*e}\": 1]")
    end

    test "nested lists" do
      assert_conforms("[[1]]")
      assert_conforms("[[1], [2]]")
      assert_conforms("[[[1, 2]]]")
    end

    test "lists with charlist" do
      assert_conforms("['']")
    end
  end

  describe "containers - tuples" do
    test "empty tuple" do
      assert_conforms("{}")
    end

    test "single element tuple" do
      assert_conforms("{1}")
    end

    test "two element tuple" do
      assert_conforms("{1, 2}")
    end

    test "multi element tuple" do
      assert_conforms("{1, 2, 3}")
      assert_conforms("{1, 2, 3, 4, 5}")
    end

    test "nested tuples" do
      assert_conforms("{{1}}")
      assert_conforms("{{1, 2}, {3, 4}}")
    end

    test "tuple with trailing comma" do
      assert_conforms("{1,}")
      assert_conforms("{1, 2,}")
    end
  end

  describe "containers - maps" do
    test "empty map" do
      assert_conforms("%{}")
    end

    test "map with keyword syntax" do
      assert_conforms("%{a: 1}")
      assert_conforms("%{a: 1, b: 2}")
    end

    test "map with arrow syntax" do
      assert_conforms("%{:a => 1}")
      assert_conforms("%{1 => 2}")
      assert_conforms("%{\"key\" => \"value\"}")
    end

    test "map with mixed syntax" do
      assert_conforms("%{:a => 1, b: 2}")
      assert_conforms("%{1 => :one, a: :a}")
    end

    test "map update syntax" do
      assert_conforms("%{map | a: 1}")
      assert_conforms("%{map | a: 1, b: 2}")
      assert_conforms("%{map | :a => 1}")
    end

    test "nested maps" do
      assert_conforms("%{a: %{b: 1}}")
      assert_conforms("%{outer: %{inner: %{deep: :value}}}")
    end
  end

  describe "containers - structs" do
    test "empty struct" do
      assert_conforms("%Foo{}")
    end

    test "struct with fields" do
      assert_conforms("%Foo{a: 1}")
      assert_conforms("%Foo{a: 1, b: 2}")
    end

    test "struct update" do
      assert_conforms("%Foo{struct | a: 1}")
      assert_conforms("%Foo{struct | a: 1, b: 2}")
    end

    test "nested struct alias" do
      assert_conforms("%Foo.Bar{}")
      assert_conforms("%Foo.Bar.Baz{a: 1}")
      assert_conforms("%Foo.Bar.Baz{'a': 1}")
    end

    test "struct with dynamic name" do
      assert_conforms("%foo{}")
      assert_conforms("%foo{'a': 1}")
      assert_conforms("%@module{}")
    end
  end

  describe "containers - bitstrings" do
    test "empty bitstring" do
      assert_conforms("<<>>")
    end

    test "bitstring with values" do
      assert_conforms("<<1>>")
      assert_conforms("<<1, 2, 3>>")
    end

    test "bitstring with size modifier" do
      assert_conforms("<<x::8>>")
      assert_conforms("<<x::size(8)>>")
    end

    test "bitstring with type modifier" do
      assert_conforms("<<x::binary>>")
      assert_conforms("<<x::integer>>")
      assert_conforms("<<x::float>>")
      assert_conforms("<<x::bits>>")
      assert_conforms("<<x::bitstring>>")
      assert_conforms("<<x::utf8>>")
      assert_conforms("<<x::utf16>>")
      assert_conforms("<<x::utf32>>")
    end

    test "bitstring with multiple modifiers" do
      assert_conforms("<<x::size(8)-integer-unsigned-big>>")
      assert_conforms("<<x::8-little>>")
    end

    test "bitstring with binary segments" do
      assert_conforms("<<rest::binary>>")
      assert_conforms("<<a::8, rest::binary>>")
    end
  end

  # =============================================================================
  # PRECEDENCE AND ASSOCIATIVITY
  # From elixir_parser.yrl lines 69-97
  # =============================================================================

  describe "operator precedence" do
    test "multiplication binds tighter than addition" do
      assert_conforms("1 + 2 * 3")
      assert_conforms("1 * 2 + 3")
    end

    test "power binds tighter than multiplication" do
      assert_conforms("2 ** 3 * 4")
      assert_conforms("2 * 3 ** 4")
    end

    test "unary binds tighter than binary" do
      assert_conforms("-1 + 2")
      assert_conforms("1 + -2")
      assert_conforms("!a && b")
      assert_conforms("not a and b")
    end

    test "comparison chains" do
      assert_conforms("1 < 2 and 2 < 3")
      assert_conforms("a == b && c != d")
    end

    test "pipe chains" do
      assert_conforms("a |> b |> c")
      assert_conforms("1 |> foo() |> bar()")
      assert_conforms("1 |> if x do\n:ok\nend |> bar 123")
    end

    test "match is right-associative" do
      assert_conforms("a = b = c")
      assert_conforms("1 = x = 2")
    end

    test "concat is right-associative" do
      assert_conforms("a ++ b ++ c")
      assert_conforms("a <> b <> c")
    end

    test "or and and precedence" do
      assert_conforms("a && b || c")
      assert_conforms("a || b && c")
      assert_conforms("a and b or c")
      assert_conforms("a or b and c")
    end

    test "range and step operator" do
      assert_conforms("1..10")
      assert_conforms("1..10//2")
      assert_conforms("a..b//c")
    end

    test "in operator with not" do
      assert_conforms("a in b")
      assert_conforms("a not in b")
      # Note: `not a in b` is deprecated and rewrites
    end

    test "when operator" do
      assert_conforms("x when is_integer(x)")
      assert_conforms("x when x > 0 and x < 10")
    end

    test "type annotation" do
      assert_conforms("x :: integer")
      assert_conforms("x :: integer | atom")
      assert_conforms("{a, b} :: {integer, atom}")
    end
  end

  # =============================================================================
  # DOT EXPRESSIONS
  # From elixir_parser.yrl lines 472-498
  # =============================================================================

  describe "dot expressions" do
    test "dot identifier" do
      assert_conforms("foo.bar")
      assert_conforms("foo.bar.baz")
    end

    test "dot alias" do
      assert_conforms("foo.Bar")
      assert_conforms("foo.Bar.Baz")
      assert_conforms("Foo.Bar")
      assert_conforms("Foo.Bar.Baz")
    end

    test "dot call" do
      assert_conforms("foo.bar()")
      assert_conforms("foo.bar(1)")
      assert_conforms("foo.bar(1, 2)")
    end

    test "dot call with remote module" do
      assert_conforms("Foo.bar()")
      assert_conforms("Foo.Bar.baz()")
    end

    test "dot op identifier" do
      # dot_op_identifier for operators used as functions
      assert_conforms("foo.+")
      assert_conforms("Kernel.+")
      assert_conforms("foo.+(1, 2)")
    end

    test "dot bracket identifier" do
      assert_conforms("foo.bar[0]")
      assert_conforms("foo.bar[:key]")
    end

    test "dot do identifier" do
      assert_conforms("foo.bar do\n:ok\nend")
    end

    test "dot dangling operator errors" do
      assert_conforms("foo.")
    end

    test "dot with tuple" do
      # dot_alias -> matched_expr dot_op open_curly container_args close_curly
      assert_conforms("foo.{A, B}")
      assert_conforms("foo.{A, B, C}")
    end

    test "anonymous function call" do
      # dot_call_identifier -> matched_expr dot_call_op
      assert_conforms("foo.()")
      assert_conforms("-foo.()")
      assert_conforms("foo.(1)")
      assert_conforms("foo.(1, 2)")
    end

    test "quoted identifier" do
      # identifier
      assert_conforms("D.\"foo\"")
      # op_identifier
      assert_conforms("D.\"foo\" -1")
      # parens_identifier
      assert_conforms("D.\"foo\"()")
      # bracket_identifier
      assert_conforms("D.\"foo\"[1]")
      # do_identifier
      assert_conforms("D.\"foo\" do\n\:ok\nend")
    end
  end

  # =============================================================================
  # KEYWORD ARGUMENTS
  # From elixir_parser.yrl lines 564-589
  # =============================================================================

  describe "keyword arguments" do
    test "keyword in function call" do
      assert_conforms("foo(a: 1)")
      assert_conforms("foo('a': 1)")
      assert_conforms("foo(a: 1, b: 2)")
      assert_conforms("foo('a': 1, b: 2)")
      assert_conforms("foo(a: 1, 'b': 2)")
      assert_conforms("foo('a': 1, 'b': 2)")
      assert_conforms("foo(1, a: 2)")
      assert_conforms("foo(1, 'a': 2)")
      assert_conforms("foo(1, 2, a: 3, b: 4)")
      assert_conforms("foo(1, 2, 'a': 3, b: 4)")
      assert_conforms("foo(1, 2, a: 3, 'b': 4)")
      assert_conforms("foo(1, 2, 'a': 3, 'b': 4)")
    end

    test "keyword in no-parens call" do
      assert_conforms("foo a: 1")
      assert_conforms("foo a: 1, b: 2")
      assert_conforms("foo 1, a: 2")
    end

    test "keyword with quoted keys" do
      assert_conforms(~S[foo("a": 1)])
      assert_conforms(~S[foo('a': 1)])
    end

    test "keyword values with quoted keys allow blocks" do
      assert_conforms(~S[foo("a": a do end)])
      assert_conforms(~S(["a": a do end]))
    end

    test "keyword trailing comma (warning)" do
      # warn_trailing_comma in parser
      assert_conforms("foo(a: 1,)")
    end
  end

  describe "additional valid edge combinations" do
    test "stab_parens_many with guards and nesting" do
      assert_conforms("fn (a, b, c: 1) when guard -> body end")
      assert_conforms("fn (a, b, 'c': 1) when guard -> body end")
      assert_conforms("fn (a, b, 'c\#{p}': 1) when guard -> body end")
      assert_conforms("fn (a, b, c: 1, 'd': 1) when guard -> body end")
      assert_conforms("fn (a, b, 'c': 1, d: 1) when guard -> body end")
      assert_conforms("fn ((nested)) -> body end")
      assert_conforms("fn ({:ok, x}, {:error, y}) when x > y -> x end")
    end

    test "nested no-parens keyword values" do
      assert_conforms("foo a: bar 1, 2")
      assert_conforms("foo 'a': bar 1, 2")
      assert_conforms("foo a: 0, b: bar 1, 2")
      assert_conforms("foo 'a': 0, b: bar 1, 2")
      assert_conforms("foo a: 0, 'b': bar 1, 2")
      assert_conforms("foo 'a': 0, 'b': bar 1, 2")

      assert_conforms("foo a: bar 1, 2, b: baz 3")
      assert_conforms("foo a: bar 1, 2, 'b': baz 3")
      assert_conforms("foo a: bar 1, 2, b: x, c: baz 3")
      assert_conforms("foo a: bar 1, 2, b: x, 'c': baz 3")
      assert_conforms("foo a: bar 1, 2, 'b': x, c: baz 3")
      assert_conforms("foo a: bar 1, 2, b: x, 'c': baz 3")

      assert_conforms("if a: bar 1, 2 do\nend")
      assert_conforms("if 'a': bar 1, 2 do\nend")
      assert_conforms("if a: 0, b: bar 1, 2 do\nend")
      assert_conforms("if 'a': 0, b: bar 1, 2 do\nend")
      assert_conforms("if a: 0, 'b': bar 1, 2 do\nend")
      assert_conforms("if 'a': 0, 'b': bar 1, 2 do\nend")
    end

    test "arrow ops with no-parens rhs" do
      assert_conforms("a ~> foo 1")
      assert_conforms("a <~ foo 1")
      assert_conforms("a <<< foo 1")
      assert_conforms("a >>> foo 1")
      assert_conforms("a <~> foo 1")
    end

    test "map update bases and operators" do
      assert_conforms("%{foo() | a => b}")
      assert_conforms("%{Mod.func() | a => b}")
      assert_conforms("%{@attr | a: 1}")
      assert_conforms("%{x.y.z | a: 1}")
      assert_conforms("%{if foo do bar end | a: 1}")
    end

    test "dot_container variants" do
      assert_conforms("Foo.{A, b: 1}")
      assert_conforms("Foo.{A, 'b': 1}")
      assert_conforms("Foo.{if a do b end}")
      assert_conforms("Foo.{A, B, c: 1, d: 2}")
      assert_conforms("Foo.{A, B, 'c': 1, d: 2}")
      assert_conforms("Foo.{A, B, c: 1, 'd': 2}")
      assert_conforms("Foo.{A, B, 'c': 1, 'd': 2}")
      assert_conforms("foo.bar.{A, B, C}")
      assert_conforms("1.{A}")
      assert_conforms("(a + b).{C}")
    end

    test "expanded quoted identifiers" do
      assert_conforms(~S[foo."bar baz"()])
      assert_conforms("foo.\"bar\\nbaz\" 1, 2")
      assert_conforms(~S(foo."bar"[:key]))
      assert_conforms("foo.\"bar\" do\nend")
      assert_conforms(~S[&"func"/1])
    end

    test "bitstring modifiers" do
      assert_conforms("<<x::size(if a do 8 else 16 end)>>")
      assert_conforms("<<x::size(foo 1, 2)>>")
      assert_conforms("<<x::(if a do binary else integer end)>>")
      assert_conforms("<<@attr::binary>>")
      assert_conforms("<<foo()::8-unit(4)>>")
    end

    test "containers with unmatched expressions" do
      assert_conforms("[if a do 1 end, if b do 2 end]")
      assert_conforms("{if a do 1 end, if b do 2 end, if c do 3 end}")
      assert_conforms("%{if a do :key end => if b do :val end}")
      assert_conforms("<<if a do 1 end>>")
    end

    test "keyword block forms" do
      assert_conforms("if true, do: :ok")
      assert_conforms("if true, do: :ok, else: :error")
      assert_conforms("with {:ok, x} <- foo(), do: x, else: (_ -> :error)")
    end

    test "block list combinations" do
      assert_conforms(
        "try do\n:ok\nrescue e -> e\ncatch :throw, x -> x\nelse _ -> :default\nafter :cleanup\nend"
      )

      assert_conforms("receive do\n:msg -> :ok\nafter 100 -> :timeout\nend")

      assert_conforms(
        "with {:ok, a} <- foo(), {:ok, b} <- bar() do\n{a, b}\nelse {:error, e} -> e\nend"
      )
    end

    test "stab_eoe annotations" do
      assert_conforms("fn x -> x; y -> y end")
      assert_conforms("(fn x -> x; y -> y end)")
    end

    test "dot_call newline boundaries" do
      assert_conforms("foo.\n()")
      assert_conforms("foo.\n(1, 2)")
    end

    test "sigil delimiters" do
      assert_conforms("~s(foo)")
      assert_conforms("~s[foo]")
      assert_conforms("~s{foo}")
      assert_conforms("~s<foo>")
      assert_conforms("~s/foo/")
      assert_conforms("~s|foo|")
      assert_conforms("~s'foo'")
    end

    test "unicode identifiers and atoms" do
      assert_conforms("fóó()")
      assert_conforms(":bår")
      assert_conforms("fn fóó -> fóó end")
    end

    test "operator edge cases" do
      assert_conforms("& &1 + &2")
      assert_conforms("x <- y")
      assert_conforms("x \\\\ y")
      assert_conforms("1..10//2 + 3")
    end
  end

  test "ternary after range" do
    assert_conforms("x..0;//y")
  end

  test "bracket on alias" do
    assert_conforms("x = A[d]")
    assert_conforms("&A[d]")
    assert_conforms("[\"foo\#{A[d]}\": 1]")
    assert_conforms("A[d]..x//y")
    assert_conforms("~s\"\"\"\nfoo\#{A[d]}\n\"\"\"")
    assert_conforms("%{x | foo: A[d]}")
    assert_conforms("def foo() when A[d] do 1 end")
  end

  test "ellipsis ternary" do
    assert_conforms("x...;//y")
  end

  test "no_parens call on block" do
    assert_conforms("foo.e ()")
  end

  test "interpolation repro 1" do
    assert_conforms("'''\nfoo\#{t;..<e}\n'''")
  end

  test "naked ellipsis" do
    assert_conforms("def foo() when ... do 1 end")
    assert_conforms(":\"foo\#{...}\"")
  end

  test "when keyword in stab" do
    # no_parens_op_expr -> when_op_eol call_args_no_parens_kw
    # stab_expr -> expr
    assert_conforms("(x when y: z, z: w)")
    # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
    assert_conforms("(() when x when y: z, z: w -> 0)")
    # stab_parens_many when_op expr stab_op_eol_and_expr
    assert_conforms("((a) when x when y: z, z: w -> 0)")
    assert_conforms("((a, b) when x when y: z, z: w -> 0)")

    # stab_expr -> stab_op_eol_and_expr
    assert_conforms("(-> x when y: z, z: w)")
    # stab_expr -> empty_paren stab_op_eol_and_expr
    assert_conforms("(() -> x when y: z, z: w)")
    # stab_expr -> empty_paren stab_op_eol_and_expr
    assert_conforms("(() -> x when y: z, z: w)")
    # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
    assert_conforms("(() when foo -> x when y: z, z: w)")
    assert_conforms("(() when a when foo: 1 -> x when y: z, z: w)")
    # stab_expr -> call_args_no_parens_all stab_op_eol_and_expr
    assert_conforms("(a -> x when y: z, z: w)")
    assert_conforms("(a, b -> x when y: z, z: w)")
    # stab_expr -> stab_parens_many stab_op_eol_and_expr
    assert_conforms("((a) -> x when y: z, z: w)")
    assert_conforms("((a, b) -> x when y: z, z: w)")
    # stab_parens_many when_op expr stab_op_eol_and_expr
    assert_conforms("((a) when 1 -> x when y: z, z: w)")
    assert_conforms("((a, b) when 1 -> x when y: z, z: w)")
    assert_conforms("((a) when foo: 1 -> x when y: z, z: w)")
  end

  test "when keyword in fn" do
    # no_parens_op_expr -> when_op_eol call_args_no_parens_kw
    # stab_expr -> expr - forbidden error case
    # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
    assert_conforms("fn () when x when y: z, z: w -> 0 end")
    # stab_parens_many when_op expr stab_op_eol_and_expr
    assert_conforms("fn (a) when x when y: z, z: w -> 0 end")
    assert_conforms("fn (a, b) when x when y: z, z: w -> 0 end")

    # stab_expr -> stab_op_eol_and_expr
    assert_conforms("fn -> x when y: z, z: w end")
    # stab_expr -> empty_paren stab_op_eol_and_expr
    assert_conforms("fn () -> x when y: z, z: w end")
    # stab_expr -> empty_paren stab_op_eol_and_expr
    assert_conforms("fn () -> x when y: z, z: w end")
    # stab_expr -> empty_paren when_op expr stab_op_eol_and_expr
    assert_conforms("fn () when foo -> x when y: z, z: w end")
    assert_conforms("fn () when a when foo: 1 -> x when y: z, z: w end")
    # stab_expr -> call_args_no_parens_all stab_op_eol_and_expr
    assert_conforms("fn a -> x when y: z, z: w end")
    assert_conforms("fn a, b -> x when y: z, z: w end")
    # stab_expr -> stab_parens_many stab_op_eol_and_expr
    assert_conforms("fn (a) -> x when y: z, z: w end")
    assert_conforms("fn (a, b) -> x when y: z, z: w end")
    # stab_parens_many when_op expr stab_op_eol_and_expr
    assert_conforms("fn (a) when 1 -> x when y: z, z: w end")
    assert_conforms("fn (a, b) when 1 -> x when y: z, z: w end")
    assert_conforms("fn (a) when foo: 1 -> x when y: z, z: w end")
  end

  test "when keyword in do-block" do
    assert_conforms("a do x when y: z, z: w end")
    assert_conforms("if a do 1 else x when y: z, z: w end")
  end

  test "when keyword in call_args_no_parens_kw" do
    assert_conforms("a foo: x when y: z, z: w when q1: m, q2: n")

    assert_conforms("a foo: x when y: z, z: w")
    assert_conforms("a 1, 2, foo: x when y: z, z: w")
    assert_conforms("a 1, 2, foo: 0, bar: x when y: z, z: w")
    assert_conforms("a 1, 2, foo: x when y: z, z: w do :ok end")

    assert_conforms("((foo: x when y: z, z: w) -> 1)")
    assert_conforms("((a, foo: x when y: z, z: w) -> 1)")
  end

  test "ternary combinations" do
    assert_conforms("// foo do ... end ** 2")
    assert_conforms("// foo() ** 2")
  end

  test "newlines with unary" do
    assert_conforms("foo \n;")
    assert_conforms("^!u\n;")
    assert_conforms("foo()\n;")
  end

  # =============================================================================
  # Helper function
  # =============================================================================

  defp assert_conforms(code, _opts \\ []) do
    reference = s2q(code)

    case reference do
      {:ok, expected_ast} ->
        actual = spitfire_parse(code)

        assert actual == reference,
               """
               AST mismatch for: #{inspect(code)}

               Reference:
               #{inspect(expected_ast, pretty: true)}

               Actual:
               #{inspect(actual, pretty: true)}
               """

      {:error, _} ->
        # Reference parser errors - skip, nothing to validate conformance against
        :ok
    end
  end

  defp s2q(code) do
    Code.string_to_quoted(
      code,
      columns: true,
      token_metadata: true,
      emit_warnings: false
    )
  end

  defp spitfire_parse(code) do
    case Spitfire.parse(code) do
      {:ok, ast} -> {:ok, ast}
      {:error, _ast, _errors} -> {:error, :parse_error}
      {:error, :no_fuel_remaining} -> {:error, :no_fuel_remaining}
    end
  end
end
