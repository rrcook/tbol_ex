defmodule Tbol.LexerTest do
  use ExUnit.Case, async: true

  alias Tbol.Lexer

  describe "tokenize/1" do
    test "tokenizes simple program structure" do
      source = "PROGRAM test; PROC main = END_PROC"

      assert {:ok, tokens} = Lexer.tokenize(source)

      assert [
               {:keyword, :PROGRAM, {1, 1}},
               {:identifier, "test", {1, 9}},
               {:semicolon, nil, {1, 13}},
               {:keyword, :PROC, {1, 15}},
               {:identifier, "main", {1, 20}},
               {:op, :=, {1, 25}},
               {:keyword, :END_PROC, {1, 27}},
               {:eof, nil, {0, 0}}
             ] = tokens
    end

    test "tokenizes integer literals" do
      assert {:ok, tokens} = Lexer.tokenize("42 123 0")

      assert [
               {:integer, 42, {1, 1}},
               {:integer, 123, {1, 4}},
               {:integer, 0, {1, 8}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes string literals" do
      assert {:ok, tokens} = Lexer.tokenize("'hello world'")

      assert [
               {:string, "hello world", {1, 1}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes string with hex escape" do
      assert {:ok, tokens} = Lexer.tokenize("'line1\\x0d\\x0aline2'")

      assert [
               {:string, "line1\r\nline2", {1, 1}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes hex literals" do
      assert {:ok, tokens} = Lexer.tokenize("0x0d0a 0xFF")

      assert [
               {:hex, <<13, 10>>, {1, 1}},
               {:hex, <<255>>, {1, 8}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes screen fields" do
      assert {:ok, tokens} = Lexer.tokenize("&1 &25 &100")

      assert [
               {:screen_field, {1, nil}, {1, 1}},
               {:screen_field, {25, nil}, {1, 4}},
               {:screen_field, {100, nil}, {1, 8}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes screen field with subscript" do
      assert {:ok, tokens} = Lexer.tokenize("&1(I6)")

      assert [
               {:screen_field, {1, [{:i_register, 6}]}, {1, 1}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes integer registers" do
      assert {:ok, tokens} = Lexer.tokenize("I1 I8 i4")

      assert [
               {:i_register, 1, {1, 1}},
               {:i_register, 8, {1, 4}},
               {:i_register, 4, {1, 7}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes decimal registers" do
      assert {:ok, tokens} = Lexer.tokenize("D1 D8 d4")

      assert [
               {:d_register, 1, {1, 1}},
               {:d_register, 8, {1, 4}},
               {:d_register, 4, {1, 7}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes parameter registers" do
      assert {:ok, tokens} = Lexer.tokenize("P0 P1 P8")

      assert [
               {:p_register, 0, {1, 1}},
               {:p_register, 1, {1, 4}},
               {:p_register, 8, {1, 7}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes register with subscript" do
      assert {:ok, tokens} = Lexer.tokenize("I1(I8)")

      assert [
               {:i_register, {1, [{:i_register, 8}]}, {1, 1}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes RDA references" do
      assert {:ok, tokens} = Lexer.tokenize("RDA0 RDA118")

      assert [
               {:rda, 0, {1, 1}},
               {:rda, 118, {1, 6}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes GEV references" do
      assert {:ok, tokens} = Lexer.tokenize("SYS_RETURN_CODE PRF_USER_ID")

      assert [
               {:sys_gev, "SYS_RETURN_CODE", {1, 1}},
               {:prf_gev, "PRF_USER_ID", {1, 17}},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes comparison operators" do
      assert {:ok, tokens} = Lexer.tokenize("= <> < > <= >= =>")

      assert [
               {:op, :=, _},
               {:op, :<>, _},
               {:op, :<, _},
               {:op, :>, _},
               {:op, :<=, _},
               {:op, :>=, _},
               {:op, :>=, _},  # => is alias for >=
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes label definition" do
      assert {:ok, tokens} = Lexer.tokenize("my_label:")

      assert [
               {:label_def, "my_label", {1, 1}},
               {:eof, _, _}
             ] = tokens
    end

    test "skips comments" do
      assert {:ok, tokens} = Lexer.tokenize("MOVE {this is a comment} I1")

      assert [
               {:keyword, :MOVE, {1, 1}},
               {:i_register, 1, {1, 26}},
               {:eof, _, _}
             ] = tokens
    end

    test "handles multiline comments" do
      source = """
      MOVE { this is
      a multiline
      comment } I1
      """

      assert {:ok, tokens} = Lexer.tokenize(source)

      assert [
               {:keyword, :MOVE, _},
               {:i_register, 1, _},
               {:eof, _, _}
             ] = tokens
    end

    test "tokenizes keywords" do
      keywords = ~w(IF THEN ELSE DO END WHILE GOTO MOVE ADD SUBTRACT MULTIPLY DIVIDE)

      for keyword <- keywords do
        assert {:ok, [{:keyword, atom, _} | _]} = Lexer.tokenize(keyword)
        assert atom == String.to_atom(keyword)
      end
    end

    test "handles unterminated string" do
      assert {:error, {:unterminated_string, _, _}} = Lexer.tokenize("'hello")
    end
  end
end
