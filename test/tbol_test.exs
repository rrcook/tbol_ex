defmodule TbolTest do
  use ExUnit.Case, async: true

  alias Tbol
  alias Tbol.Runtime.State

  describe "run/1 API" do
    test "runs simple program" do
      source = """
      PROGRAM simple;
      PROC main =
        MOVE 42, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 42
    end

    test "returns error for invalid syntax" do
      source = "INVALID SYNTAX"
      assert {:error, _} = Tbol.run(source)
    end
  end

  describe "tokenize/1 API" do
    test "returns tokens" do
      source = "PROGRAM test; PROC main = END_PROC"
      assert {:ok, tokens} = Tbol.tokenize(source)
      assert is_list(tokens)
    end
  end

  describe "parse/1 API" do
    test "returns AST" do
      source = "PROGRAM test; PROC main = END_PROC"
      assert {:ok, %Tbol.AST.Program{}} = Tbol.parse(source)
    end
  end
end
