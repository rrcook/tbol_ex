defmodule Tbol.ParserTest do
  use ExUnit.Case, async: true

  alias Tbol.{Lexer, Parser}
  alias Tbol.AST

  defp parse(source) do
    with {:ok, tokens} <- Lexer.tokenize(source) do
      Parser.parse(tokens)
    end
  end

  describe "parse/1 - program structure" do
    test "parses minimal program" do
      source = "PROGRAM test; PROC main = END_PROC"

      assert {:ok, %AST.Program{name: "test", procedures: [%AST.Procedure{name: "main", statements: []}]}} =
               parse(source)
    end

    test "parses DEFINE declarations" do
      source = """
      PROGRAM test;
      DEFINE MY_CONST, 42;
      DEFINE MY_STRING, 'hello';
      PROC main = END_PROC
      """

      assert {:ok, %AST.Program{defines: defines}} = parse(source)
      assert length(defines) == 2
      assert %AST.Define{name: "MY_CONST", value: %AST.Literal{value: 42}} = hd(defines)
    end

    test "parses COPY declarations" do
      source = """
      PROGRAM test;
      COPY MYLIB;
      PROC main = END_PROC
      """

      assert {:ok, %AST.Program{copies: [%AST.Copy{name: "MYLIB"}]}} = parse(source)
    end

    test "parses multiple procedures" do
      source = """
      PROGRAM test;
      PROC main = END_PROC
      PROC helper = END_PROC
      """

      assert {:ok, %AST.Program{procedures: procs}} = parse(source)
      assert length(procs) == 2
      assert Enum.map(procs, & &1.name) == ["main", "helper"]
    end
  end

  describe "parse/1 - data statements" do
    test "parses MOVE statement" do
      source = "PROGRAM t; PROC main = MOVE I1, I2; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.Move{
               source: %AST.Register{type: :i, number: 1},
               destination: %AST.Register{type: :i, number: 2},
               abs: false
             } = stmt
    end

    test "parses MOVE with ABS" do
      source = "PROGRAM t; PROC main = MOVE I1, I2, ABS; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Move{abs: true} = stmt
    end

    test "parses CLEAR statement" do
      source = "PROGRAM t; PROC main = CLEAR I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Clear{destination: %AST.Register{type: :i, number: 1}} = stmt
    end
  end

  describe "parse/1 - arithmetic statements" do
    test "parses ADD statement" do
      source = "PROGRAM t; PROC main = ADD 5, I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.Add{
               value: %AST.Literal{value: 5},
               destination: %AST.Register{type: :i, number: 1}
             } = stmt
    end

    test "parses SUBTRACT statement" do
      source = "PROGRAM t; PROC main = SUBTRACT I2, I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Subtract{} = stmt
    end

    test "parses MULTIPLY statement" do
      source = "PROGRAM t; PROC main = MULTIPLY 2, I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Multiply{} = stmt
    end

    test "parses DIVIDE with remainder" do
      source = "PROGRAM t; PROC main = DIVIDE 3, I1, I2; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.Divide{
               value: %AST.Literal{value: 3},
               destination: %AST.Register{type: :i, number: 1},
               remainder: %AST.Register{type: :i, number: 2}
             } = stmt
    end
  end

  describe "parse/1 - control flow" do
    test "parses IF-THEN statement" do
      source = "PROGRAM t; PROC main = IF I1 = 0 THEN CLEAR I2; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.If{
               condition: %AST.BinaryOp{op: :=, left: _, right: _},
               then_clause: %AST.Clear{},
               else_clause: nil
             } = stmt
    end

    test "parses IF-THEN-ELSE statement" do
      source = "PROGRAM t; PROC main = IF I1 = 0 THEN CLEAR I2; ELSE CLEAR I3; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.If{else_clause: %AST.Clear{}} = stmt
    end

    test "parses IF with DO block" do
      source = """
      PROGRAM t;
      PROC main =
        IF I1 = 0 THEN DO
          CLEAR I2;
          CLEAR I3;
        END;
      END_PROC
      """

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.If{then_clause: %AST.DoBlock{statements: stmts}} = stmt
      assert length(stmts) == 2
    end

    test "parses compound condition with AND" do
      source = "PROGRAM t; PROC main = IF I1 = 0 AND I2 = 0 THEN CLEAR I3; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.If{
               condition: %AST.BinaryOp{
                 op: :and,
                 left: %AST.BinaryOp{op: :=},
                 right: %AST.BinaryOp{op: :=}
               }
             } = stmt
    end

    test "parses WHILE loop" do
      source = """
      PROGRAM t;
      PROC main =
        WHILE I1 < 10 DO
          ADD 1, I1;
        END;
      END_PROC
      """

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.While{
               condition: %AST.BinaryOp{op: :<},
               body: [%AST.Add{}]
             } = stmt
    end

    test "parses GOTO statement" do
      source = "PROGRAM t; PROC main = GOTO my_label; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Goto{label: "my_label"} = stmt
    end

    test "parses GOTO_DEPENDING_ON" do
      source = "PROGRAM t; PROC main = GOTO_DEPENDING_ON I1, label1, label2, label3; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.GotoDependingOn{
               index: %AST.Register{type: :i, number: 1},
               labels: ["label1", "label2", "label3"]
             } = stmt
    end

    test "parses label definition" do
      source = "PROGRAM t; PROC main = my_label: CLEAR I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: stmts}]}} = parse(source)
      assert [%AST.Label{name: "my_label"}, %AST.Clear{}] = stmts
    end
  end

  describe "parse/1 - string operations" do
    test "parses STRING statement" do
      source = "PROGRAM t; PROC main = STRING &1, 'hello', ' ', 'world'; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.StringOp{
               destination: %AST.ScreenField{number: 1},
               sources: sources
             } = stmt

      assert length(sources) == 3
    end

    test "parses SUBSTR statement" do
      source = "PROGRAM t; PROC main = SUBSTR &1, 1, 5, &2; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.Substr{
               source: %AST.ScreenField{number: 1},
               start: %AST.Literal{value: 1},
               length: %AST.Literal{value: 5},
               destination: %AST.ScreenField{number: 2}
             } = stmt
    end

    test "parses INSTR statement" do
      source = "PROGRAM t; PROC main = INSTR &1, ',', I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Instr{search: %AST.Literal{value: ","}} = stmt
    end

    test "parses LENGTH statement" do
      source = "PROGRAM t; PROC main = LENGTH &1, I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Length{} = stmt
    end

    test "parses UPPERCASE statement" do
      source = "PROGRAM t; PROC main = UPPERCASE &1, &2; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Uppercase{} = stmt
    end
  end

  describe "parse/1 - procedure calls" do
    test "parses LINK statement" do
      source = "PROGRAM t; PROC main = LINK helper; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Link{target: %AST.Identifier{name: "helper"}, params: []} = stmt
    end

    test "parses LINK with parameters" do
      source = "PROGRAM t; PROC main = LINK helper, 'arg1', I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Link{params: params} = stmt
      assert length(params) == 2
    end

    test "parses RETURN statement" do
      source = "PROGRAM t; PROC main = RETURN; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Return{} = stmt
    end

    test "parses EXIT statement" do
      source = "PROGRAM t; PROC main = EXIT; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Exit{} = stmt
    end
  end

  describe "parse/1 - expressions" do
    test "parses screen field with subscript" do
      source = "PROGRAM t; PROC main = CLEAR &1(I6); END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.Clear{
               destination: %AST.ScreenField{
                 number: 1,
                 subscript: %AST.Register{type: :i, number: 6}
               }
             } = stmt
    end

    test "parses register indirection" do
      source = "PROGRAM t; PROC main = MOVE I1(I8), I2; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)

      assert %AST.Move{
               source: %AST.Register{type: :i, number: 1, subscript: %AST.Register{type: :i, number: 8}}
             } = stmt
    end

    test "parses RDA reference" do
      source = "PROGRAM t; PROC main = MOVE RDA0, &1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Move{source: %AST.RdaRef{slot: 0}} = stmt
    end

    test "parses GEV reference" do
      source = "PROGRAM t; PROC main = MOVE SYS_RETURN_CODE, I1; END_PROC"

      assert {:ok, %AST.Program{procedures: [%AST.Procedure{statements: [stmt]}]}} = parse(source)
      assert %AST.Move{source: %AST.GevRef{name: "SYS_RETURN_CODE"}} = stmt
    end
  end
end
