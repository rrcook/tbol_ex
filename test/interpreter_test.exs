defmodule Tbol.InterpreterTest do
  use ExUnit.Case, async: true

  alias Tbol
  alias Tbol.Runtime.State

  describe "run/2 - data operations" do
    test "MOVE copies integer value" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 42, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 42
    end

    test "MOVE copies string to screen field" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'hello', &1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_pev(state, 1) == "hello"
    end

    test "MOVE with ABS takes absolute value" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE -42, I1;
        MOVE I1, I2, ABS;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == -42
      assert State.get_i(state, 2) == 42
    end

    test "CLEAR sets integer register to zero" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 42, I1;
        CLEAR I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 0
    end

    test "CLEAR sets screen field to empty string" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'hello', &1;
        CLEAR &1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_pev(state, 1) == ""
    end
  end

  describe "run/2 - arithmetic operations" do
    test "ADD increments destination" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 10, I1;
        ADD 5, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 15
    end

    test "SUBTRACT decrements destination" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 10, I1;
        SUBTRACT 3, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 7
    end

    test "MULTIPLY multiplies destination" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 6, I1;
        MULTIPLY 7, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 42
    end

    test "DIVIDE with remainder" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 17, I1;
        DIVIDE 5, I1, I2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 3  # quotient
      assert State.get_i(state, 2) == 2  # remainder
    end
  end

  describe "run/2 - control flow" do
    test "IF-THEN executes when true" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 0, I1;
        IF I1 = 0 THEN MOVE 1, I2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 2) == 1
    end

    test "IF-THEN skips when false" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 1, I1;
        IF I1 = 0 THEN MOVE 1, I2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 2) == 0
    end

    test "IF-ELSE executes else branch" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 1, I1;
        IF I1 = 0 THEN MOVE 1, I2; ELSE MOVE 2, I2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 2) == 2
    end

    test "IF with compound AND condition" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 5, I1;
        MOVE 10, I2;
        IF I1 = 5 AND I2 = 10 THEN MOVE 1, I3;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 3) == 1
    end

    test "IF with compound OR condition" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 5, I1;
        MOVE 20, I2;
        IF I1 = 5 OR I2 = 10 THEN MOVE 1, I3;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 3) == 1
    end

    test "WHILE loop executes until condition is false" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 0, I1;
        WHILE I1 < 5 DO
          ADD 1, I1;
        END;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 5
    end

    test "GOTO jumps to label" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 1, I1;
        GOTO skip;
        MOVE 2, I1;
      skip:
        MOVE 3, I2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 1
      assert State.get_i(state, 2) == 3
    end

    test "GOTO_DEPENDING_ON selects correct label" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 2, I1;
        GOTO_DEPENDING_ON I1, first, second, third;
        GOTO done;
      first:
        MOVE 1, I2;
        GOTO done;
      second:
        MOVE 2, I2;
        GOTO done;
      third:
        MOVE 3, I2;
      done:
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 2) == 2
    end
  end

  describe "run/2 - string operations" do
    test "STRING concatenates values" do
      source = """
      PROGRAM test;
      PROC main =
        STRING &1, 'hello', ' ', 'world';
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_pev(state, 1) == "hello world"
    end

    test "SUBSTR extracts substring" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'hello world', &1;
        SUBSTR &1, 7, 5, &2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_pev(state, 2) == "world"
    end

    test "INSTR finds substring position" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'hello,world', &1;
        INSTR &1, ',', I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 6
    end

    test "INSTR returns 0 when not found" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'hello world', &1;
        INSTR &1, ',', I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 0
    end

    test "LENGTH returns string length" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'hello', &1;
        LENGTH &1, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 5
    end

    test "UPPERCASE converts to uppercase" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'Hello World', &1;
        UPPERCASE &1, &2;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_pev(state, 2) == "HELLO WORLD"
    end
  end

  describe "run/2 - procedure calls" do
    test "LINK calls procedure and returns" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 1, I1;
        LINK helper;
        MOVE 3, I1;
      END_PROC
      PROC helper =
        MOVE 2, I2;
        RETURN;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 3
      assert State.get_i(state, 2) == 2
    end

    test "LINK passes parameters" do
      source = """
      PROGRAM test;
      PROC main =
        LINK helper, 42, 'test';
      END_PROC
      PROC helper =
        MOVE P1, I1;
        MOVE P2, &1;
        RETURN;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 42
      assert State.get_pev(state, 1) == "test"
    end

    test "EXIT halts execution" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 1, I1;
        EXIT;
        MOVE 2, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 1
    end
  end

  describe "run/2 - bitwise operations" do
    test "AND performs bitwise AND" do
      source = """
      PROGRAM test;
      PROC main =
        AND 12, 10, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 8  # 1100 AND 1010 = 1000
    end

    test "OR performs bitwise OR" do
      source = """
      PROGRAM test;
      PROC main =
        OR 12, 10, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 14  # 1100 OR 1010 = 1110
    end

    test "XOR performs bitwise XOR" do
      source = """
      PROGRAM test;
      PROC main =
        XOR 12, 10, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 6  # 1100 XOR 1010 = 0110
    end
  end

  describe "run/2 - stack operations" do
    test "PUSH and POP work correctly" do
      source = """
      PROGRAM test;
      PROC main =
        PUSH 42;
        PUSH 'hello';
        POP &1;
        POP I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_pev(state, 1) == "hello"
      assert State.get_i(state, 1) == 42
    end
  end

  describe "run/2 - DEFINE aliases" do
    test "DEFINE creates alias for value" do
      source = """
      PROGRAM test;
      DEFINE MY_CONST, 42;
      PROC main =
        MOVE MY_CONST, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 42
    end
  end

  describe "run/2 - UI stubs" do
    test "CLOSE_WINDOW logs event" do
      source = """
      PROGRAM test;
      PROC main =
        CLOSE_WINDOW;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_events(state, :close_window) == [%{window_id: nil}]
    end

    test "OPEN_WINDOW logs event" do
      source = """
      PROGRAM test;
      PROC main =
        OPEN_WINDOW 'mywindow';
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert [%{window_id: "mywindow"}] = State.get_events(state, :open_window)
    end
  end

  describe "run/2 - parameters" do
    test "accepts initial parameters" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE P1, I1;
        MOVE P2, &1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source, params: [100, "test"])
      assert State.get_i(state, 1) == 100
      assert State.get_pev(state, 1) == "test"
    end

    test "P0 contains parameter count" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE P0, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source, params: ["a", "b", "c"])
      assert State.get_i(state, 1) == 3
    end
  end

  describe "run/2 - complex scenarios" do
    test "comma parsing loop" do
      source = """
      PROGRAM test;
      PROC main =
        MOVE 'a,b,c', &1;
        MOVE 0, I1;
      loop:
        INSTR &1, ',', I2;
        IF I2 = 0 THEN GOTO done;
        ADD 1, I1;
        SUBTRACT 1, I2;
        SUBSTR &1, 1, I2, &2;
        ADD 2, I2;
        LENGTH &1, I3;
        SUBTRACT I2, I3;
        ADD 1, I3;
        SUBSTR &1, I2, I3, &1;
        GOTO loop;
      done:
        ADD 1, I1;
      END_PROC
      """

      assert {:ok, state} = Tbol.run(source)
      assert State.get_i(state, 1) == 3  # Found 3 items
    end
  end
end
