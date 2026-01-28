defmodule Tbol.Parser do
  @moduledoc """
  Recursive descent parser for TBOL.

  Builds an AST from a token stream.
  """

  alias Tbol.AST

  @type tokens :: [Tbol.Lexer.token()]
  @type parse_result :: {:ok, AST.Program.t()} | {:error, term()}

  @doc """
  Parse a token stream into an AST.
  """
  @spec parse(tokens()) :: parse_result()
  def parse(tokens) do
    case parse_program(tokens) do
      {:ok, program, [{:eof, _, _}]} ->
        {:ok, program}

      {:ok, _program, [token | _]} ->
        {:error, {:unexpected_token, token}}

      {:error, _} = err ->
        err
    end
  end

  # program = "PROGRAM" identifier ";" {copy | define} {procedure}
  defp parse_program([{:keyword, :PROGRAM, _}, {:identifier, name, _}, {:semicolon, _, _} | rest]) do
    {copies, defines, rest2} = parse_copies_and_defines(rest, [], [])

    case parse_procedures(rest2, []) do
      {:ok, procs, rest3} ->
        program = %AST.Program{
          name: name,
          copies: Enum.reverse(copies),
          defines: Enum.reverse(defines),
          procedures: procs
        }

        {:ok, program, rest3}

      {:error, _} = err ->
        err
    end
  end

  defp parse_program([token | _]) do
    {:error, {:expected_program, token}}
  end

  # Parse COPY and DEFINE declarations
  defp parse_copies_and_defines([{:keyword, :COPY, _}, {:identifier, name, _}, {:semicolon, _, _} | rest], copies, defines) do
    parse_copies_and_defines(rest, [%AST.Copy{name: name} | copies], defines)
  end

  defp parse_copies_and_defines([{:keyword, :DEFINE, _} | rest], copies, defines) do
    case parse_define(rest) do
      {:ok, define, rest2} ->
        parse_copies_and_defines(rest2, copies, [define | defines])

      {:error, _} = err ->
        err
    end
  end

  defp parse_copies_and_defines(rest, copies, defines), do: {copies, defines, rest}

  # DEFINE name, value;
  defp parse_define([{:identifier, name, _}, {:comma, _, _} | rest]) do
    case parse_expression(rest) do
      {:ok, value, [{:semicolon, _, _} | rest2]} ->
        {:ok, %AST.Define{name: name, value: value}, rest2}

      {:ok, _, [token | _]} ->
        {:error, {:expected_semicolon, token}}

      {:error, _} = err ->
        err
    end
  end

  defp parse_define([token | _]) do
    {:error, {:invalid_define, token}}
  end

  # Parse procedures
  defp parse_procedures([{:keyword, :PROC, _} | _] = tokens, acc) do
    case parse_procedure(tokens) do
      {:ok, proc, rest} ->
        parse_procedures(rest, [proc | acc])

      {:error, _} = err ->
        err
    end
  end

  defp parse_procedures(rest, acc) do
    {:ok, Enum.reverse(acc), rest}
  end

  # PROC name = {statement} END_PROC
  defp parse_procedure([{:keyword, :PROC, _}, {:identifier, name, _}, {:op, :=, _} | rest]) do
    case parse_statements(rest, []) do
      {:ok, stmts, [{:keyword, :END_PROC, _} | rest2]} ->
        {:ok, %AST.Procedure{name: name, statements: stmts}, rest2}

      {:ok, _, [token | _]} ->
        {:error, {:expected_end_proc, token}}

      {:error, _} = err ->
        err
    end
  end

  defp parse_procedure([token | _]) do
    {:error, {:invalid_procedure, token}}
  end

  # Parse statements until END_PROC or END
  defp parse_statements([{:keyword, :END_PROC, _} | _] = tokens, acc) do
    {:ok, Enum.reverse(acc), tokens}
  end

  defp parse_statements([{:keyword, :END, _} | _] = tokens, acc) do
    {:ok, Enum.reverse(acc), tokens}
  end

  defp parse_statements([{:eof, _, _}] = tokens, acc) do
    {:ok, Enum.reverse(acc), tokens}
  end

  defp parse_statements(tokens, acc) do
    case parse_statement(tokens) do
      {:ok, stmt, rest} ->
        parse_statements(rest, [stmt | acc])

      {:error, _} = err ->
        err
    end
  end

  # Parse a single statement
  defp parse_statement([{:label_def, name, _} | rest]) do
    {:ok, %AST.Label{name: name}, rest}
  end

  defp parse_statement([{:keyword, :MOVE, _} | rest]) do
    parse_move(rest)
  end

  defp parse_statement([{:keyword, :CLEAR, _} | rest]) do
    parse_clear(rest)
  end

  defp parse_statement([{:keyword, :ADD, _} | rest]) do
    parse_add(rest)
  end

  defp parse_statement([{:keyword, :SUBTRACT, _} | rest]) do
    parse_subtract(rest)
  end

  defp parse_statement([{:keyword, :MULTIPLY, _} | rest]) do
    parse_multiply(rest)
  end

  defp parse_statement([{:keyword, :DIVIDE, _} | rest]) do
    parse_divide(rest)
  end

  defp parse_statement([{:keyword, :IF, _} | rest]) do
    parse_if(rest)
  end

  defp parse_statement([{:keyword, :WHILE, _} | rest]) do
    parse_while(rest)
  end

  defp parse_statement([{:keyword, :GOTO, _} | rest]) do
    parse_goto(rest)
  end

  defp parse_statement([{:keyword, :GOTO_DEPENDING_ON, _} | rest]) do
    parse_goto_depending_on(rest)
  end

  defp parse_statement([{:keyword, :LINK, _} | rest]) do
    parse_link(rest)
  end

  defp parse_statement([{:keyword, :TRANSFER, _} | rest]) do
    parse_transfer(rest)
  end

  defp parse_statement([{:keyword, :RETURN, _}, {:semicolon, _, _} | rest]) do
    {:ok, %AST.Return{}, rest}
  end

  defp parse_statement([{:keyword, :EXIT, _}, {:semicolon, _, _} | rest]) do
    {:ok, %AST.Exit{}, rest}
  end

  defp parse_statement([{:keyword, :STRING, _} | rest]) do
    parse_string_op(rest)
  end

  defp parse_statement([{:keyword, :SUBSTR, _} | rest]) do
    parse_substr(rest)
  end

  defp parse_statement([{:keyword, :INSTR, _} | rest]) do
    parse_instr(rest)
  end

  defp parse_statement([{:keyword, :LENGTH, _} | rest]) do
    parse_length(rest)
  end

  defp parse_statement([{:keyword, :UPPERCASE, _} | rest]) do
    parse_uppercase(rest)
  end

  defp parse_statement([{:keyword, :AND, _} | rest]) do
    parse_bitwise_and(rest)
  end

  defp parse_statement([{:keyword, :OR, _} | rest]) do
    parse_bitwise_or(rest)
  end

  defp parse_statement([{:keyword, :XOR, _} | rest]) do
    parse_xor(rest)
  end

  defp parse_statement([{:keyword, :PUSH, _} | rest]) do
    parse_push(rest)
  end

  defp parse_statement([{:keyword, :POP, _} | rest]) do
    parse_pop(rest)
  end

  defp parse_statement([{:keyword, :SAVE, _} | rest]) do
    parse_save(rest)
  end

  defp parse_statement([{:keyword, :RESTORE, _} | rest]) do
    parse_restore(rest)
  end

  defp parse_statement([{:keyword, :RELEASE, _} | rest]) do
    parse_release(rest)
  end

  defp parse_statement([{:keyword, :LOOKUP, _} | rest]) do
    parse_lookup(rest)
  end

  defp parse_statement([{:keyword, :FILL, _} | rest]) do
    parse_fill(rest)
  end

  defp parse_statement([{:keyword, :EDIT, _} | rest]) do
    parse_edit(rest)
  end

  defp parse_statement([{:keyword, :FORMAT, _} | rest]) do
    parse_format(rest)
  end

  defp parse_statement([{:keyword, :MAKE_FORMAT, _} | rest]) do
    parse_make_format(rest)
  end

  defp parse_statement([{:keyword, :SORT, _} | rest]) do
    parse_sort(rest)
  end

  defp parse_statement([{:keyword, :OPEN, _} | rest]) do
    parse_open(rest)
  end

  defp parse_statement([{:keyword, :WRITE, _} | rest]) do
    parse_write(rest)
  end

  defp parse_statement([{:keyword, :CLOSE, _} | rest]) do
    parse_close(rest)
  end

  defp parse_statement([{:keyword, :SEND, _} | rest]) do
    parse_send(rest)
  end

  defp parse_statement([{:keyword, :RECEIVE, _} | rest]) do
    parse_receive(rest)
  end

  defp parse_statement([{:keyword, :OPEN_WINDOW, _} | rest]) do
    parse_open_window(rest)
  end

  defp parse_statement([{:keyword, :CLOSE_WINDOW, _} | rest]) do
    parse_close_window(rest)
  end

  defp parse_statement([{:keyword, :SET_CURSOR, _} | rest]) do
    parse_set_cursor(rest)
  end

  defp parse_statement([{:keyword, :SET_ATTRIBUTE, _} | rest]) do
    parse_set_attribute(rest)
  end

  defp parse_statement([{:keyword, :SET_FUNCTION, _} | rest]) do
    parse_set_function(rest)
  end

  defp parse_statement([{:keyword, :NAVIGATE, _} | rest]) do
    parse_navigate(rest)
  end

  defp parse_statement([{:keyword, :FETCH, _} | rest]) do
    parse_fetch(rest)
  end

  defp parse_statement([{:keyword, :REFRESH, _} | rest]) do
    parse_refresh(rest)
  end

  defp parse_statement([{:keyword, :TIMEOUT, _} | rest]) do
    parse_timeout(rest)
  end

  defp parse_statement([{:keyword, :PRIORITY, _} | rest]) do
    parse_priority(rest)
  end

  defp parse_statement([{:keyword, :OPT_HDRS, _} | rest]) do
    parse_opt_hdrs(rest)
  end

  defp parse_statement([token | _]) do
    {:error, {:unexpected_statement, token}}
  end

  # MOVE source, destination [, ABS];
  defp parse_move(tokens) do
    with {:ok, source, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      {abs, rest3} = parse_optional_abs(rest2)
      expect_semicolon(rest3, %AST.Move{source: source, destination: dest, abs: abs})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_optional_abs([{:comma, _, _}, {:keyword, :ABS, _} | rest]), do: {true, rest}
  defp parse_optional_abs(rest), do: {false, rest}

  # CLEAR destination;
  # CLEAR destination; or CLEAR start, end;
  defp parse_clear(tokens) do
    with {:ok, dest, rest} <- parse_expression(tokens) do
      case rest do
        [{:comma, _, _} | rest2] ->
          with {:ok, end_dest, rest3} <- parse_expression(rest2) do
            expect_semicolon(rest3, %AST.Clear{destination: dest, end_destination: end_dest})
          end

        _ ->
          expect_semicolon(rest, %AST.Clear{destination: dest, end_destination: nil})
      end
    end
  end

  # ADD value, destination;
  defp parse_add(tokens) do
    with {:ok, value, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Add{value: value, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # SUBTRACT value, destination;
  defp parse_subtract(tokens) do
    with {:ok, value, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Subtract{value: value, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # MULTIPLY value, destination;
  defp parse_multiply(tokens) do
    with {:ok, value, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Multiply{value: value, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # DIVIDE value, destination [, remainder];
  defp parse_divide(tokens) do
    with {:ok, value, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      case rest2 do
        [{:comma, _, _} | rest3] ->
          with {:ok, rem, rest4} <- parse_expression(rest3) do
            expect_semicolon(rest4, %AST.Divide{value: value, destination: dest, remainder: rem})
          end

        _ ->
          expect_semicolon(rest2, %AST.Divide{value: value, destination: dest, remainder: nil})
      end
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # IF condition THEN clause [ELSE clause]
  defp parse_if(tokens) do
    with {:ok, condition, [{:keyword, :THEN, _} | rest]} <- parse_condition(tokens),
         {:ok, then_clause, rest2} <- parse_clause(rest) do
      case rest2 do
        [{:keyword, :ELSE, _} | rest3] ->
          with {:ok, else_clause, rest4} <- parse_clause(rest3) do
            {:ok, %AST.If{condition: condition, then_clause: then_clause, else_clause: else_clause}, rest4}
          end

        _ ->
          {:ok, %AST.If{condition: condition, then_clause: then_clause, else_clause: nil}, rest2}
      end
    else
      {:ok, _, [token | _]} -> {:error, {:expected_then, token}}
      {:error, _} = err -> err
    end
  end

  # WHILE condition DO statements END;
  # WHILE condition [THEN] DO statements END;
  # Note: Some TBOL sources use THEN before DO, which is optional
  defp parse_while(tokens) do
    with {:ok, condition, rest} <- parse_condition(tokens) do
      # Skip optional THEN
      rest = case rest do
        [{:keyword, :THEN, _} | rest2] -> rest2
        _ -> rest
      end

      case rest do
        [{:keyword, :DO, _} | rest2] ->
          case parse_statements(rest2, []) do
            {:ok, body, [{:keyword, :END, _}, {:semicolon, _, _} | rest3]} ->
              {:ok, %AST.While{condition: condition, body: body}, rest3}

            {:ok, _, [token | _]} ->
              {:error, {:expected_end, token}}

            {:error, _} = err ->
              err
          end

        [token | _] ->
          {:error, {:expected_do, token}}
      end
    end
  end

  # Parse a clause (single statement or DO...END block)
  defp parse_clause([{:keyword, :DO, _} | rest]) do
    case parse_statements(rest, []) do
      {:ok, stmts, [{:keyword, :END, _}, {:semicolon, _, _} | rest2]} ->
        {:ok, %AST.DoBlock{statements: stmts}, rest2}

      {:ok, _, [token | _]} ->
        {:error, {:expected_end, token}}

      {:error, _} = err ->
        err
    end
  end

  defp parse_clause(tokens) do
    parse_statement(tokens)
  end

  # GOTO label;
  defp parse_goto([{:identifier, label, _}, {:semicolon, _, _} | rest]) do
    {:ok, %AST.Goto{label: label}, rest}
  end

  defp parse_goto([token | _]) do
    {:error, {:expected_label, token}}
  end

  # GOTO_DEPENDING_ON index, label1, label2, ...;
  defp parse_goto_depending_on(tokens) do
    with {:ok, index, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, labels, rest2} <- parse_label_list(rest, []) do
      expect_semicolon(rest2, %AST.GotoDependingOn{index: index, labels: Enum.reverse(labels)})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_label_list([{:identifier, label, _}, {:comma, _, _} | rest], acc) do
    parse_label_list(rest, [label | acc])
  end

  defp parse_label_list([{:identifier, label, _} | rest], acc) do
    {:ok, [label | acc], rest}
  end

  defp parse_label_list([token | _], _acc) do
    {:error, {:expected_label, token}}
  end

  # LINK target [, params...];
  defp parse_link(tokens) do
    with {:ok, target, rest} <- parse_expression(tokens) do
      {params, rest2} = parse_optional_params(rest)
      expect_semicolon(rest2, %AST.Link{target: target, params: params})
    end
  end

  # TRANSFER target [, params...];
  defp parse_transfer(tokens) do
    with {:ok, target, rest} <- parse_expression(tokens) do
      {params, rest2} = parse_optional_params(rest)
      expect_semicolon(rest2, %AST.Transfer{target: target, params: params})
    end
  end

  defp parse_optional_params([{:comma, _, _} | rest]) do
    parse_param_list(rest, [])
  end

  defp parse_optional_params(rest), do: {[], rest}

  defp parse_param_list(tokens, acc) do
    case parse_expression(tokens) do
      {:ok, expr, [{:comma, _, _} | rest]} ->
        parse_param_list(rest, [expr | acc])

      {:ok, expr, rest} ->
        {Enum.reverse([expr | acc]), rest}

      {:error, _} ->
        {Enum.reverse(acc), tokens}
    end
  end

  # STRING dest, src1 [, src2, ...];
  defp parse_string_op(tokens) do
    with {:ok, dest, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, sources, rest2} <- parse_expression_list(rest, []) do
      expect_semicolon(rest2, %AST.StringOp{destination: dest, sources: Enum.reverse(sources)})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_expression_list(tokens, acc) do
    case parse_expression(tokens) do
      {:ok, expr, [{:comma, _, _} | rest]} ->
        parse_expression_list(rest, [expr | acc])

      {:ok, expr, rest} ->
        {:ok, [expr | acc], rest}

      {:error, _} = err ->
        err
    end
  end

  # SUBSTR source, start, length, destination;
  defp parse_substr(tokens) do
    with {:ok, source, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, start, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, length, [{:comma, _, _} | rest3]} <- parse_expression(rest2),
         {:ok, dest, rest4} <- parse_expression(rest3) do
      expect_semicolon(rest4, %AST.Substr{source: source, start: start, length: length, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # INSTR source, search, destination;
  defp parse_instr(tokens) do
    with {:ok, source, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, search, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, dest, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Instr{source: source, search: search, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # LENGTH source, destination;
  defp parse_length(tokens) do
    with {:ok, source, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Length{source: source, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # UPPERCASE source, destination;
  defp parse_uppercase(tokens) do
    with {:ok, source, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Uppercase{source: source, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # AND value1, value2, destination;
  defp parse_bitwise_and(tokens) do
    with {:ok, v1, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, v2, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, dest, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.And{value1: v1, value2: v2, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # OR value1, value2, destination;
  defp parse_bitwise_or(tokens) do
    with {:ok, v1, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, v2, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, dest, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Or{value1: v1, value2: v2, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # XOR value1, value2, destination;
  defp parse_xor(tokens) do
    with {:ok, v1, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, v2, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, dest, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Xor{value1: v1, value2: v2, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # PUSH value;
  defp parse_push(tokens) do
    with {:ok, value, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Push{value: value})
    end
  end

  # POP destination;
  defp parse_pop(tokens) do
    with {:ok, dest, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Pop{destination: dest})
    end
  end

  # SAVE block_name, start, end;
  defp parse_save(tokens) do
    with {:ok, name, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, start, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, end_val, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Save{block_name: name, start: start, end: end_val})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # RESTORE block_name, start;
  defp parse_restore(tokens) do
    with {:ok, name, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, start, rest2} <- parse_expression(rest1) do
      expect_semicolon(rest2, %AST.Restore{block_name: name, start: start})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # RELEASE block_name;
  defp parse_release(tokens) do
    with {:ok, name, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Release{block_name: name})
    end
  end

  # LOOKUP key, table, result [, not_found];
  defp parse_lookup(tokens) do
    with {:ok, key, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, table, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, result, rest3} <- parse_expression(rest2) do
      case rest3 do
        [{:comma, _, _} | rest4] ->
          with {:ok, not_found, rest5} <- parse_expression(rest4) do
            expect_semicolon(rest5, %AST.Lookup{key: key, table: table, result: result, not_found: not_found})
          end

        _ ->
          expect_semicolon(rest3, %AST.Lookup{key: key, table: table, result: result, not_found: nil})
      end
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # FILL value, start, count;
  defp parse_fill(tokens) do
    with {:ok, value, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, start, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, count, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Fill{value: value, start: start, count: count})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # EDIT source, format, destination;
  defp parse_edit(tokens) do
    with {:ok, source, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, format, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, dest, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Edit{source: source, format: format, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # FORMAT source, format_string, destination;
  defp parse_format(tokens) do
    with {:ok, source, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, fmt, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, dest, rest3} <- parse_expression(rest2) do
      expect_semicolon(rest3, %AST.Format{source: source, format_string: fmt, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # MAKE_FORMAT source, destination;
  defp parse_make_format(tokens) do
    with {:ok, source, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.MakeFormat{source: source, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # SORT start, count, width [, direction];
  defp parse_sort(tokens) do
    with {:ok, start, [{:comma, _, _} | rest1]} <- parse_expression(tokens),
         {:ok, count, [{:comma, _, _} | rest2]} <- parse_expression(rest1),
         {:ok, width, rest3} <- parse_expression(rest2) do
      case rest3 do
        [{:comma, _, _} | rest4] ->
          with {:ok, dir, rest5} <- parse_expression(rest4) do
            expect_semicolon(rest5, %AST.Sort{start: start, count: count, width: width, direction: dir})
          end

        _ ->
          expect_semicolon(rest3, %AST.Sort{start: start, count: count, width: width, direction: :ascending})
      end
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # I/O stubs
  defp parse_open(tokens) do
    with {:ok, file, rest} <- parse_expression(tokens) do
      case rest do
        [{:comma, _, _} | rest2] ->
          with {:ok, mode, rest3} <- parse_expression(rest2) do
            expect_semicolon(rest3, %AST.Open{file: file, mode: mode})
          end

        _ ->
          expect_semicolon(rest, %AST.Open{file: file, mode: nil})
      end
    end
  end

  defp parse_write(tokens) do
    with {:ok, file, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, data, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Write{file: file, data: data})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_close(tokens) do
    with {:ok, file, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Close{file: file})
    end
  end

  defp parse_send(tokens) do
    with {:ok, dest, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, data, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Send{destination: dest, data: data})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_receive(tokens) do
    with {:ok, source, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, dest, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.Receive{source: source, destination: dest})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  # UI stubs
  defp parse_open_window(tokens) do
    with {:ok, window_id, rest} <- parse_expression(tokens) do
      {params, rest2} = parse_optional_params(rest)
      expect_semicolon(rest2, %AST.OpenWindow{window_id: window_id, params: params})
    end
  end

  defp parse_close_window([{:semicolon, _, _} | rest]) do
    {:ok, %AST.CloseWindow{window_id: nil}, rest}
  end

  defp parse_close_window(tokens) do
    with {:ok, window_id, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.CloseWindow{window_id: window_id})
    end
  end

  defp parse_set_cursor(tokens) do
    with {:ok, field, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.SetCursor{field: field})
    end
  end

  defp parse_set_attribute(tokens) do
    with {:ok, field, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, attr, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.SetAttribute{field: field, attribute: attr})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_set_function(tokens) do
    with {:ok, key, [{:comma, _, _} | rest]} <- parse_expression(tokens),
         {:ok, action, rest2} <- parse_expression(rest) do
      expect_semicolon(rest2, %AST.SetFunction{key: key, action: action})
    else
      {:ok, _, [token | _]} -> {:error, {:expected_comma, token}}
      {:error, _} = err -> err
    end
  end

  defp parse_navigate(tokens) do
    with {:ok, target, rest} <- parse_expression(tokens) do
      {params, rest2} = parse_optional_params(rest)
      expect_semicolon(rest2, %AST.Navigate{target: target, params: params})
    end
  end

  defp parse_fetch(tokens) do
    with {:ok, object_id, rest} <- parse_expression(tokens) do
      {params, rest2} = parse_optional_params(rest)
      expect_semicolon(rest2, %AST.Fetch{object_id: object_id, params: params})
    end
  end

  defp parse_refresh([{:semicolon, _, _} | rest]) do
    {:ok, %AST.Refresh{field: nil}, rest}
  end

  defp parse_refresh(tokens) do
    with {:ok, field, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Refresh{field: field})
    end
  end

  defp parse_timeout(tokens) do
    with {:ok, value, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Timeout{value: value})
    end
  end

  defp parse_priority(tokens) do
    with {:ok, value, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.Priority{value: value})
    end
  end

  defp parse_opt_hdrs(tokens) do
    with {:ok, option, rest} <- parse_expression(tokens) do
      expect_semicolon(rest, %AST.OptHdrs{option: option})
    end
  end

  # Parse condition (comparison with optional AND/OR)
  defp parse_condition(tokens) do
    with {:ok, left, rest} <- parse_comparison(tokens) do
      parse_condition_rest(left, rest)
    end
  end

  defp parse_condition_rest(left, [{:keyword, :AND, _} | rest]) do
    with {:ok, right, rest2} <- parse_comparison(rest) do
      parse_condition_rest(%AST.BinaryOp{op: :and, left: left, right: right}, rest2)
    end
  end

  defp parse_condition_rest(left, [{:keyword, :OR, _} | rest]) do
    with {:ok, right, rest2} <- parse_comparison(rest) do
      parse_condition_rest(%AST.BinaryOp{op: :or, left: left, right: right}, rest2)
    end
  end

  defp parse_condition_rest(left, rest) do
    {:ok, left, rest}
  end

  # Parse comparison
  defp parse_comparison(tokens) do
    with {:ok, left, rest} <- parse_expression(tokens) do
      case rest do
        [{:op, op, _} | rest2] when op in [:=, :<>, :<, :>, :<=, :>=] ->
          with {:ok, right, rest3} <- parse_expression(rest2) do
            {:ok, %AST.BinaryOp{op: op, left: left, right: right}, rest3}
          end

        _ ->
          {:ok, left, rest}
      end
    end
  end

  # Parse expression (single value)
  defp parse_expression([{:string, value, _} | rest]) do
    {:ok, %AST.Literal{value: value, type: :string}, rest}
  end

  defp parse_expression([{:hex, value, _} | rest]) do
    {:ok, %AST.Literal{value: value, type: :hex}, rest}
  end

  defp parse_expression([{:integer, value, _} | rest]) do
    {:ok, %AST.Literal{value: value, type: :integer}, rest}
  end

  defp parse_expression([{:screen_field, {number, subscript}, _} | rest]) do
    sub_expr = parse_subscript_tokens(subscript)
    {:ok, %AST.ScreenField{number: number, subscript: sub_expr}, rest}
  end

  defp parse_expression([{:i_register, {number, subscript}, _} | rest]) do
    sub_expr = parse_subscript_tokens(subscript)
    {:ok, %AST.Register{type: :i, number: number, subscript: sub_expr}, rest}
  end

  defp parse_expression([{:i_register, number, _} | rest]) when is_integer(number) do
    {:ok, %AST.Register{type: :i, number: number, subscript: nil}, rest}
  end

  defp parse_expression([{:d_register, {number, subscript}, _} | rest]) do
    sub_expr = parse_subscript_tokens(subscript)
    {:ok, %AST.Register{type: :d, number: number, subscript: sub_expr}, rest}
  end

  defp parse_expression([{:d_register, number, _} | rest]) when is_integer(number) do
    {:ok, %AST.Register{type: :d, number: number, subscript: nil}, rest}
  end

  defp parse_expression([{:p_register, {number, subscript}, _} | rest]) do
    sub_expr = parse_subscript_tokens(subscript)
    {:ok, %AST.Register{type: :p, number: number, subscript: sub_expr}, rest}
  end

  defp parse_expression([{:p_register, number, _} | rest]) when is_integer(number) do
    {:ok, %AST.Register{type: :p, number: number, subscript: nil}, rest}
  end

  defp parse_expression([{:rda, {slot, subscript}, _} | rest]) do
    sub_expr = parse_subscript_tokens(subscript)
    {:ok, %AST.RdaRef{slot: slot, subscript: sub_expr}, rest}
  end

  defp parse_expression([{:rda, slot, _} | rest]) when is_integer(slot) do
    {:ok, %AST.RdaRef{slot: slot, subscript: nil}, rest}
  end

  defp parse_expression([{:sys_gev, name, _} | rest]) do
    {:ok, %AST.GevRef{name: name}, rest}
  end

  defp parse_expression([{:prf_gev, name, _} | rest]) do
    {:ok, %AST.GevRef{name: name}, rest}
  end

  defp parse_expression([{:identifier, name, _} | rest]) do
    {:ok, %AST.Identifier{name: name}, rest}
  end

  defp parse_expression([token | _]) do
    {:error, {:unexpected_expression, token}}
  end

  # Convert subscript tokens to AST
  defp parse_subscript_tokens(nil), do: nil

  defp parse_subscript_tokens(tokens) when is_list(tokens) do
    case tokens do
      [{:integer, n}] -> %AST.Literal{value: n, type: :integer}
      [{:i_register, n}] -> %AST.Register{type: :i, number: n, subscript: nil}
      [{:d_register, n}] -> %AST.Register{type: :d, number: n, subscript: nil}
      [{:p_register, n}] -> %AST.Register{type: :p, number: n, subscript: nil}
      [{:identifier, name}] -> %AST.Identifier{name: name}
      _ -> nil
    end
  end

  # Expect semicolon and return statement
  defp expect_semicolon([{:semicolon, _, _} | rest], stmt) do
    {:ok, stmt, rest}
  end

  defp expect_semicolon([token | _], _stmt) do
    {:error, {:expected_semicolon, token}}
  end

  defp expect_semicolon([], _stmt) do
    {:error, :unexpected_eof}
  end
end
