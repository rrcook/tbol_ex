defmodule Tbol.Interpreter do
  @moduledoc """
  Tree-walk interpreter for TBOL programs.

  Executes TBOL AST by walking the tree and dispatching to instruction modules.
  """

  alias Tbol.AST
  alias Tbol.Runtime.State
  alias Tbol.Instructions.{Arithmetic, Bitwise, Control, Data, IO, Procedure, String}

  @max_iterations 100_000

  @doc """
  Run a TBOL program.

  Returns `{:ok, final_state}` on success or `{:error, reason}` on failure.
  """
  def run(%AST.Program{} = program, %State{} = initial_state \\ State.new()) do
    state =
      initial_state
      |> load_defines(program.defines)
      |> load_procedures(program.procedures)

    case execute_procedure(state, "main") do
      {:ok, final_state} -> {:ok, final_state}
      {:error, _} = err -> err
    end
  end

  @doc """
  Load DEFINE declarations into state.
  """
  def load_defines(state, defines) do
    defines_map =
      Enum.reduce(defines, %{}, fn %AST.Define{name: name, value: value}, acc ->
        Map.put(acc, name, value)
      end)

    %{state | defines: Map.merge(state.defines, defines_map)}
  end

  @doc """
  Load procedures into state.
  """
  def load_procedures(state, procedures) do
    procs_map =
      Enum.reduce(procedures, %{}, fn %AST.Procedure{name: name} = proc, acc ->
        Map.put(acc, name, proc)
      end)

    # Build label maps for each procedure
    labels_map =
      Enum.reduce(procedures, %{}, fn %AST.Procedure{name: name, statements: stmts}, acc ->
        Map.put(acc, name, Control.build_label_map(stmts))
      end)

    %{state | procedures: Map.merge(state.procedures, procs_map), labels: Map.merge(state.labels, labels_map)}
  end

  @doc """
  Execute a procedure by name.
  """
  def execute_procedure(state, proc_name) do
    case Map.get(state.procedures, proc_name) do
      nil ->
        {:error, {:undefined_procedure, proc_name}}

      %AST.Procedure{statements: statements} ->
        state = %{state | current_procedure: proc_name, current_index: 0}
        label_map = Map.get(state.labels, proc_name, %{})
        execute_statements(state, statements, 0, label_map)
    end
  end

  # Execute statements starting at index
  defp execute_statements(state, statements, index, label_map, iterations \\ 0)

  defp execute_statements(state, _statements, _index, _label_map, iterations) when iterations > @max_iterations do
    {:error, {:max_iterations_exceeded, iterations}}
  end

  defp execute_statements(%{halted: true} = state, _statements, _index, _label_map, _iterations) do
    {:ok, state}
  end

  defp execute_statements(state, statements, index, label_map, iterations) do
    case Enum.at(statements, index) do
      nil ->
        # End of procedure
        {:ok, state}

      stmt ->
        state = %{state | current_index: index}

        case execute_statement(state, stmt, statements, label_map) do
          {:ok, new_state, :next} ->
            execute_statements(new_state, statements, index + 1, label_map, iterations + 1)

          {:ok, new_state, {:goto, target_index}} ->
            execute_statements(new_state, statements, target_index, label_map, iterations + 1)

          {:ok, new_state, {:call, proc_name}} ->
            case execute_procedure(new_state, proc_name) do
              {:ok, returned_state} ->
                execute_statements(returned_state, statements, index + 1, label_map, iterations + 1)

              {:error, _} = err ->
                err
            end

          {:ok, new_state, {:return, proc_name, return_index}} ->
            # Return to a different procedure
            case Map.get(new_state.procedures, proc_name) do
              nil -> {:ok, new_state}
              %AST.Procedure{statements: ret_stmts} ->
                ret_label_map = Map.get(new_state.labels, proc_name, %{})
                execute_statements(new_state, ret_stmts, return_index, ret_label_map, iterations + 1)
            end

          {:ok, new_state, :halt} ->
            {:ok, new_state}

          {:error, _} = err ->
            err
        end
    end
  end

  # Execute a single statement
  defp execute_statement(state, %AST.Label{}, _statements, _label_map) do
    {:ok, state, :next}
  end

  defp execute_statement(state, %AST.Move{} = stmt, _statements, _label_map) do
    {:ok, Data.move(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Clear{} = stmt, _statements, _label_map) do
    {:ok, Data.clear(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Add{} = stmt, _statements, _label_map) do
    {:ok, Arithmetic.add(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Subtract{} = stmt, _statements, _label_map) do
    {:ok, Arithmetic.subtract(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Multiply{} = stmt, _statements, _label_map) do
    {:ok, Arithmetic.multiply(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Divide{} = stmt, _statements, _label_map) do
    {:ok, Arithmetic.divide(state, stmt), :next}
  end

  defp execute_statement(state, %AST.If{} = stmt, statements, label_map) do
    if Control.evaluate_condition(state, stmt.condition) do
      execute_clause(state, stmt.then_clause, statements, label_map)
    else
      case stmt.else_clause do
        nil -> {:ok, state, :next}
        else_clause -> execute_clause(state, else_clause, statements, label_map)
      end
    end
  end

  defp execute_statement(state, %AST.While{condition: condition, body: body}, statements, label_map) do
    execute_while(state, condition, body, statements, label_map, 0)
  end

  defp execute_statement(state, %AST.Goto{label: label}, _statements, label_map) do
    case Map.get(label_map, label) do
      nil -> {:error, {:undefined_label, label}}
      target_index -> {:ok, state, {:goto, target_index}}
    end
  end

  defp execute_statement(state, %AST.GotoDependingOn{} = stmt, _statements, label_map) do
    case Control.get_goto_depending_target(state, stmt, label_map) do
      nil -> {:ok, state, :next}  # Out of range - continue
      target_index -> {:ok, state, {:goto, target_index}}
    end
  end

  defp execute_statement(state, %AST.Link{} = stmt, _statements, _label_map) do
    {new_state, target} = Procedure.prepare_link(state, stmt)
    {:ok, new_state, {:call, target}}
  end

  defp execute_statement(state, %AST.Transfer{} = stmt, _statements, _label_map) do
    {new_state, target} = Procedure.prepare_transfer(state, stmt)
    {:ok, new_state, {:call, target}}
  end

  defp execute_statement(state, %AST.Return{}, _statements, _label_map) do
    case Procedure.handle_return(state) do
      {new_state, :halt} -> {:ok, new_state, :halt}
      {new_state, proc_name, return_index} -> {:ok, new_state, {:return, proc_name, return_index}}
    end
  end

  defp execute_statement(state, %AST.Exit{}, _statements, _label_map) do
    {:ok, Procedure.handle_exit(state), :halt}
  end

  defp execute_statement(state, %AST.StringOp{} = stmt, _statements, _label_map) do
    {:ok, String.string_concat(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Substr{} = stmt, _statements, _label_map) do
    {:ok, String.substr(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Instr{} = stmt, _statements, _label_map) do
    {:ok, String.instr(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Length{} = stmt, _statements, _label_map) do
    {:ok, String.length_op(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Uppercase{} = stmt, _statements, _label_map) do
    {:ok, String.uppercase(state, stmt), :next}
  end

  defp execute_statement(state, %AST.And{} = stmt, _statements, _label_map) do
    {:ok, Bitwise.bitwise_and(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Or{} = stmt, _statements, _label_map) do
    {:ok, Bitwise.bitwise_or(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Xor{} = stmt, _statements, _label_map) do
    {:ok, Bitwise.bitwise_xor(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Push{} = stmt, _statements, _label_map) do
    {:ok, IO.push(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Pop{} = stmt, _statements, _label_map) do
    {:ok, IO.pop(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Save{} = stmt, _statements, _label_map) do
    {:ok, IO.save(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Restore{} = stmt, _statements, _label_map) do
    {:ok, IO.restore(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Release{} = stmt, _statements, _label_map) do
    {:ok, IO.release(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Lookup{} = stmt, _statements, _label_map) do
    {:ok, IO.lookup(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Fill{} = stmt, _statements, _label_map) do
    {:ok, IO.fill(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Edit{} = stmt, _statements, _label_map) do
    {:ok, IO.edit(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Format{} = stmt, _statements, _label_map) do
    {:ok, IO.format(state, stmt), :next}
  end

  defp execute_statement(state, %AST.MakeFormat{} = stmt, _statements, _label_map) do
    {:ok, IO.make_format(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Sort{} = stmt, _statements, _label_map) do
    {:ok, IO.sort(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Open{} = stmt, _statements, _label_map) do
    {:ok, IO.open(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Write{} = stmt, _statements, _label_map) do
    {:ok, IO.write(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Close{} = stmt, _statements, _label_map) do
    {:ok, IO.close(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Send{} = stmt, _statements, _label_map) do
    {:ok, IO.send_data(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Receive{} = stmt, _statements, _label_map) do
    {:ok, IO.receive_data(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Fetch{} = stmt, _statements, _label_map) do
    {:ok, IO.fetch(state, stmt), :next}
  end

  defp execute_statement(state, %AST.OpenWindow{} = stmt, _statements, _label_map) do
    {:ok, IO.open_window(state, stmt), :next}
  end

  defp execute_statement(state, %AST.CloseWindow{} = stmt, _statements, _label_map) do
    {:ok, IO.close_window(state, stmt), :next}
  end

  defp execute_statement(state, %AST.SetCursor{} = stmt, _statements, _label_map) do
    {:ok, IO.set_cursor(state, stmt), :next}
  end

  defp execute_statement(state, %AST.SetAttribute{} = stmt, _statements, _label_map) do
    {:ok, IO.set_attribute(state, stmt), :next}
  end

  defp execute_statement(state, %AST.SetFunction{} = stmt, _statements, _label_map) do
    {:ok, IO.set_function(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Navigate{} = stmt, _statements, _label_map) do
    {:ok, IO.navigate(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Refresh{} = stmt, _statements, _label_map) do
    {:ok, IO.refresh(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Timeout{} = stmt, _statements, _label_map) do
    {:ok, IO.timeout(state, stmt), :next}
  end

  defp execute_statement(state, %AST.Priority{} = stmt, _statements, _label_map) do
    {:ok, IO.priority(state, stmt), :next}
  end

  defp execute_statement(state, %AST.OptHdrs{} = stmt, _statements, _label_map) do
    {:ok, IO.opt_hdrs(state, stmt), :next}
  end

  defp execute_statement(_state, stmt, _statements, _label_map) do
    {:error, {:unknown_statement, stmt}}
  end

  # Execute a clause (single statement or DO block)
  defp execute_clause(state, %AST.DoBlock{statements: stmts}, _outer_stmts, label_map) do
    case execute_block(state, stmts, label_map) do
      {:ok, new_state, nil} -> {:ok, new_state, :next}
      {:ok, new_state, jump} -> {:ok, new_state, jump}
      {:error, _} = err -> err
    end
  end

  defp execute_clause(state, stmt, statements, label_map) do
    execute_statement(state, stmt, statements, label_map)
  end

  # Execute a block of statements (for DO...END)
  defp execute_block(state, [], _label_map) do
    {:ok, state, nil}
  end

  defp execute_block(state, [stmt | rest], label_map) do
    case execute_statement(state, stmt, rest, label_map) do
      {:ok, new_state, :next} ->
        execute_block(new_state, rest, label_map)

      {:ok, _new_state, _jump} = result ->
        result

      {:error, _} = err ->
        err
    end
  end

  # Execute WHILE loop
  defp execute_while(state, _condition, _body, _statements, _label_map, iterations) when iterations > @max_iterations do
    {:error, {:max_iterations_exceeded, iterations}}
  end

  defp execute_while(state, condition, body, statements, label_map, iterations) do
    if Control.evaluate_condition(state, condition) do
      case execute_block(state, body, label_map) do
        {:ok, new_state, nil} ->
          execute_while(new_state, condition, body, statements, label_map, iterations + 1)

        {:ok, new_state, {:goto, _} = jump} ->
          {:ok, new_state, jump}

        {:ok, new_state, :halt} ->
          {:ok, new_state, :halt}

        {:ok, new_state, {:return, _, _} = ret} ->
          {:ok, new_state, ret}

        {:error, _} = err ->
          err
      end
    else
      {:ok, state, :next}
    end
  end
end
