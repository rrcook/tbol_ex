defmodule Tbol.Instructions.Procedure do
  @moduledoc """
  Procedure call instructions: LINK, TRANSFER, RETURN, EXIT
  """

  alias Tbol.AST
  alias Tbol.Runtime.State
  alias Tbol.Instructions.Data

  @doc """
  Prepare for a LINK call.

  Saves current P registers and sets up parameters.
  Returns updated state with call_stack entry.
  """
  def prepare_link(state, %AST.Link{target: target, params: params}) do
    target_name = resolve_target(state, target)

    # Resolve all parameters
    param_values = Enum.map(params, &Data.resolve_value(state, &1))

    # Save current P registers and execution context
    saved_context = %{
      procedure: state.current_procedure,
      index: state.current_index,
      p_registers: state.p_registers
    }

    # Set up new P registers
    p_regs = setup_params(param_values)

    state
    |> Map.put(:p_registers, p_regs)
    |> Map.put(:call_stack, [saved_context | state.call_stack])
    |> then(&{&1, target_name})
  end

  @doc """
  Prepare for a TRANSFER call.

  Like LINK but does not save context (cannot return).
  """
  def prepare_transfer(state, %AST.Transfer{target: target, params: params}) do
    target_name = resolve_target(state, target)

    # Resolve all parameters
    param_values = Enum.map(params, &Data.resolve_value(state, &1))

    # Set up new P registers
    p_regs = setup_params(param_values)

    state
    |> Map.put(:p_registers, p_regs)
    |> then(&{&1, target_name})
  end

  @doc """
  Handle RETURN instruction.

  Restores P registers and returns to caller.
  Returns {state, procedure_name, statement_index} or {state, :halt} if no caller.
  """
  def handle_return(state) do
    case state.call_stack do
      [saved_context | rest] ->
        state
        |> Map.put(:p_registers, saved_context.p_registers)
        |> Map.put(:call_stack, rest)
        |> then(&{&1, saved_context.procedure, saved_context.index + 1})

      [] ->
        # No caller - halt execution
        {%{state | halted: true}, :halt}
    end
  end

  @doc """
  Handle EXIT instruction.

  Halts execution immediately.
  """
  def handle_exit(state) do
    %{state | halted: true}
  end

  # Resolve target procedure name
  defp resolve_target(state, %AST.Identifier{name: name}) do
    # Check if it's a defined alias
    case Map.get(state.defines, name) do
      %AST.Literal{value: value, type: :string} -> value
      _ -> name
    end
  end

  defp resolve_target(_state, %AST.Literal{value: value, type: :string}), do: value
  defp resolve_target(_state, target), do: to_string(target)

  # Set up P registers for call
  defp setup_params(param_values) do
    count = length(param_values)

    base = %{0 => count, 1 => "", 2 => "", 3 => "", 4 => "", 5 => "", 6 => "", 7 => "", 8 => ""}

    param_values
    |> Enum.with_index(1)
    |> Enum.reduce(base, fn {value, idx}, acc ->
      if idx <= 8, do: Map.put(acc, idx, value), else: acc
    end)
  end
end
