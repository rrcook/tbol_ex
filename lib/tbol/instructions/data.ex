defmodule Tbol.Instructions.Data do
  @moduledoc """
  Data movement instructions: MOVE, CLEAR
  """

  alias Tbol.AST
  alias Tbol.Runtime.State

  @doc """
  Execute MOVE instruction.

  MOVE source, destination [, ABS]
  Copies value from source to destination.
  If ABS is specified, takes absolute value (for numeric).
  """
  def move(state, %AST.Move{source: source, destination: dest, abs: abs}) do
    value = resolve_value(state, source)
    value = if abs, do: abs_value(value), else: value
    set_value(state, dest, value)
  end

  @doc """
  Execute CLEAR instruction.

  CLEAR destination
  Clears the destination to its zero/empty value.
  """
  def clear(state, %AST.Clear{destination: dest, end_destination: nil}) do
    clear_single(state, dest)
  end

  def clear(state, %AST.Clear{destination: start_dest, end_destination: end_dest}) do
    # Range clear - clear all fields/RDA slots between start and end
    case {start_dest, end_dest} do
      {%AST.ScreenField{number: start_n}, %AST.ScreenField{number: end_n}} ->
        Enum.reduce(start_n..end_n, state, fn n, acc ->
          Tbol.Runtime.State.set_pev(acc, n, "")
        end)

      {%AST.RdaRef{slot: start_s}, %AST.RdaRef{slot: end_s}} ->
        Enum.reduce(start_s..end_s, state, fn s, acc ->
          Tbol.Runtime.State.set_rda(acc, s, "")
        end)

      _ ->
        # Fallback: just clear both
        state
        |> clear_single(start_dest)
        |> clear_single(end_dest)
    end
  end

  defp clear_single(state, dest) do
    case dest do
      %AST.Register{type: :i} -> set_value(state, dest, 0)
      %AST.Register{type: :d} -> set_value(state, dest, Decimal.new(0))
      %AST.Register{type: :p} -> set_value(state, dest, "")
      %AST.ScreenField{} -> set_value(state, dest, "")
      %AST.RdaRef{} -> set_value(state, dest, "")
      %AST.GevRef{} -> set_value(state, dest, "")
      %AST.Identifier{name: name} ->
        # Check if it's a defined alias
        case resolve_define(state, name) do
          nil -> state
          resolved -> clear_single(state, resolved)
        end
      _ -> state
    end
  end

  @doc """
  Resolve a value from an AST expression.
  """
  def resolve_value(state, %AST.Literal{value: value, type: :string}), do: value
  def resolve_value(state, %AST.Literal{value: value, type: :hex}), do: value
  def resolve_value(state, %AST.Literal{value: value, type: :integer}), do: value

  def resolve_value(state, %AST.Register{type: :i, number: n, subscript: sub}) do
    State.get_i(state, n, sub)
  end

  def resolve_value(state, %AST.Register{type: :d, number: n, subscript: sub}) do
    State.get_d(state, n, sub)
  end

  def resolve_value(state, %AST.Register{type: :p, number: n, subscript: sub}) do
    State.get_p(state, n, sub)
  end

  def resolve_value(state, %AST.ScreenField{number: n, subscript: sub}) do
    State.get_pev(state, n, sub)
  end

  def resolve_value(state, %AST.RdaRef{slot: slot, subscript: sub}) do
    State.get_rda(state, slot, sub)
  end

  def resolve_value(state, %AST.GevRef{name: name}) do
    State.get_gev(state, name)
  end

  def resolve_value(state, %AST.Identifier{name: name}) do
    case resolve_define(state, name) do
      nil -> ""
      resolved -> resolve_value(state, resolved)
    end
  end

  def resolve_value(_state, nil), do: ""

  @doc """
  Set a value to a destination.
  """
  def set_value(state, %AST.Register{type: :i, number: n, subscript: sub}, value) do
    int_val = to_integer(value)
    State.set_i(state, n, int_val, sub)
  end

  def set_value(state, %AST.Register{type: :d, number: n, subscript: sub}, value) do
    dec_val = to_decimal(value)
    State.set_d(state, n, dec_val, sub)
  end

  def set_value(state, %AST.Register{type: :p, number: n, subscript: sub}, value) do
    State.set_p(state, n, to_string_val(value), sub)
  end

  def set_value(state, %AST.ScreenField{number: n, subscript: sub}, value) do
    State.set_pev(state, n, to_string_val(value), sub)
  end

  def set_value(state, %AST.RdaRef{slot: slot, subscript: sub}, value) do
    State.set_rda(state, slot, to_string_val(value), sub)
  end

  def set_value(state, %AST.GevRef{name: name}, value) do
    State.set_gev(state, name, value)
  end

  def set_value(state, %AST.Identifier{name: name}, value) do
    case resolve_define(state, name) do
      nil -> state
      resolved -> set_value(state, resolved, value)
    end
  end

  def set_value(state, _, _), do: state

  # Helper to resolve DEFINE aliases
  defp resolve_define(state, name) do
    Map.get(state.defines, name)
  end

  # Convert to integer
  defp to_integer(value) when is_integer(value), do: value
  defp to_integer(value) when is_float(value), do: trunc(value)
  defp to_integer(%Decimal{} = value), do: Decimal.to_integer(Decimal.round(value, 0))
  defp to_integer(value) when is_binary(value) do
    case Integer.parse(String.trim(value)) do
      {n, _} -> n
      :error -> 0
    end
  end
  defp to_integer(_), do: 0

  # Convert to Decimal
  defp to_decimal(%Decimal{} = value), do: value
  defp to_decimal(value) when is_integer(value), do: Decimal.new(value)
  defp to_decimal(value) when is_float(value), do: Decimal.from_float(value)
  defp to_decimal(value) when is_binary(value) do
    case Decimal.parse(String.trim(value)) do
      {:ok, d} -> d
      {d, _} -> d
      :error -> Decimal.new(0)
    end
  end
  defp to_decimal(_), do: Decimal.new(0)

  # Convert to string
  defp to_string_val(value) when is_binary(value), do: value
  defp to_string_val(value) when is_integer(value), do: Integer.to_string(value)
  defp to_string_val(%Decimal{} = value), do: Decimal.to_string(value)
  defp to_string_val(value), do: to_string(value)

  # Absolute value helper
  defp abs_value(value) when is_integer(value), do: abs(value)
  defp abs_value(%Decimal{} = value), do: Decimal.abs(value)
  defp abs_value(value) when is_binary(value) do
    case Integer.parse(value) do
      {n, _} -> abs(n)
      :error ->
        case Decimal.parse(value) do
          {:ok, d} -> Decimal.abs(d)
          {d, _} -> Decimal.abs(d)
          :error -> value
        end
    end
  end
  defp abs_value(value), do: value
end
