defmodule Tbol.Instructions.Bitwise do
  @moduledoc """
  Bitwise instructions: AND, OR, XOR
  """

  alias Tbol.AST
  alias Tbol.Instructions.Data

  @doc """
  Execute AND instruction.

  AND value1, value2, destination
  destination = value1 AND value2 (bitwise)
  """
  def bitwise_and(state, %AST.And{value1: v1_expr, value2: v2_expr, destination: dest}) do
    v1 = Data.resolve_value(state, v1_expr) |> to_integer()
    v2 = Data.resolve_value(state, v2_expr) |> to_integer()

    result = Bitwise.band(v1, v2)
    Data.set_value(state, dest, result)
  end

  @doc """
  Execute OR instruction.

  OR value1, value2, destination
  destination = value1 OR value2 (bitwise)
  """
  def bitwise_or(state, %AST.Or{value1: v1_expr, value2: v2_expr, destination: dest}) do
    v1 = Data.resolve_value(state, v1_expr) |> to_integer()
    v2 = Data.resolve_value(state, v2_expr) |> to_integer()

    result = Bitwise.bor(v1, v2)
    Data.set_value(state, dest, result)
  end

  @doc """
  Execute XOR instruction.

  XOR value1, value2, destination
  destination = value1 XOR value2 (bitwise)
  """
  def bitwise_xor(state, %AST.Xor{value1: v1_expr, value2: v2_expr, destination: dest}) do
    v1 = Data.resolve_value(state, v1_expr) |> to_integer()
    v2 = Data.resolve_value(state, v2_expr) |> to_integer()

    result = Bitwise.bxor(v1, v2)
    Data.set_value(state, dest, result)
  end

  defp to_integer(value) when is_integer(value), do: value
  defp to_integer(%Decimal{} = value), do: Decimal.to_integer(Decimal.round(value, 0))
  defp to_integer(value) when is_binary(value) do
    case Integer.parse(String.trim(value)) do
      {n, _} -> n
      :error -> 0
    end
  end
  defp to_integer(_), do: 0
end
