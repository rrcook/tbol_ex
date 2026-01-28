defmodule Tbol.Instructions.Arithmetic do
  @moduledoc """
  Arithmetic instructions: ADD, SUBTRACT, MULTIPLY, DIVIDE
  """

  alias Tbol.AST
  alias Tbol.Runtime.State
  alias Tbol.Instructions.Data

  @doc """
  Execute ADD instruction.

  ADD value, destination
  destination = destination + value
  """
  def add(state, %AST.Add{value: value_expr, destination: dest}) do
    value = Data.resolve_value(state, value_expr)
    current = Data.resolve_value(state, dest)

    result = numeric_add(current, value)
    Data.set_value(state, dest, result)
  end

  @doc """
  Execute SUBTRACT instruction.

  SUBTRACT value, destination
  destination = destination - value
  """
  def subtract(state, %AST.Subtract{value: value_expr, destination: dest}) do
    value = Data.resolve_value(state, value_expr)
    current = Data.resolve_value(state, dest)

    result = numeric_subtract(current, value)
    Data.set_value(state, dest, result)
  end

  @doc """
  Execute MULTIPLY instruction.

  MULTIPLY value, destination
  destination = destination * value
  """
  def multiply(state, %AST.Multiply{value: value_expr, destination: dest}) do
    value = Data.resolve_value(state, value_expr)
    current = Data.resolve_value(state, dest)

    result = numeric_multiply(current, value)
    Data.set_value(state, dest, result)
  end

  @doc """
  Execute DIVIDE instruction.

  DIVIDE value, destination [, remainder]
  destination = destination / value
  If remainder is provided, stores remainder there.
  """
  def divide(state, %AST.Divide{value: value_expr, destination: dest, remainder: rem_dest}) do
    value = Data.resolve_value(state, value_expr)
    current = Data.resolve_value(state, dest)

    {quotient, remainder} = numeric_divide(current, value)

    state = Data.set_value(state, dest, quotient)

    if rem_dest do
      Data.set_value(state, rem_dest, remainder)
    else
      state
    end
  end

  # Numeric operations that handle mixed types
  defp numeric_add(a, b) do
    {a_num, b_num, is_decimal} = normalize_operands(a, b)

    if is_decimal do
      Decimal.add(a_num, b_num)
    else
      a_num + b_num
    end
  end

  defp numeric_subtract(a, b) do
    {a_num, b_num, is_decimal} = normalize_operands(a, b)

    if is_decimal do
      Decimal.sub(a_num, b_num)
    else
      a_num - b_num
    end
  end

  defp numeric_multiply(a, b) do
    {a_num, b_num, is_decimal} = normalize_operands(a, b)

    if is_decimal do
      Decimal.mult(a_num, b_num)
    else
      a_num * b_num
    end
  end

  defp numeric_divide(a, b) do
    {a_num, b_num, is_decimal} = normalize_operands(a, b)

    cond do
      # Division by zero check
      (is_decimal and Decimal.equal?(b_num, Decimal.new(0))) or
      (not is_decimal and b_num == 0) ->
        {0, 0}

      is_decimal ->
        # Integer division for Decimal
        quotient = Decimal.div_int(a_num, b_num)
        remainder = Decimal.rem(a_num, b_num)
        {quotient, remainder}

      true ->
        {div(a_num, b_num), rem(a_num, b_num)}
    end
  end

  # Normalize operands for arithmetic
  defp normalize_operands(a, b) do
    a_is_decimal = is_struct(a, Decimal)
    b_is_decimal = is_struct(b, Decimal)

    cond do
      a_is_decimal or b_is_decimal ->
        {to_decimal(a), to_decimal(b), true}

      true ->
        {to_integer(a), to_integer(b), false}
    end
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

  defp to_decimal(%Decimal{} = value), do: value
  defp to_decimal(value) when is_integer(value), do: Decimal.new(value)
  defp to_decimal(value) when is_binary(value) do
    case Decimal.parse(String.trim(value)) do
      {:ok, d} -> d
      {d, _} -> d
      :error -> Decimal.new(0)
    end
  end
  defp to_decimal(_), do: Decimal.new(0)
end
