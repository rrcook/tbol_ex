defmodule Tbol.Instructions.Control do
  @moduledoc """
  Control flow instructions: IF, WHILE, GOTO, GOTO_DEPENDING_ON
  """

  alias Tbol.AST
  alias Tbol.Instructions.Data

  @doc """
  Evaluate a condition expression.

  Returns true or false based on the condition.
  """
  def evaluate_condition(state, %AST.BinaryOp{op: :and, left: left, right: right}) do
    evaluate_condition(state, left) and evaluate_condition(state, right)
  end

  def evaluate_condition(state, %AST.BinaryOp{op: :or, left: left, right: right}) do
    evaluate_condition(state, left) or evaluate_condition(state, right)
  end

  def evaluate_condition(state, %AST.BinaryOp{op: op, left: left, right: right}) do
    left_val = Data.resolve_value(state, left)
    right_val = Data.resolve_value(state, right)
    compare(left_val, right_val, op)
  end

  def evaluate_condition(state, expr) do
    # Single expression: true if non-zero/non-empty
    value = Data.resolve_value(state, expr)
    truthy?(value)
  end

  @doc """
  Compare two values with the given operator.

  Handles TBOL string comparison rules:
  - Truncates longer string to shorter length
  - Uses "high/low" rules for length tiebreaker
  """
  def compare(left, right, op) do
    # Normalize types for comparison
    {left_norm, right_norm} = normalize_for_comparison(left, right)

    case op do
      := -> left_norm == right_norm
      :<> -> left_norm != right_norm
      :< -> compare_less(left_norm, right_norm)
      :> -> compare_greater(left_norm, right_norm)
      :<= -> compare_less(left_norm, right_norm) or left_norm == right_norm
      :>= -> compare_greater(left_norm, right_norm) or left_norm == right_norm
    end
  end

  # Normalize values for comparison
  defp normalize_for_comparison(left, right) when is_binary(left) and is_binary(right) do
    # TBOL truncates longer string to shorter length for comparison
    min_len = min(String.length(left), String.length(right))
    {String.slice(left, 0, min_len), String.slice(right, 0, min_len)}
  end

  defp normalize_for_comparison(left, right) when is_integer(left) and is_integer(right) do
    {left, right}
  end

  defp normalize_for_comparison(%Decimal{} = left, %Decimal{} = right) do
    {left, right}
  end

  defp normalize_for_comparison(%Decimal{} = left, right) when is_integer(right) do
    {left, Decimal.new(right)}
  end

  defp normalize_for_comparison(left, %Decimal{} = right) when is_integer(left) do
    {Decimal.new(left), right}
  end

  defp normalize_for_comparison(left, right) when is_binary(left) and is_integer(right) do
    # Try to parse string as integer
    case Integer.parse(String.trim(left)) do
      {n, _} -> {n, right}
      :error -> {left, Integer.to_string(right)}
    end
  end

  defp normalize_for_comparison(left, right) when is_integer(left) and is_binary(right) do
    case Integer.parse(String.trim(right)) do
      {n, _} -> {left, n}
      :error -> {Integer.to_string(left), right}
    end
  end

  defp normalize_for_comparison(left, right) do
    {to_string(left), to_string(right)}
  end

  defp compare_less(left, right) when is_binary(left) and is_binary(right) do
    left < right
  end

  defp compare_less(left, right) when is_integer(left) and is_integer(right) do
    left < right
  end

  defp compare_less(%Decimal{} = left, %Decimal{} = right) do
    Decimal.lt?(left, right)
  end

  defp compare_less(_, _), do: false

  defp compare_greater(left, right) when is_binary(left) and is_binary(right) do
    left > right
  end

  defp compare_greater(left, right) when is_integer(left) and is_integer(right) do
    left > right
  end

  defp compare_greater(%Decimal{} = left, %Decimal{} = right) do
    Decimal.gt?(left, right)
  end

  defp compare_greater(_, _), do: false

  defp truthy?(0), do: false
  defp truthy?(""), do: false
  defp truthy?(%Decimal{} = d), do: not Decimal.equal?(d, Decimal.new(0))
  defp truthy?(nil), do: false
  defp truthy?(_), do: true

  @doc """
  Find label index in a procedure's statements.
  """
  def find_label(statements, label_name) do
    Enum.find_index(statements, fn
      %AST.Label{name: name} -> name == label_name
      _ -> false
    end)
  end

  @doc """
  Build a label map for a procedure.

  Returns a map of label_name => statement_index.
  """
  def build_label_map(statements) do
    statements
    |> Enum.with_index()
    |> Enum.filter(fn {stmt, _} -> match?(%AST.Label{}, stmt) end)
    |> Enum.map(fn {%AST.Label{name: name}, idx} -> {name, idx} end)
    |> Map.new()
  end

  @doc """
  Get the target index for GOTO_DEPENDING_ON.

  Returns the label index based on the index value (1-based).
  """
  def get_goto_depending_target(state, %AST.GotoDependingOn{index: index_expr, labels: labels}, label_map) do
    index = Data.resolve_value(state, index_expr)
    index_int = to_integer(index)

    # TBOL uses 1-based indexing for GOTO_DEPENDING_ON
    if index_int >= 1 and index_int <= length(labels) do
      label_name = Enum.at(labels, index_int - 1)
      Map.get(label_map, label_name)
    else
      nil
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
end
