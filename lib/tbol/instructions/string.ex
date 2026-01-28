defmodule Tbol.Instructions.String do
  @moduledoc """
  String instructions: STRING, SUBSTR, INSTR, LENGTH, UPPERCASE
  """

  alias Tbol.AST
  alias Tbol.Instructions.Data

  @doc """
  Execute STRING instruction.

  STRING destination, src1 [, src2, ...]
  Concatenates all sources into destination.
  """
  def string_concat(state, %AST.StringOp{destination: dest, sources: sources}) do
    result =
      sources
      |> Enum.map(&Data.resolve_value(state, &1))
      |> Enum.map(&to_string_val/1)
      |> Enum.join("")

    Data.set_value(state, dest, result)
  end

  @doc """
  Execute SUBSTR instruction.

  SUBSTR source, start, length, destination
  Extracts substring from source starting at position start (1-indexed) for length characters.
  """
  def substr(state, %AST.Substr{source: source, start: start_expr, length: len_expr, destination: dest}) do
    source_val = Data.resolve_value(state, source) |> to_string_val()
    start = Data.resolve_value(state, start_expr) |> to_integer()
    length = Data.resolve_value(state, len_expr) |> to_integer()

    # TBOL uses 1-indexed positions
    result =
      if start >= 1 and length > 0 do
        String.slice(source_val, start - 1, length) || ""
      else
        ""
      end

    Data.set_value(state, dest, result)
  end

  @doc """
  Execute INSTR instruction.

  INSTR source, search, destination
  Finds the position of search in source (1-indexed). Returns 0 if not found.
  """
  def instr(state, %AST.Instr{source: source, search: search_expr, destination: dest}) do
    source_val = Data.resolve_value(state, source) |> to_string_val()
    search_val = Data.resolve_value(state, search_expr) |> to_string_val()

    result =
      case :binary.match(source_val, search_val) do
        {pos, _len} -> pos + 1  # Convert 0-indexed to 1-indexed
        :nomatch -> 0
      end

    Data.set_value(state, dest, result)
  end

  @doc """
  Execute LENGTH instruction.

  LENGTH source, destination
  Stores the length of source string in destination.
  """
  def length_op(state, %AST.Length{source: source, destination: dest}) do
    source_val = Data.resolve_value(state, source) |> to_string_val()
    result = String.length(source_val)
    Data.set_value(state, dest, result)
  end

  @doc """
  Execute UPPERCASE instruction.

  UPPERCASE source, destination
  Converts source to uppercase and stores in destination.
  """
  def uppercase(state, %AST.Uppercase{source: source, destination: dest}) do
    source_val = Data.resolve_value(state, source) |> to_string_val()
    result = String.upcase(source_val)
    Data.set_value(state, dest, result)
  end

  # Convert to string
  defp to_string_val(value) when is_binary(value), do: value
  defp to_string_val(value) when is_integer(value), do: Integer.to_string(value)
  defp to_string_val(%Decimal{} = value), do: Decimal.to_string(value)
  defp to_string_val(value), do: to_string(value)

  # Convert to integer
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
