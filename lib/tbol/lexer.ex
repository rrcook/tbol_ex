defmodule Tbol.Lexer do
  @moduledoc """
  Tokenizer for TBOL source code.

  Converts source text into a stream of tokens for the parser.
  """

  @keywords ~w(
    PROGRAM PROC END_PROC IF THEN ELSE DO END WHILE GOTO GOTO_DEPENDING_ON
    LINK TRANSFER RETURN EXIT DEFINE COPY MOVE CLEAR ADD SUBTRACT MULTIPLY
    DIVIDE STRING SUBSTR INSTR LENGTH UPPERCASE AND OR XOR ABS PUSH POP
    SAVE RESTORE RELEASE LOOKUP FILL EDIT FORMAT MAKE_FORMAT SORT OPEN
    WRITE CLOSE SEND RECEIVE OPEN_WINDOW CLOSE_WINDOW SET_CURSOR
    SET_ATTRIBUTE SET_FUNCTION NAVIGATE FETCH REFRESH TIMEOUT PRIORITY
    OPT_HDRS
  )

  @type token :: {atom(), any(), {pos_integer(), pos_integer()}}

  @doc """
  Tokenize TBOL source code.

  Returns `{:ok, tokens}` on success or `{:error, reason}` on failure.
  """
  @spec tokenize(String.t()) :: {:ok, [token()]} | {:error, term()}
  def tokenize(source) do
    source
    |> String.to_charlist()
    |> do_tokenize(1, 1, [])
  end

  defp do_tokenize([], _line, _col, acc) do
    {:ok, Enum.reverse([{:eof, nil, {0, 0}} | acc])}
  end

  # Skip whitespace
  defp do_tokenize([?\s | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, acc)
  end

  defp do_tokenize([?\t | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, acc)
  end

  defp do_tokenize([?\r, ?\n | rest], line, _col, acc) do
    do_tokenize(rest, line + 1, 1, acc)
  end

  defp do_tokenize([?\n | rest], line, _col, acc) do
    do_tokenize(rest, line + 1, 1, acc)
  end

  defp do_tokenize([?\r | rest], line, _col, acc) do
    do_tokenize(rest, line + 1, 1, acc)
  end

  # Comments: { ... }
  defp do_tokenize([?{ | rest], line, col, acc) do
    {rest2, line2, col2} = skip_comment(rest, line, col + 1)
    do_tokenize(rest2, line2, col2, acc)
  end

  # String literals: '...'
  defp do_tokenize([?' | rest], line, col, acc) do
    case scan_string(rest, line, col + 1, []) do
      {:ok, value, rest2, line2, col2} ->
        token = {:string, value, {line, col}}
        do_tokenize(rest2, line2, col2, [token | acc])

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Hex literals: 0x...
  defp do_tokenize([?0, ?x | rest], line, col, acc) do
    {hex_chars, rest2} = scan_hex(rest, [])
    hex_str = to_string(Enum.reverse(hex_chars))

    case Base.decode16(hex_str, case: :mixed) do
      {:ok, binary} ->
        token = {:hex, binary, {line, col}}
        do_tokenize(rest2, line, col + 2 + length(hex_chars), [token | acc])

      :error ->
        {:error, {:invalid_hex, hex_str, line, col}}
    end
  end

  # Screen field: &number or &number(subscript)
  defp do_tokenize([?& | rest], line, col, acc) do
    {num_chars, rest2} = scan_digits(rest, [])

    if num_chars == [] do
      {:error, {:invalid_screen_field, line, col}}
    else
      number = num_chars |> Enum.reverse() |> to_string() |> String.to_integer()
      {subscript, rest3, col3} = maybe_scan_subscript(rest2, col + 1 + length(num_chars))
      token = {:screen_field, {number, subscript}, {line, col}}
      do_tokenize(rest3, line, col3, [token | acc])
    end
  end

  # Operators and punctuation
  defp do_tokenize([?<, ?> | rest], line, col, acc) do
    do_tokenize(rest, line, col + 2, [{:op, :<>, {line, col}} | acc])
  end

  defp do_tokenize([?<, ?= | rest], line, col, acc) do
    do_tokenize(rest, line, col + 2, [{:op, :<=, {line, col}} | acc])
  end

  defp do_tokenize([?>, ?= | rest], line, col, acc) do
    do_tokenize(rest, line, col + 2, [{:op, :>=, {line, col}} | acc])
  end

  defp do_tokenize([?=, ?> | rest], line, col, acc) do
    # => also means >=
    do_tokenize(rest, line, col + 2, [{:op, :>=, {line, col}} | acc])
  end

  defp do_tokenize([?= | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:op, :=, {line, col}} | acc])
  end

  defp do_tokenize([?< | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:op, :<, {line, col}} | acc])
  end

  defp do_tokenize([?> | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:op, :>, {line, col}} | acc])
  end

  defp do_tokenize([?; | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:semicolon, nil, {line, col}} | acc])
  end

  defp do_tokenize([?, | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:comma, nil, {line, col}} | acc])
  end

  defp do_tokenize([?( | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:lparen, nil, {line, col}} | acc])
  end

  defp do_tokenize([?) | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:rparen, nil, {line, col}} | acc])
  end

  defp do_tokenize([?: | rest], line, col, acc) do
    do_tokenize(rest, line, col + 1, [{:colon, nil, {line, col}} | acc])
  end

  # Negative numbers: - followed by digits
  defp do_tokenize([?-, c | rest], line, col, acc) when c >= ?0 and c <= ?9 do
    {num_chars, rest2} = scan_digits([c | rest], [])
    value = -(num_chars |> Enum.reverse() |> to_string() |> String.to_integer())
    token = {:integer, value, {line, col}}
    do_tokenize(rest2, line, col + 1 + length(num_chars), [token | acc])
  end

  # Numbers
  defp do_tokenize([c | _] = chars, line, col, acc) when c >= ?0 and c <= ?9 do
    {num_chars, rest} = scan_digits(chars, [])
    value = num_chars |> Enum.reverse() |> to_string() |> String.to_integer()
    token = {:integer, value, {line, col}}
    do_tokenize(rest, line, col + length(num_chars), [token | acc])
  end

  # Identifiers and keywords
  defp do_tokenize([c | _] = chars, line, col, acc) when c >= ?a and c <= ?z or c >= ?A and c <= ?Z or c == ?_ do
    {id_chars, rest} = scan_identifier(chars, [])
    id_str = id_chars |> Enum.reverse() |> to_string()
    id_upper = String.upcase(id_str)
    new_col = col + length(id_chars)

    # Check if followed by colon (label definition)
    case rest do
      [?: | rest2] ->
        token = {:label_def, id_str, {line, col}}
        do_tokenize(rest2, line, new_col + 1, [token | acc])

      _ ->
        token = classify_identifier(id_str, id_upper, line, col)
        {token2, rest2, col2} = maybe_add_subscript(token, rest, line, new_col)
        do_tokenize(rest2, line, col2, [token2 | acc])
    end
  end

  defp do_tokenize([c | _], line, col, _acc) do
    {:error, {:unexpected_char, c, line, col}}
  end

  # Skip comment until closing brace
  defp skip_comment([], line, col), do: {[], line, col}
  defp skip_comment([?} | rest], line, col), do: {rest, line, col + 1}
  defp skip_comment([?\n | rest], line, _col), do: skip_comment(rest, line + 1, 1)
  defp skip_comment([_ | rest], line, col), do: skip_comment(rest, line, col + 1)

  # Scan string literal
  defp scan_string([], line, col, _acc) do
    {:error, {:unterminated_string, line, col}}
  end

  defp scan_string([?' | rest], _line, col, acc) do
    value = acc |> Enum.reverse() |> to_string()
    {:ok, value, rest, _line, col + 1}
  end

  defp scan_string([?\\, ?x, h1, h2 | rest], line, col, acc) when h1 in ?0..?9 or h1 in ?a..?f or h1 in ?A..?F do
    hex = <<h1, h2>>
    case Base.decode16(hex, case: :mixed) do
      {:ok, <<byte>>} ->
        scan_string(rest, line, col + 4, [byte | acc])

      :error ->
        {:error, {:invalid_hex_escape, line, col}}
    end
  end

  defp scan_string([?\n | rest], line, _col, acc) do
    scan_string(rest, line + 1, 1, [?\n | acc])
  end

  defp scan_string([c | rest], line, col, acc) do
    scan_string(rest, line, col + 1, [c | acc])
  end

  # Scan hex digits
  defp scan_hex([c | rest], acc) when c in ?0..?9 or c in ?a..?f or c in ?A..?F do
    scan_hex(rest, [c | acc])
  end

  defp scan_hex(rest, acc), do: {acc, rest}

  # Scan digits
  defp scan_digits([c | rest], acc) when c >= ?0 and c <= ?9 do
    scan_digits(rest, [c | acc])
  end

  defp scan_digits(rest, acc), do: {acc, rest}

  # Scan identifier
  defp scan_identifier([c | rest], acc)
       when c >= ?a and c <= ?z or c >= ?A and c <= ?Z or c >= ?0 and c <= ?9 or c == ?_ do
    scan_identifier(rest, [c | acc])
  end

  defp scan_identifier(rest, acc), do: {acc, rest}

  # Classify identifier as keyword, register, or identifier
  defp classify_identifier(original, upper, line, col) do
    cond do
      upper in @keywords ->
        {:keyword, String.to_atom(upper), {line, col}}

      # Integer registers I1-I8
      Regex.match?(~r/^I[1-8]$/i, upper) ->
        num = String.to_integer(String.slice(upper, 1..-1//1))
        {:i_register, num, {line, col}}

      # Decimal registers D1-D8
      Regex.match?(~r/^D[1-8]$/i, upper) ->
        num = String.to_integer(String.slice(upper, 1..-1//1))
        {:d_register, num, {line, col}}

      # Parameter registers P0-P8
      Regex.match?(~r/^P[0-8]$/i, upper) ->
        num = String.to_integer(String.slice(upper, 1..-1//1))
        {:p_register, num, {line, col}}

      # RDA references
      Regex.match?(~r/^RDA\d+$/i, upper) ->
        num = String.to_integer(String.slice(upper, 3..-1//1))
        {:rda, num, {line, col}}

      # System GEV
      String.starts_with?(upper, "SYS_") ->
        {:sys_gev, upper, {line, col}}

      # Profile GEV
      String.starts_with?(upper, "PRF_") ->
        {:prf_gev, upper, {line, col}}

      true ->
        {:identifier, original, {line, col}}
    end
  end

  # Maybe scan subscript for registers
  defp maybe_add_subscript({type, value, pos} = token, [?( | rest], _line, col)
       when type in [:i_register, :d_register, :p_register, :rda] do
    case scan_subscript_expr(rest, []) do
      {:ok, subscript_tokens, rest2, col2} ->
        {{type, {value, subscript_tokens}, pos}, rest2, col2}

      {:error, _} = err ->
        {token, [?( | rest], col}
    end
  end

  defp maybe_add_subscript(token, rest, _line, col), do: {token, rest, col}

  # Maybe scan subscript for screen fields
  defp maybe_scan_subscript([?( | rest], col) do
    case scan_subscript_expr(rest, []) do
      {:ok, subscript_tokens, rest2, col2} ->
        {subscript_tokens, rest2, col2}

      {:error, _} ->
        {nil, [?( | rest], col}
    end
  end

  defp maybe_scan_subscript(rest, col), do: {nil, rest, col}

  # Scan subscript expression until closing paren
  defp scan_subscript_expr([?) | rest], acc) do
    {:ok, Enum.reverse(acc), rest, 0}  # col doesn't matter much here
  end

  defp scan_subscript_expr([], _acc) do
    {:error, :unterminated_subscript}
  end

  defp scan_subscript_expr([?\s | rest], acc) do
    scan_subscript_expr(rest, acc)
  end

  defp scan_subscript_expr([c | _] = chars, acc) when c >= ?0 and c <= ?9 do
    {num_chars, rest} = scan_digits(chars, [])
    value = num_chars |> Enum.reverse() |> to_string() |> String.to_integer()
    scan_subscript_expr(rest, [{:integer, value} | acc])
  end

  defp scan_subscript_expr([c | _] = chars, acc) when c >= ?a and c <= ?z or c >= ?A and c <= ?Z or c == ?_ do
    {id_chars, rest} = scan_identifier(chars, [])
    id_str = id_chars |> Enum.reverse() |> to_string()
    id_upper = String.upcase(id_str)

    token =
      cond do
        Regex.match?(~r/^I[1-8]$/i, id_upper) ->
          {:i_register, String.to_integer(String.slice(id_upper, 1..-1//1))}

        Regex.match?(~r/^D[1-8]$/i, id_upper) ->
          {:d_register, String.to_integer(String.slice(id_upper, 1..-1//1))}

        Regex.match?(~r/^P[0-8]$/i, id_upper) ->
          {:p_register, String.to_integer(String.slice(id_upper, 1..-1//1))}

        true ->
          {:identifier, id_str}
      end

    scan_subscript_expr(rest, [token | acc])
  end

  defp scan_subscript_expr([c | _], _acc) do
    {:error, {:unexpected_in_subscript, c}}
  end
end
