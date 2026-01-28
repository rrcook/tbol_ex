defmodule Tbol.Instructions.IO do
  @moduledoc """
  I/O and UI instructions (stubs for simulation).

  These operations log events for testing rather than performing real I/O.
  """

  alias Tbol.AST
  alias Tbol.Runtime.State
  alias Tbol.Instructions.Data

  # File I/O stubs

  @doc """
  Execute OPEN instruction (stub).
  """
  def open(state, %AST.Open{file: file, mode: mode}) do
    file_val = Data.resolve_value(state, file)
    mode_val = if mode, do: Data.resolve_value(state, mode), else: nil
    State.log_event(state, :open, %{file: file_val, mode: mode_val})
  end

  @doc """
  Execute WRITE instruction (stub).
  """
  def write(state, %AST.Write{file: file, data: data}) do
    file_val = Data.resolve_value(state, file)
    data_val = Data.resolve_value(state, data)
    State.log_event(state, :write, %{file: file_val, data: data_val})
  end

  @doc """
  Execute CLOSE instruction (stub).
  """
  def close(state, %AST.Close{file: file}) do
    file_val = Data.resolve_value(state, file)
    State.log_event(state, :close, %{file: file_val})
  end

  # Network stubs

  @doc """
  Execute SEND instruction (stub).
  """
  def send_data(state, %AST.Send{destination: dest, data: data}) do
    dest_val = Data.resolve_value(state, dest)
    data_val = Data.resolve_value(state, data)
    State.log_event(state, :send, %{destination: dest_val, data: data_val})
  end

  @doc """
  Execute RECEIVE instruction (stub).
  """
  def receive_data(state, %AST.Receive{source: source, destination: dest}) do
    source_val = Data.resolve_value(state, source)
    # Stub: receive empty data
    state = Data.set_value(state, dest, "")
    State.log_event(state, :receive, %{source: source_val})
  end

  @doc """
  Execute FETCH instruction (stub).
  """
  def fetch(state, %AST.Fetch{object_id: obj_id, params: params}) do
    obj_val = Data.resolve_value(state, obj_id)
    param_vals = Enum.map(params, &Data.resolve_value(state, &1))
    State.log_event(state, :fetch, %{object_id: obj_val, params: param_vals})
  end

  # UI stubs

  @doc """
  Execute OPEN_WINDOW instruction (stub).
  """
  def open_window(state, %AST.OpenWindow{window_id: win_id, params: params}) do
    win_val = Data.resolve_value(state, win_id)
    param_vals = Enum.map(params || [], &Data.resolve_value(state, &1))
    State.log_event(state, :open_window, %{window_id: win_val, params: param_vals})
  end

  @doc """
  Execute CLOSE_WINDOW instruction (stub).
  """
  def close_window(state, %AST.CloseWindow{window_id: win_id}) do
    win_val = if win_id, do: Data.resolve_value(state, win_id), else: nil
    State.log_event(state, :close_window, %{window_id: win_val})
  end

  @doc """
  Execute SET_CURSOR instruction (stub).
  """
  def set_cursor(state, %AST.SetCursor{field: field}) do
    field_val = Data.resolve_value(state, field)
    State.log_event(state, :set_cursor, %{field: field_val})
  end

  @doc """
  Execute SET_ATTRIBUTE instruction (stub).
  """
  def set_attribute(state, %AST.SetAttribute{field: field, attribute: attr}) do
    field_val = Data.resolve_value(state, field)
    attr_val = Data.resolve_value(state, attr)
    State.log_event(state, :set_attribute, %{field: field_val, attribute: attr_val})
  end

  @doc """
  Execute SET_FUNCTION instruction (stub).
  """
  def set_function(state, %AST.SetFunction{key: key, action: action}) do
    key_val = Data.resolve_value(state, key)
    action_val = Data.resolve_value(state, action)
    State.log_event(state, :set_function, %{key: key_val, action: action_val})
  end

  @doc """
  Execute NAVIGATE instruction (stub).
  """
  def navigate(state, %AST.Navigate{target: target, params: params}) do
    target_val = Data.resolve_value(state, target)
    param_vals = Enum.map(params || [], &Data.resolve_value(state, &1))
    State.log_event(state, :navigate, %{target: target_val, params: param_vals})
  end

  @doc """
  Execute REFRESH instruction (stub).
  """
  def refresh(state, %AST.Refresh{field: field}) do
    field_val = if field, do: Data.resolve_value(state, field), else: nil
    State.log_event(state, :refresh, %{field: field_val})
  end

  @doc """
  Execute TIMEOUT instruction (stub).
  """
  def timeout(state, %AST.Timeout{value: value}) do
    val = Data.resolve_value(state, value)
    State.log_event(state, :timeout, %{value: val})
  end

  @doc """
  Execute PRIORITY instruction (stub).
  """
  def priority(state, %AST.Priority{value: value}) do
    val = Data.resolve_value(state, value)
    State.log_event(state, :priority, %{value: val})
  end

  @doc """
  Execute OPT_HDRS instruction (stub).
  """
  def opt_hdrs(state, %AST.OptHdrs{option: option}) do
    val = Data.resolve_value(state, option)
    State.log_event(state, :opt_hdrs, %{option: val})
  end

  # Advanced operations

  @doc """
  Execute PUSH instruction.
  """
  def push(state, %AST.Push{value: value}) do
    val = Data.resolve_value(state, value)
    State.push(state, val)
  end

  @doc """
  Execute POP instruction.
  """
  def pop(state, %AST.Pop{destination: dest}) do
    {val, state} = State.pop(state)
    Data.set_value(state, dest, val)
  end

  @doc """
  Execute SAVE instruction.

  Saves a range of PEV values for later restoration.
  """
  def save(state, %AST.Save{block_name: name_expr, start: start_expr, end: end_expr}) do
    name = Data.resolve_value(state, name_expr) |> to_string()
    start_idx = Data.resolve_value(state, start_expr) |> to_integer()
    end_idx = Data.resolve_value(state, end_expr) |> to_integer()

    # Save PEV values in range
    saved_data =
      for i <- start_idx..end_idx, into: %{} do
        {i, State.get_pev(state, i)}
      end

    block = %{start: start_idx, data: saved_data}
    %{state | saved_blocks: Map.put(state.saved_blocks, name, block)}
  end

  @doc """
  Execute RESTORE instruction.

  Restores previously saved PEV values.
  """
  def restore(state, %AST.Restore{block_name: name_expr, start: start_expr}) do
    name = Data.resolve_value(state, name_expr) |> to_string()
    new_start = Data.resolve_value(state, start_expr) |> to_integer()

    case Map.get(state.saved_blocks, name) do
      nil ->
        state

      %{start: orig_start, data: data} ->
        offset = new_start - orig_start

        Enum.reduce(data, state, fn {orig_idx, value}, acc ->
          State.set_pev(acc, orig_idx + offset, value)
        end)
    end
  end

  @doc """
  Execute RELEASE instruction.

  Removes a saved block.
  """
  def release(state, %AST.Release{block_name: name_expr}) do
    name = Data.resolve_value(state, name_expr) |> to_string()
    %{state | saved_blocks: Map.delete(state.saved_blocks, name)}
  end

  @doc """
  Execute LOOKUP instruction (stub).
  """
  def lookup(state, %AST.Lookup{key: key, table: table, result: result, not_found: not_found}) do
    key_val = Data.resolve_value(state, key)
    table_val = Data.resolve_value(state, table)

    # Stub: always return not_found value or empty
    result_val = if not_found, do: Data.resolve_value(state, not_found), else: ""

    state = Data.set_value(state, result, result_val)
    State.log_event(state, :lookup, %{key: key_val, table: table_val})
  end

  @doc """
  Execute FILL instruction.

  Fills a range of PEV slots with a value.
  """
  def fill(state, %AST.Fill{value: value_expr, start: start_expr, count: count_expr}) do
    value = Data.resolve_value(state, value_expr)
    start = Data.resolve_value(state, start_expr) |> to_integer()
    count = Data.resolve_value(state, count_expr) |> to_integer()

    Enum.reduce(0..(count - 1), state, fn i, acc ->
      State.set_pev(acc, start + i, value)
    end)
  end

  @doc """
  Execute EDIT instruction (stub).
  """
  def edit(state, %AST.Edit{source: source, format: format, destination: dest}) do
    source_val = Data.resolve_value(state, source)
    format_val = Data.resolve_value(state, format)
    # Stub: just copy source to dest
    state = Data.set_value(state, dest, source_val)
    State.log_event(state, :edit, %{source: source_val, format: format_val})
  end

  @doc """
  Execute FORMAT instruction (stub).
  """
  def format(state, %AST.Format{source: source, format_string: fmt, destination: dest}) do
    source_val = Data.resolve_value(state, source)
    fmt_val = Data.resolve_value(state, fmt)
    # Stub: just copy source to dest
    state = Data.set_value(state, dest, source_val)
    State.log_event(state, :format, %{source: source_val, format: fmt_val})
  end

  @doc """
  Execute MAKE_FORMAT instruction (stub).
  """
  def make_format(state, %AST.MakeFormat{source: source, destination: dest}) do
    source_val = Data.resolve_value(state, source)
    # Stub: just copy source to dest
    state = Data.set_value(state, dest, source_val)
    State.log_event(state, :make_format, %{source: source_val})
  end

  @doc """
  Execute SORT instruction (stub).
  """
  def sort(state, %AST.Sort{start: start_expr, count: count_expr, width: width_expr, direction: dir}) do
    start = Data.resolve_value(state, start_expr) |> to_integer()
    count = Data.resolve_value(state, count_expr) |> to_integer()
    width = Data.resolve_value(state, width_expr) |> to_integer()
    direction = if is_atom(dir), do: dir, else: Data.resolve_value(state, dir)

    # Gather items to sort
    items =
      for i <- 0..(count - 1) do
        # Gather 'width' PEV slots for each item
        for j <- 0..(width - 1), into: "" do
          State.get_pev(state, start + i * width + j)
        end
      end

    # Sort items
    sorted =
      case direction do
        :ascending -> Enum.sort(items)
        :descending -> Enum.sort(items, :desc)
        _ -> Enum.sort(items)
      end

    # Write sorted items back
    Enum.with_index(sorted)
    |> Enum.reduce(state, fn {item, i}, acc ->
      # This is simplified - actual implementation would distribute across width slots
      State.set_pev(acc, start + i * width, item)
    end)
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
