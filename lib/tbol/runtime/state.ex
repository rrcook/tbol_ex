defmodule Tbol.Runtime.State do
  @moduledoc """
  Runtime state for the TBOL interpreter.

  Manages all registers, memory areas, and execution context.
  """

  defstruct [
    # Integer registers I1-I8 (range: -32767 to 32767)
    i_registers: %{1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0},

    # Decimal registers D1-D8 (high precision decimals)
    d_registers: %{1 => Decimal.new(0), 2 => Decimal.new(0), 3 => Decimal.new(0),
                   4 => Decimal.new(0), 5 => Decimal.new(0), 6 => Decimal.new(0),
                   7 => Decimal.new(0), 8 => Decimal.new(0)},

    # Parameter registers P0-P8 (P0 = param count, read-only in some contexts)
    p_registers: %{0 => 0, 1 => "", 2 => "", 3 => "", 4 => "", 5 => "", 6 => "", 7 => "", 8 => ""},

    # Partition External Variables &1-&256 (screen fields)
    pev: %{},

    # Global External Variables #1-#32000
    gev: %{},

    # Runtime Data Array RDA0-RDA118+
    rda: %{},

    # System stack for PUSH/POP
    stack: [],

    # Saved blocks (SAVE/RESTORE)
    saved_blocks: %{},

    # System variables (SYS_*)
    sys: %{
      "SYS_RETURN_CODE" => 0,
      "SYS_DATE" => "",
      "SYS_TIME" => "",
      "SYS_USER_ID" => "",
      "SYS_OBJECT_ID" => "",
      "SYS_ELEMENT_ID" => ""
    },

    # Profile data (PRF_*)
    profile: %{},

    # DEFINE aliases
    defines: %{},

    # Loaded procedures
    procedures: %{},

    # Labels to statement indices for each procedure
    labels: %{},

    # Call stack for procedure returns (stores {proc_name, statement_index, saved_p_registers})
    call_stack: [],

    # Current execution context
    current_procedure: nil,
    current_index: 0,

    # UI/IO event log (for testing stubs)
    event_log: [],

    # Halt flag
    halted: false
  ]

  @type t :: %__MODULE__{}

  @doc """
  Create a new runtime state.
  """
  @spec new() :: t()
  def new do
    %__MODULE__{
      d_registers: %{
        1 => Decimal.new(0), 2 => Decimal.new(0), 3 => Decimal.new(0),
        4 => Decimal.new(0), 5 => Decimal.new(0), 6 => Decimal.new(0),
        7 => Decimal.new(0), 8 => Decimal.new(0)
      }
    }
  end

  @doc """
  Set parameters P1-P8 and P0 (param count).
  """
  @spec set_params(t(), list()) :: t()
  def set_params(state, params) when is_list(params) do
    count = length(params)
    p_regs = state.p_registers
             |> Map.put(0, count)

    p_regs = params
             |> Enum.with_index(1)
             |> Enum.reduce(p_regs, fn {value, idx}, acc ->
               if idx <= 8, do: Map.put(acc, idx, value), else: acc
             end)

    %{state | p_registers: p_regs}
  end

  @doc """
  Get an I register value, with optional indirection.
  """
  @spec get_i(t(), integer(), term()) :: integer()
  def get_i(state, number, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    Map.get(state.i_registers, clamp_register_index(index), 0)
  end

  @doc """
  Set an I register value.
  """
  @spec set_i(t(), integer(), integer(), term()) :: t()
  def set_i(state, number, value, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    clamped_index = clamp_register_index(index)
    clamped_value = clamp_integer(value)
    %{state | i_registers: Map.put(state.i_registers, clamped_index, clamped_value)}
  end

  @doc """
  Get a D register value.
  """
  @spec get_d(t(), integer(), term()) :: Decimal.t()
  def get_d(state, number, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    Map.get(state.d_registers, clamp_register_index(index), Decimal.new(0))
  end

  @doc """
  Set a D register value.
  """
  @spec set_d(t(), integer(), Decimal.t() | number(), term()) :: t()
  def set_d(state, number, value, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    decimal_value = if is_struct(value, Decimal), do: value, else: Decimal.new(to_string(value))
    %{state | d_registers: Map.put(state.d_registers, clamp_register_index(index), decimal_value)}
  end

  @doc """
  Get a P register value.
  """
  @spec get_p(t(), integer(), term()) :: term()
  def get_p(state, number, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    Map.get(state.p_registers, clamp_p_index(index), "")
  end

  @doc """
  Set a P register value (P0 is read-only param count).
  """
  @spec set_p(t(), integer(), term(), term()) :: t()
  def set_p(state, 0, _value, _subscript), do: state  # P0 is read-only

  def set_p(state, number, value, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    %{state | p_registers: Map.put(state.p_registers, clamp_p_index(index), value)}
  end

  @doc """
  Get a PEV (screen field) value.
  """
  @spec get_pev(t(), integer(), term()) :: String.t()
  def get_pev(state, number, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    Map.get(state.pev, index, "")
  end

  @doc """
  Set a PEV (screen field) value.
  """
  @spec set_pev(t(), integer(), term(), term()) :: t()
  def set_pev(state, number, value, subscript \\ nil) do
    index = resolve_index(state, number, subscript)
    %{state | pev: Map.put(state.pev, index, to_string(value))}
  end

  @doc """
  Get an RDA slot value.
  """
  @spec get_rda(t(), integer(), term()) :: String.t()
  def get_rda(state, slot, subscript \\ nil) do
    index = resolve_index(state, slot, subscript)
    Map.get(state.rda, index, "")
  end

  @doc """
  Set an RDA slot value.
  """
  @spec set_rda(t(), integer(), term(), term()) :: t()
  def set_rda(state, slot, value, subscript \\ nil) do
    index = resolve_index(state, slot, subscript)
    %{state | rda: Map.put(state.rda, index, to_string(value))}
  end

  @doc """
  Get a GEV value (SYS_* or PRF_*).
  """
  @spec get_gev(t(), String.t()) :: term()
  def get_gev(state, name) when is_binary(name) do
    cond do
      String.starts_with?(name, "SYS_") ->
        Map.get(state.sys, name, "")

      String.starts_with?(name, "PRF_") ->
        Map.get(state.profile, name, "")

      true ->
        Map.get(state.gev, name, "")
    end
  end

  @doc """
  Set a GEV value.
  """
  @spec set_gev(t(), String.t(), term()) :: t()
  def set_gev(state, name, value) when is_binary(name) do
    cond do
      String.starts_with?(name, "SYS_") ->
        %{state | sys: Map.put(state.sys, name, value)}

      String.starts_with?(name, "PRF_") ->
        %{state | profile: Map.put(state.profile, name, value)}

      true ->
        %{state | gev: Map.put(state.gev, name, value)}
    end
  end

  @doc """
  Push a value onto the stack.
  """
  @spec push(t(), term()) :: t()
  def push(state, value) do
    %{state | stack: [value | state.stack]}
  end

  @doc """
  Pop a value from the stack.
  """
  @spec pop(t()) :: {term(), t()}
  def pop(%{stack: []} = state), do: {"", state}

  def pop(%{stack: [head | tail]} = state) do
    {head, %{state | stack: tail}}
  end

  @doc """
  Log an event (for UI/IO stub testing).
  """
  @spec log_event(t(), atom(), term()) :: t()
  def log_event(state, type, data) do
    %{state | event_log: [{type, data} | state.event_log]}
  end

  @doc """
  Get events of a specific type.
  """
  @spec get_events(t(), atom()) :: [term()]
  def get_events(state, type) do
    state.event_log
    |> Enum.filter(fn {t, _} -> t == type end)
    |> Enum.map(fn {_, data} -> data end)
    |> Enum.reverse()
  end

  # Resolve register indirection
  defp resolve_index(state, number, nil), do: number

  defp resolve_index(state, _number, %Tbol.AST.Register{type: :i, number: n, subscript: nil}) do
    get_i(state, n)
  end

  defp resolve_index(state, _number, %Tbol.AST.Literal{value: v, type: :integer}) do
    v
  end

  defp resolve_index(_state, number, _), do: number

  # Clamp I register index to 1-8
  defp clamp_register_index(n) when n < 1, do: 1
  defp clamp_register_index(n) when n > 8, do: 8
  defp clamp_register_index(n), do: n

  # Clamp P register index to 0-8
  defp clamp_p_index(n) when n < 0, do: 0
  defp clamp_p_index(n) when n > 8, do: 8
  defp clamp_p_index(n), do: n

  # Clamp integer to TBOL range
  defp clamp_integer(n) when n > 32767, do: 32767
  defp clamp_integer(n) when n < -32767, do: -32767
  defp clamp_integer(n), do: n
end
