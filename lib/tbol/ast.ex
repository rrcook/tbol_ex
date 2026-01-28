defmodule Tbol.AST do
  @moduledoc """
  AST node definitions for TBOL programs.
  """

  # Program structure
  defmodule Program do
    @moduledoc "Top-level program node"
    defstruct [:name, defines: [], copies: [], procedures: []]
  end

  defmodule Procedure do
    @moduledoc "PROC name = ... END_PROC"
    defstruct [:name, statements: []]
  end

  defmodule Define do
    @moduledoc "DEFINE name, value;"
    defstruct [:name, :value]
  end

  defmodule Copy do
    @moduledoc "COPY copybook_name;"
    defstruct [:name]
  end

  # Data movement statements
  defmodule Move do
    @moduledoc "MOVE source, destination [, ABS];"
    defstruct [:source, :destination, abs: false]
  end

  defmodule Clear do
    @moduledoc "CLEAR destination or CLEAR start, end (range);"
    defstruct [:destination, :end_destination]
  end

  # Arithmetic statements
  defmodule Add do
    @moduledoc "ADD value, destination;"
    defstruct [:value, :destination]
  end

  defmodule Subtract do
    @moduledoc "SUBTRACT value, destination;"
    defstruct [:value, :destination]
  end

  defmodule Multiply do
    @moduledoc "MULTIPLY value, destination;"
    defstruct [:value, :destination]
  end

  defmodule Divide do
    @moduledoc "DIVIDE value, destination [, remainder];"
    defstruct [:value, :destination, :remainder]
  end

  # Control flow statements
  defmodule If do
    @moduledoc "IF condition THEN clause [ELSE clause]"
    defstruct [:condition, :then_clause, :else_clause]
  end

  defmodule While do
    @moduledoc "WHILE condition DO statements END;"
    defstruct [:condition, :body]
  end

  defmodule Goto do
    @moduledoc "GOTO label;"
    defstruct [:label]
  end

  defmodule GotoDependingOn do
    @moduledoc "GOTO_DEPENDING_ON index, label1, label2, ...;"
    defstruct [:index, labels: []]
  end

  defmodule Label do
    @moduledoc "label: (marks a position for GOTO)"
    defstruct [:name]
  end

  defmodule DoBlock do
    @moduledoc "DO statements END; (compound statement)"
    defstruct [statements: []]
  end

  # Procedure calls
  defmodule Link do
    @moduledoc "LINK target [, params...];"
    defstruct [:target, params: []]
  end

  defmodule Transfer do
    @moduledoc "TRANSFER target [, params...];"
    defstruct [:target, params: []]
  end

  defmodule Return do
    @moduledoc "RETURN;"
    defstruct []
  end

  defmodule Exit do
    @moduledoc "EXIT;"
    defstruct []
  end

  # String operations
  defmodule StringOp do
    @moduledoc "STRING dest, src1 [, src2, ...];"
    defstruct [:destination, sources: []]
  end

  defmodule Substr do
    @moduledoc "SUBSTR source, start, length, destination;"
    defstruct [:source, :start, :length, :destination]
  end

  defmodule Instr do
    @moduledoc "INSTR source, search, destination;"
    defstruct [:source, :search, :destination]
  end

  defmodule Length do
    @moduledoc "LENGTH source, destination;"
    defstruct [:source, :destination]
  end

  defmodule Uppercase do
    @moduledoc "UPPERCASE source, destination;"
    defstruct [:source, :destination]
  end

  # Bitwise operations
  defmodule And do
    @moduledoc "AND value1, value2, destination;"
    defstruct [:value1, :value2, :destination]
  end

  defmodule Or do
    @moduledoc "OR value1, value2, destination;"
    defstruct [:value1, :value2, :destination]
  end

  defmodule Xor do
    @moduledoc "XOR value1, value2, destination;"
    defstruct [:value1, :value2, :destination]
  end

  # Stack operations
  defmodule Push do
    @moduledoc "PUSH value;"
    defstruct [:value]
  end

  defmodule Pop do
    @moduledoc "POP destination;"
    defstruct [:destination]
  end

  # Persistence
  defmodule Save do
    @moduledoc "SAVE block_name, start, end;"
    defstruct [:block_name, :start, :end]
  end

  defmodule Restore do
    @moduledoc "RESTORE block_name, start;"
    defstruct [:block_name, :start]
  end

  defmodule Release do
    @moduledoc "RELEASE block_name;"
    defstruct [:block_name]
  end

  # Lookup and data operations
  defmodule Lookup do
    @moduledoc "LOOKUP key, table, result [, not_found];"
    defstruct [:key, :table, :result, :not_found]
  end

  defmodule Fill do
    @moduledoc "FILL value, start, count;"
    defstruct [:value, :start, :count]
  end

  defmodule Edit do
    @moduledoc "EDIT source, format, destination;"
    defstruct [:source, :format, :destination]
  end

  defmodule Format do
    @moduledoc "FORMAT source, format_string, destination;"
    defstruct [:source, :format_string, :destination]
  end

  defmodule MakeFormat do
    @moduledoc "MAKE_FORMAT source, destination;"
    defstruct [:source, :destination]
  end

  defmodule Sort do
    @moduledoc "SORT start, count, width [, direction];"
    defstruct [:start, :count, :width, direction: :ascending]
  end

  # I/O operations (stubs)
  defmodule Open do
    @moduledoc "OPEN file, mode;"
    defstruct [:file, :mode]
  end

  defmodule Write do
    @moduledoc "WRITE file, data;"
    defstruct [:file, :data]
  end

  defmodule Close do
    @moduledoc "CLOSE file;"
    defstruct [:file]
  end

  defmodule Send do
    @moduledoc "SEND destination, data;"
    defstruct [:destination, :data]
  end

  defmodule Receive do
    @moduledoc "RECEIVE source, destination;"
    defstruct [:source, :destination]
  end

  # UI operations (stubs)
  defmodule OpenWindow do
    @moduledoc "OPEN_WINDOW window_id;"
    defstruct [:window_id, :params]
  end

  defmodule CloseWindow do
    @moduledoc "CLOSE_WINDOW [window_id];"
    defstruct [:window_id]
  end

  defmodule SetCursor do
    @moduledoc "SET_CURSOR field;"
    defstruct [:field]
  end

  defmodule SetAttribute do
    @moduledoc "SET_ATTRIBUTE field, attribute;"
    defstruct [:field, :attribute]
  end

  defmodule SetFunction do
    @moduledoc "SET_FUNCTION key, action;"
    defstruct [:key, :action]
  end

  defmodule Navigate do
    @moduledoc "NAVIGATE target;"
    defstruct [:target, :params]
  end

  defmodule Fetch do
    @moduledoc "FETCH object_id;"
    defstruct [:object_id, :params]
  end

  defmodule Refresh do
    @moduledoc "REFRESH [field];"
    defstruct [:field]
  end

  defmodule Timeout do
    @moduledoc "TIMEOUT value;"
    defstruct [:value]
  end

  defmodule Priority do
    @moduledoc "PRIORITY value;"
    defstruct [:value]
  end

  defmodule OptHdrs do
    @moduledoc "OPT_HDRS option;"
    defstruct [:option]
  end

  # Expressions
  defmodule BinaryOp do
    @moduledoc "Binary operation: left op right"
    defstruct [:op, :left, :right]
  end

  defmodule UnaryOp do
    @moduledoc "Unary operation: op operand"
    defstruct [:op, :operand]
  end

  defmodule ScreenField do
    @moduledoc "&number or &number(subscript) - partition external variable"
    defstruct [:number, :subscript]
  end

  defmodule Register do
    @moduledoc "I1-I8, D1-D8, P0-P8 registers"
    defstruct [:type, :number, :subscript]
  end

  defmodule RdaRef do
    @moduledoc "RDA slot reference: RDA0-RDA118+"
    defstruct [:slot, :subscript]
  end

  defmodule GevRef do
    @moduledoc "Global external variable: SYS_*, PRF_* prefixed names"
    defstruct [:name]
  end

  defmodule Identifier do
    @moduledoc "Named identifier (may be DEFINE alias)"
    defstruct [:name]
  end

  defmodule Literal do
    @moduledoc "Literal value: string, integer, or hex"
    defstruct [:value, :type]
  end
end
