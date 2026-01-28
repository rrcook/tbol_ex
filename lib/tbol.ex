defmodule Tbol do
  @moduledoc """
  TBOL (Trintex Basic Object Language) interpreter and simulator.

  TBOL is a statement-based language from the 1980s Prodigy system
  with approximately 50 core verbs for data manipulation, control flow,
  string operations, and UI interaction.
  """

  alias Tbol.{Lexer, Parser, Interpreter}
  alias Tbol.Runtime.State

  @doc """
  Parse and run a TBOL program from source code.

  ## Options
    * `:initial_state` - Initial runtime state (default: new State)
    * `:params` - List of parameters to pass (sets P1-P8, P0 = count)

  ## Examples

      iex> Tbol.run("PROGRAM test; PROC main = END_PROC")
      {:ok, %Tbol.Runtime.State{}}

  """
  def run(source, opts \\ []) do
    with {:ok, tokens} <- Lexer.tokenize(source),
         {:ok, ast} <- Parser.parse(tokens) do
      initial_state = Keyword.get(opts, :initial_state, State.new())
      params = Keyword.get(opts, :params, [])

      state = State.set_params(initial_state, params)
      Interpreter.run(ast, state)
    end
  end

  @doc """
  Parse and run a TBOL program from a file.
  """
  def run_file(path, opts \\ []) do
    case File.read(path) do
      {:ok, source} -> run(source, opts)
      {:error, reason} -> {:error, {:file_error, reason}}
    end
  end

  @doc """
  Tokenize TBOL source code without parsing.
  """
  def tokenize(source) do
    Lexer.tokenize(source)
  end

  @doc """
  Parse TBOL source code into an AST.
  """
  def parse(source) do
    with {:ok, tokens} <- Lexer.tokenize(source) do
      Parser.parse(tokens)
    end
  end

  @doc """
  Pretty-print the AST for debugging.
  """
  def inspect_ast(source) do
    with {:ok, ast} <- parse(source) do
      {:ok, inspect(ast, pretty: true, limit: :infinity)}
    end
  end
end
