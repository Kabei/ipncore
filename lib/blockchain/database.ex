defmodule Ipncore.Database do
  @doc """
  Open local database
  """
  @callback open() :: {:ok, term} | {:error, any}

  @doc """
  Close database
  """
  @callback close() :: :ok

  @callback put!(term) :: boolean

  # @callback fetch(term) :: term | nil

  @callback fetch!(term) :: term
end
