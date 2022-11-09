defmodule Ipncore.Database do
  @doc """
  Open local database
  """
  @callback open(String.t()) :: {:ok, term} | {:error, any}

  @doc """
  Close database
  """
  @callback close(String.t()) :: :ok

  @callback put!(term) :: boolean

  # @callback fetch(term) :: term | nil

  @callback fetch!(term) :: term
end
