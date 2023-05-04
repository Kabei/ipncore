defmodule Ippan.Store.KV do
  @callback child_spec(term) :: term()
  @callback start_link(term) :: {:ok, pid()}
  @callback create() :: :ok | {:error, term()}
  @callback drop() :: :ok | {:error, term()}
  @callback base() :: atom()
  @callback insert(x :: term()) :: :ok | {:error, term()}
  @callback insert_new(x :: term()) :: boolean()
  @callback exists?(term()) :: boolean()
  @callback not_exists?(term()) :: boolean()
  @callback keys() :: list()
  @callback get(key :: term) :: nil | term()
  @callback owner?(key :: term(), owner :: binary()) :: boolean()
  @callback get_if_owner(key :: term(), owner :: binary()) :: nil | term()
  @callback delete(key :: term()) :: :ok | {:error, term()}
  @callback delete_all() :: :ok
  @callback info() :: map()
  @callback size() :: non_neg_integer()
  @callback file_size() :: non_neg_integer()
end

defmodule Ippan.Store.Relational do
  @callback child_spec(term()) :: term()
  @callback start_link(term()) :: {:ok, pid()}
  @callback create() :: :ok | {:error, term()}
  @callback drop() :: :ok | {:error, term()}
  @callback base() :: atom()
  @callback insert(x :: term()) :: :ok | {:error, term()}
  @callback insert_new(x :: term()) :: boolean()
  @callback insert_all(list()) :: :ok | {:error, term()}
  @callback exists?(term()) :: boolean()
  @callback not_exists?(term()) :: boolean()
  @callback keys() :: list()
  @callback get(key :: term) :: nil | term()
  @callback all() :: list()
  @callback owner?(key :: term(), owner :: binary()) :: boolean()
  @callback get_if_owner(key :: term(), owner :: binary()) :: nil | term()
  @callback update(where :: keyword(), props :: map() | keyword()) :: :ok | {:error, term()}
  @callback delete(key :: term()) :: :ok | {:error, term()}
  # @callback delete_owner(key :: term(), owner :: binary) :: :ok | {:error, term()}
  @callback delete_all() :: non_neg_integer()
  @callback count() :: non_neg_integer()
  @callback sync() :: :ok | :error
end
