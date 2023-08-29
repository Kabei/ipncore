defmodule NetworkBehaviour do
  @callback on_connect(node_id :: term(), map :: map()) :: any()
  @callback on_disconnect(node_id :: term()) :: any()
  @callback on_message(node_id :: term(), socket :: term(), packet :: term(), sharedkey :: binary) ::
              any()
  @callback connect(node :: term(), opts :: keyword()) :: term() | {:error, term()}
  @callback disconnect(node_id :: term()) :: :ok
  @callback fetch(id :: term()) :: map() | nil
  @callback info(node_id :: term()) :: map() | nil
  @callback list() :: [term()]
  @callback alive?(node :: term()) :: boolean()
  @callback count() :: pos_integer()
  @callback cast(node :: term(), message :: term) :: :ok | :disconnect
  @callback call(node :: term(), message :: term) :: {:ok, term()} | {:error, term()}
  @callback broadcast(message :: term()) :: :ok
  # @callback broadcast(message :: term(), role :: binary()) :: :ok
  @callback broadcast_except(message :: term(), ids :: list()) :: :ok
  @callback handle_request(method :: binary(), from :: term(), data :: map()) :: term()
  @callback handle_message(event :: binary(), from :: term(), data :: term()) :: any()
end
