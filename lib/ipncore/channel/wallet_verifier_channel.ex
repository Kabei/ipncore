defmodule WalletVerifierChannel do
  alias Ippan.Wallet

  use Channel,
    server: :verifiers,
    topic: "wallet"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @topic)
    {:ok, args}
  end

  @impl true
  def handle_info({"new", account}, state) do
    WalletStore.insert(Wallet.to_list(account))
    {:noreply, state}
  end

  def handle_info({"sub", %{id: account_id, validator: validator_id}}, state) do
    WalletStore.update(%{validator: validator_id}, id: account_id)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
