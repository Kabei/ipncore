defmodule ValidatorVerifierChannel do
  alias Ippan.Validator

  use Channel,
    server: :verifiers,
    channel: "validator"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    {:ok, args}
  end

  @impl true
  def handle_info({"new", validator}, state) do
    ValidatorStore.insert(Validator.to_list(validator))
    {:noreply, state}
  end

  def handle_info({"update", id, opts}, state) do
    ValidatorStore.update(id, opts)
    {:noreply, state}
  end

  def handle_info({"delete", validator_id}, state) do
    ValidatorStore.delete(validator_id)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
