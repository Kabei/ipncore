defmodule Ipncore.IMP.Client do
  use IMP.Conn
  alias Ipncore.{Tx, TxVote, Block, Chain}

  # @channel Application.get_env(:ipncore, :channel)

  @impl true
  def start(state) do
    {:ok, state}
  end

  @impl true
  def connection(state) do
    {:ok, state}
  end

  @impl true
  def on_message("new_tx", %{"id" => index} = payload, state) do
    # vote =
    #   case Tx.processing(payload) do
    #     {:ok, %{"id" => txid}} ->
    #       TxVote.new_approved(txid, state.address, state.falcon_pk, state.falcon_sk)

    #     _ ->
    #       TxVote.new_cancelled(index, state.address, state.falcon_pk, state.falcon_sk)
    #   end

    # Ipncore.IMP.Client.publish("tx:" <> payload.id, vote, state.skey)

    {:ok, state}
  end

  def on_message("tx" <> txid, payload, state) when is_binary(payload) do
    # Tx.put_bft(txid, payload, Default.channel())
    {:ok, state}
  end

  # def on_message("build", payload, state) do
  #   Logger.debug("build #{inspect(payload)}")

  #   block = Chain.build_next(Default.channel())
  #   Logger.debug("after next")

  #   case block do
  #     nil ->
  #       Logger.debug("build block empty")
  #       nil

  #     block ->
  #       Ipncore.IMP.Client.publish(
  #         "block:#{block.index}",
  #         Block.from_struct(block)
  #       )
  #   end

  #   {:ok, state}
  # end

  # def on_message("build", payload, state) do
  #   Logger.debug("build #{inspect(payload)}")

  #   blocks = Chain.build_all(Default.channel())
  #   Logger.debug("after next")

  #   case blocks do
  #     nil ->
  #       Logger.debug("build block empty")

  #     [] ->
  #       Logger.debug("build block empty")

  #     blocks ->
  #       Enum.each(blocks, fn block ->
  #         Ipncore.IMP.Client.publish(
  #           "block:#{block.index}",
  #           Block.from_struct(block)
  #         )
  #       end)
  #   end

  #   {:ok, state}
  # end

  def on_message(_, _, state) do
    Logger.debug("msg")
    {:ok, state}
  end

  @impl true
  def subscription(_topic, state) do
    {:ok, state}
  end

  @impl true
  def unsubscription(_topic, state) do
    {:ok, state}
  end

  @impl true
  def disconnected(_state) do
    :ok
  end

  @impl true
  def error(_command, _params, _state) do
    :ok
  end
end
