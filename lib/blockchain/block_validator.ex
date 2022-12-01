defmodule Ipncore.BlockValidator do
  alias Ipncore.Block

  @type validation_type :: :ok | {:error, atom}

  ## block validation
  @spec valid_block?(Block.t() | nil, Block.t()) :: validation_type()
  def valid_block?(_prev_block, nil), do: {:error, :block_null}

  def valid_block?(prev_block, block) do
    with :ok <- valid_block_version?(block),
         :ok <- valid_block_height?(block),
         #  :ok <- valid_block_index?(block),
         #  :ok <- valid_block_time?(block.time),
         :ok <- valid_with_prev_block?(prev_block, block) do
      #  :ok <- valid_block_hash?(block),
      #  :ok <- valid_block_type?(prev_block, block)
      #  :ok <- valid_merkle_root?(block.mk, block.events)
      :ok
    else
      err -> err
    end
  end

  defp valid_block_version?(block) do
    if block.vsn == Block.version() do
      :ok
    else
      {:error, :bad_block_version}
    end
  end

  defp valid_block_height?(block) do
    cond do
      block.height < 0 ->
        {:error, :invalid_block_height}

      block.type == 0 and block.height != 0 ->
        {:error, :invalid_block_type}

      true ->
        :ok
    end
  end

  # defp valid_block_index?(block) do
  #   cond do
  #     block.height < 0 ->
  #       {:error, :invalid_block_index}

  #     block.type == 0 and block.height != 0 ->
  #       {:error, :invalid_block_type}

  #     true ->
  #       :ok
  #   end
  # end

  # defp valid_block_time?(block_time) do
  #   iit = Chain.get_time()

  #   cond do
  #     block_time > iit ->
  #       {:error, :invalid_block_time}

  #     true ->
  #       :ok
  #   end
  # end

  # @spec valid_block_type?(Block.t() | nil, Block.t()) :: validation_type()
  # defp valid_block_type?(nil, %{type: 0}), do: :ok
  # defp valid_block_type?(nil, %{type: _}), do: {:error, :invalid_block_regular}
  # defp valid_block_type?(%Block{}, %{type: 0}), do: {:error, :invalid_block_genesis}
  # defp valid_block_type?(%Block{}, %{type: _}), do: :ok
  # defp valid_block_type?(_, _), do: {:error, :invalid_block_type}

  @spec valid_with_prev_block?(Block.t(), Block.t()) :: validation_type()
  def valid_with_prev_block?(nil, _block), do: :ok

  def valid_with_prev_block?(prev_block, block) do
    IO.inspect(block)
    IO.inspect(prev_block)

    cond do
      prev_block.height >= block.height ->
        {:error, :invalid_block_height}

      prev_block.time > block.time ->
        {:error, :invalid_block_time}

      prev_block.hash != block.prev ->
        {:error, :invalid_block_previous_hash}

      true ->
        :ok
    end
  end

  @spec valid_merkle_root?(binary, list) :: validation_type()
  def valid_merkle_root?(_merkle_root, nil), do: {:error, :no_events}
  def valid_merkle_root?(_merkle_root, []), do: {:error, :no_events}

  def valid_merkle_root?(merkle_root, events) do
    if byte_size(merkle_root) == 32 do
      compute_root =
        events
        |> Enum.map(& &1.hash)
        |> MerkleTree.root()

      if compute_root == merkle_root, do: :ok, else: {:error, :invalid_merkle_root}
    else
      {:error, :merkle_root_format}
    end
  end

  # defp valid_block_hash?(%{hash: block_hash} = block) do
  #   cond do
  #     byte_size(block_hash) != 32 ->
  #       {:error, :bad_hash}

  #     Block.compute_hash(block) != block_hash ->
  #       {:error, :wrong_hash}

  #     true ->
  #       :ok
  #   end
  # end

  # defp valid_block_hash?(_), do: {:error, :wrong_hash}

  ## transaction validation

  # @spec valid_event?(Event.t(), Block.t() | nil) :: validation_type()
  # def valid_event?(event, prev_block) do
  #   with :ok <- valid_event_version?(event),
  #        :ok <- valid_event_hash?(event),
  #        :ok <- valid_event_index?(event, prev_block),
  #        :ok <- valid_event_timestamp?(event, prev_block.time) do
  #     :ok
  #   else
  #     err -> err
  #   end
  # end

  # @spec valid_event_version?(Event.t()) :: validation_type()
  # defp valid_event_version?(event) do
  #   if Event.version() == event.vsn, do: :ok, else: {:error, :invalid_event_version}
  # end

  # @spec valid_event_hash?(Event.t()) :: validation_type()
  # defp valid_event_hash?(event) do
  #   cond do
  #     byte_size(event.hash) != 32 ->
  #       {:error, :bad_event_hash}

  #     # Event.calc_hash(event) != event.hash ->
  #     #   {:error, :invalid_event_hash}

  #     true ->
  #       :ok
  #   end
  # end

  # defp valid_event_index?(event, prev_block) do
  #   cond do
  #     prev_block.height < event.block_index ->
  #       {:error, :invalid_event_block_index}

  #     # is_nil(event.id) ->
  #     #   {:error, :invalid_event_id}

  #     # Event.generate_id(event) != event.id ->
  #     #   {:error, :invalid_event_id}

  #     true ->
  #       :ok
  #   end
  # end

  # defp valid_event_timestamp?(%{time: time}, prev_block_time) do
  #   # iit = Chain.get_time()

  #   cond do
  #     prev_block_time > time ->
  #       {:error, :invalid_event_timestamp}

  #     # iit < time ->
  #     #   {:error, :invalid_event_iit}

  #     # abs(iit - time) > Event.timeout() ->
  #     #   {:error, :invalid_event_timeout}

  #     true ->
  #       :ok
  #   end
  # end

  # @spec valid_coinbase?(Tx.t()) :: :ok | {:error, atom()}
  # defp valid_coinbase?(tx) do
  #   if tx.inputs == nil or tx.inputs == [], do: :ok, else: {:error, :invalid_coinbase}
  # end
end
