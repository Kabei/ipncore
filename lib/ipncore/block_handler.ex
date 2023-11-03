defmodule Ippan.BlockHandler do
  alias Ippan.{ClusterNodes, Block}

  import Ippan.Block,
    only: [decode_file!: 1, encode_file!: 1, hash_file: 1]

  require BalanceStore
  require Sqlite

  @app Mix.Project.config()[:app]
  @version Application.compile_env(@app, :version)
  @block_extension Application.compile_env(@app, :block_extension)
  @decode_extension Application.compile_env(@app, :decode_extension)
  @max_size Application.compile_env(@app, :max_block_data_size)

  # Generate local block and decode block file
  @spec generate_files(creator_id :: integer(), height :: integer(), prev_hash :: binary()) ::
          map | nil
  def generate_files(creator_id, height, prev, priority \\ 0) do
    block_path =
      Path.join(:persistent_term.get(:block_dir), "#{creator_id}.#{height}.#{@block_extension}")

    decode_path =
      Path.join(:persistent_term.get(:decode_dir), "#{creator_id}.#{height}.#{@decode_extension}")

    ets_msg = :ets.whereis(:msg)

    cond do
      File.exists?(decode_path) and File.exists?(block_path) ->
        IO.inspect("Already exists blockFiles")
        {:ok, file_info} = File.stat(block_path)

        {:ok, content} = File.read(block_path)

        %{"data" => messages, "vsn" => version} = decode_file!(content)

        hashfile = hash_file(block_path)
        timestamp = :os.system_time(:millisecond)
        hash = Block.compute_hash(creator_id, height, prev, hashfile, timestamp)
        {:ok, signature} = Block.sign(hash)

        %{
          count: length(messages),
          creator: creator_id,
          hash: hash,
          hashfile: hashfile,
          height: height,
          prev: prev,
          signature: signature,
          size: file_info.size,
          timestamp: timestamp,
          vsn: version
        }

      (0 == priority and :ets.info(ets_msg, :size) >= 1_000_000) or
          (1 == priority and :ets.info(ets_msg, :size) > 0) ->
        IO.inspect("MSG Size > 0")

        ets_msg = :ets.whereis(:msg)

        cref = :counters.new(3, [])
        first = :ets.first(ets_msg)

        {acc_msg, acc_decode} =
          do_iterate(first, ets_msg, cref)

        ends = :counters.get(cref, 2)
        count = :counters.get(cref, 3)

        content = encode_file!(%{"data" => acc_msg, "vsn" => @version})
        File.write(block_path, content)

        content = encode_file!(%{"data" => acc_decode, "vsn" => @version})
        File.write(decode_path, content)

        {:ok, file_info} = File.stat(block_path)

        hashfile = hash_file(block_path)
        timestamp = :os.system_time(:millisecond)
        hash = Block.compute_hash(creator_id, height, prev, hashfile, timestamp)
        {:ok, signature} = Block.sign(hash)

        ClusterNodes.broadcast(%{
          "event" => "mempool",
          "data" => %{"count" => count, "height" => height, "starts" => first, "ends" => ends}
        })

        %{
          count: count,
          creator: creator_id,
          hash: hash,
          hashfile: hashfile,
          height: height,
          prev: prev,
          signature: signature,
          size: file_info.size,
          timestamp: timestamp,
          vsn: @version
        }

      true ->
        IO.inspect("No there TXS")
        nil
    end
  end

  defp do_iterate(first, ets, cref) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :tmp)
    do_iterate(first, ets, {dets, tx, cref}, [], [])
  end

  defp do_iterate(
         :"$end_of_table",
         _ets_msg,
         _refs,
         acc_msg,
         acc_decode
       ),
       do: {Enum.reverse(acc_msg), Enum.reverse(acc_decode)}

  defp do_iterate(ix, ets_msg, refs = {dets, tx, cref}, acc_msg, acc_decode) do
    {decode, from, msg_sig, return, size} =
      case :ets.lookup(ets_msg, ix) do
        [{_ix, 0, decode = [_hash, _type, from, nonce, _args, size], msg_sig, return}] ->
          :ets.delete(:hash, {from, nonce})
          {decode, from, msg_sig, return, size}

        [{_ix, 1, decode = [_hash, type, key, from, nonce, _args, size], msg_sig, return}] ->
          :ets.delete(:hash, {from, nonce})
          :ets.delete(:dhash, {type, key})
          {decode, from, msg_sig, return, size}
      end

    :ets.delete(ets_msg, ix)
    next = :ets.next(ets_msg, ix)

    case check_wallet(from) and check_return(dets, tx, return) do
      false ->
        do_iterate(next, ets_msg, refs, acc_msg, acc_decode)

      _ ->
        :counters.add(cref, 1, size)
        :counters.put(cref, 2, ix)
        :counters.add(cref, 3, 1)

        case @max_size > :counters.get(cref, 1) do
          true ->
            do_iterate(
              next,
              ets_msg,
              refs,
              [msg_sig | acc_msg],
              [decode | acc_decode]
            )

          false ->
            {Enum.reverse(acc_msg), Enum.reverse(acc_decode)}
        end
    end
  end

  defp check_return(dets, tx, return) do
    case return do
      %{output: balances} ->
        try do
          BalanceStore.multi_requires!(dets, tx, balances)
        rescue
          _e -> false
        end

      %{output: balances, supply: supplies} ->
        try do
          BalanceStore.multi_requires!(dets, tx, balances)
          TokenSupply.multi_requires!(supplies)
        rescue
          _e -> false
        end

      _ ->
        nil
    end
  end

  defp check_wallet(from) do
    dets = DetsPlux.get(:wallet)
    tx = DetsPlux.get(:cache_wallet)
    {_pubkey, vid} = DetsPlux.get_cache(dets, tx, from)

    vid == :persistent_term.get(:vid)
  end
end
