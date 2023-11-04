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
  def generate_files(creator_id, height, prev) do
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

      :ets.info(ets_msg, :size) != 0 ->
        IO.inspect("MSG Size > 0")

        ets_msg = :ets.whereis(:msg)
        cref = :counters.new(3, [])

        :ets.safe_fixtable(ets_msg, true)
        first = :ets.first(ets_msg)

        {acc_msg, acc_decode} =
          do_iterate(first, ets_msg, cref)

        :ets.safe_fixtable(ets_msg, false)

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
    bdets = DetsPlux.get(:balance)
    wdets = DetsPlux.get(:wallet)
    sdets = DetsPlux.get(:stats)
    btx = :ets.new(:balance, [:set])
    wtx = :ets.new(:wallet, [:set])
    stx = :ets.new(:supply, [:set])

    refs = %{
      balance: {bdets, btx},
      cref: cref,
      supply: {sdets, stx},
      wallet: {wdets, wtx}
    }

    do_iterate(first, ets, refs, [], [])
  end

  defp do_iterate(
         :"$end_of_table",
         _ets,
         _refs,
         acc_msg,
         acc_decode
       ) do
    # delete_refs(refs)

    {Enum.reverse(acc_msg), Enum.reverse(acc_decode)}
  end

  defp do_iterate(
         ix,
         ets_msg,
         refs = %{balance: balances, cref: cref, supply: supplies, wallet: wallets},
         acc_msg,
         acc_decode
       ) do
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

    next = :ets.next(ets_msg, ix)
    :ets.delete(ets_msg, ix)

    case check_wallet(wallets, from) and check_return(balances, supplies, return) do
      true ->
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
            # delete_refs(refs)
            {Enum.reverse(acc_msg), Enum.reverse(acc_decode)}
        end

      _false ->
        do_iterate(next, ets_msg, refs, acc_msg, acc_decode)
    end
  end

  defp check_return({bdets, btx}, {sdets, stx}, return) do
    case return do
      %{"output" => balances, "supply" => supplies} ->
        try do
          TokenSupply.multi_requires!(sdets, stx, supplies)
          BalanceStore.multi_requires!(bdets, btx, balances)
          true
        rescue
          _e -> false
        end

      %{"output" => balances} ->
        try do
          BalanceStore.multi_requires!(bdets, btx, balances)
          true
        rescue
          _e -> false
        end

      _ ->
        true
    end
  end

  defp check_wallet({dets, tx}, from) do
    case DetsPlux.get_cache(dets, tx, from) do
      {_pubkey, vid} ->
        vid == :persistent_term.get(:vid)

      _ ->
        true
    end
  end

  # defp delete_refs(refs) do
  #   Enum.each(refs, fn
  #     {_, {_, tx}} -> :ets.delete(tx)
  #     _ -> true
  #   end)
  # end
end
