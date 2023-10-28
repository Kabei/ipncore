defmodule Ippan.BlockHandler do
  alias Ippan.Block

  import Ippan.Block,
    only: [decode_file!: 1, encode_file!: 1, hash_file: 1]

  require Sqlite

  @app Mix.Project.config()[:app]
  @version Application.compile_env(@app, :version)
  @block_extension Application.compile_env(@app, :block_extension)
  @decode_extension Application.compile_env(@app, :decode_extension)
  @max_block_data_size Application.compile_env(@app, :max_block_data_size)

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

        ets_dmsg = :ets.whereis(:dmsg)

        :ets.tab2list(ets_msg) |> IO.inspect()
        :ets.tab2list(ets_dmsg) |> IO.inspect()

        {acc_msg, acc_decode} =
          do_iterate(:ets.first(ets_msg), ets_msg, ets_dmsg, [], [], 0)

        content = encode_file!(%{"data" => acc_msg, "vsn" => @version})

        File.write(block_path, content)

        File.write(
          decode_path,
          encode_file!(%{"data" => acc_decode, "vsn" => @version})
        )

        {:ok, file_info} = File.stat(block_path)

        hashfile = hash_file(block_path)
        timestamp = :os.system_time(:millisecond)
        hash = Block.compute_hash(creator_id, height, prev, hashfile, timestamp)
        {:ok, signature} = Block.sign(hash)

        %{
          count: length(acc_msg),
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

  defp do_iterate(
         :"$end_of_table",
         _ets_msg,
         _ets_dmsg,
         acc_msg,
         acc_dmsg,
         _
       ),
       do: {Enum.reverse(acc_msg), Enum.reverse(acc_dmsg)}

  defp do_iterate(key, ets_msg, ets_dmsg, acc_msg, acc_dmsg, acc_size) do
    [{_, msg}] = :ets.lookup(ets_msg, key)
    [{_, dmsg}] = :ets.lookup(ets_dmsg, key)

    :ets.delete(ets_msg, key)
    :ets.delete(ets_dmsg, key)
    acc_size = acc_size + :lists.last(dmsg)
    acc_msg = [msg | acc_msg]
    acc_dmsg = [dmsg | acc_dmsg]

    case @max_block_data_size > acc_size do
      true ->
        do_iterate(
          :ets.next(ets_msg, key),
          ets_msg,
          ets_dmsg,
          acc_msg,
          acc_dmsg,
          acc_size
        )

      false ->
        {Enum.reverse(acc_msg), Enum.reverse(acc_dmsg)}
    end
  end
end
