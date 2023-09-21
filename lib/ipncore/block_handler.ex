defmodule BlockHandler do
  # alias Ippan.Block

  import Ippan.Block,
    only: [decode_file!: 1, encode_file!: 1, hash_file: 1]

  @version Application.compile_env(:ipncore, :version)
  @block_extension Application.compile_env(:ipncore, :block_extension)
  @max_block_data_size Application.compile_env(:ipncore, :max_block_data_size)

  # Generate local block and decode block file
  @spec generate_files(creator_id :: integer(), height :: integer()) :: map | nil
  def generate_files(creator_id, height) do
    filename = "#{creator_id}.#{height}.#{@block_extension}"
    block_path = Path.join(:persistent_term.get(:block_dir), filename)
    decode_path = Path.join(:persistent_term.get(:decode_dir), filename)
    ets_msg = :ets.whereis(:msg)

    cond do
      File.exists?(decode_path) and File.exists?(block_path) ->
        IO.inspect("Already exists blockFiles")
        {:ok, file_info} = File.stat(block_path)

        {:ok, content} = File.read(block_path)

        %{"data" => messages, "vsn" => version} = decode_file!(content)

        %{
          count: length(messages),
          creator: creator_id,
          hashfile: hash_file(block_path),
          height: height,
          size: file_info.size,
          vsn: version
        }

      :ets.info(ets_msg, :size) > 0 ->
        IO.inspect("MSG Size > 0")

        {acc_msg, acc_decode} =
          do_iterate(ets_msg, :ets.first(ets_msg), %{}, %{}, 0)

        content = encode_file!(%{"data" => acc_msg, "vsn" => @version})

        File.write(block_path, content)
        File.write(decode_path, encode_file!(%{"data" => acc_decode, "vsn" => @version}))

        {:ok, file_info} = File.stat(block_path)

        %{
          count: length(acc_msg),
          creator: creator_id,
          hashfile: hash_file(block_path),
          height: height,
          size: file_info.size,
          vsn: @version
        }

      true ->
        IO.inspect("No there TXS")
        nil
    end
  end

  defp do_iterate(_ets_msg, :"$end_of_table", messages, decode_messages, _),
    do: {Map.values(messages), Map.values(decode_messages)}

  defp do_iterate(ets_msg, key, messages, decode_message, acc_size) do
    [msg] = :ets.lookup(ets_msg, key)

    {acc_msg, acc_decode, size} =
      case msg do
        {
          hash,
          timestamp,
          type,
          from,
          args,
          msg_sig,
          size
        } ->
          acc_msg = Map.put(messages, hash, msg_sig)

          acc_decode =
            Map.put(decode_message, hash, [
              hash,
              timestamp,
              type,
              from,
              args,
              size
            ])

          {acc_msg, acc_decode, size}

        {
          hash,
          timestamp,
          _key,
          type,
          from,
          args,
          msg_sig,
          size
        } ->
          acc_msg = Map.put(messages, hash, msg_sig)

          acc_decode =
            Map.put(decode_message, hash, [
              hash,
              timestamp,
              type,
              from,
              args,
              size
            ])

          {acc_msg, acc_decode, size}
      end

    acc_size = acc_size + size

    case @max_block_data_size > acc_size do
      false ->
        :ets.delete(ets_msg, key)
        do_iterate(ets_msg, :ets.next(ets_msg, key), acc_msg, acc_decode, acc_size)

      _true ->
        {Map.values(acc_msg), Map.values(acc_decode)}
    end
  end
end
