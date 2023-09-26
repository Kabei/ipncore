defmodule Ippan.BlockHandler do
  alias Ippan.Validator
  alias Ippan.{Block, TxHandler}

  import Ippan.Block,
    only: [decode_file!: 1, encode_file!: 1, hash_file: 1]

  require SqliteStore

  @version Application.compile_env(:ipncore, :version)
  @block_extension Application.compile_env(:ipncore, :block_extension)
  @max_block_size Application.compile_env(:ipncore, :block_max_size)
  @max_block_data_size Application.compile_env(:ipncore, :max_block_data_size)

  # Generate local block and decode block file
  @spec generate_files(creator_id :: integer(), height :: integer(), prev_hash :: binary()) ::
          map | nil
  def generate_files(creator_id, height, prev, priority \\ 0) do
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

        {acc_msg, acc_decode} =
          do_iterate(ets_msg, :ets.first(ets_msg), %{}, %{}, 0)

        content = encode_file!(%{"data" => acc_msg, "vsn" => @version})

        File.write(block_path, content)
        File.write(decode_path, encode_file!(%{"data" => acc_decode, "vsn" => @version}))

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

  defp do_iterate(_ets_msg, :"$end_of_table", messages, decode_messages, _),
    do: {Map.values(messages), Map.values(decode_messages)}

  defp do_iterate(ets_msg, key, messages, decode_message, acc_size) do
    [msg] = :ets.lookup(ets_msg, key)

    {acc_msg, acc_decode, size} =
      case msg do
        {
          hash,
          type,
          from,
          args,
          timestamp,
          msg_sig,
          size
        } ->
          acc_msg = Map.put(messages, hash, msg_sig)

          acc_decode =
            Map.put(decode_message, hash, [
              hash,
              type,
              from,
              args,
              timestamp,
              size
            ])

          {acc_msg, acc_decode, size}

        {
          hash,
          type,
          _key,
          from,
          args,
          timestamp,
          msg_sig,
          size
        } ->
          acc_msg = Map.put(messages, hash, msg_sig)

          acc_decode =
            Map.put(decode_message, hash, [
              hash,
              type,
              from,
              args,
              timestamp,
              size
            ])

          {acc_msg, acc_decode, size}
      end

    acc_size = acc_size + size
    :ets.delete(ets_msg, key)

    case @max_block_data_size > acc_size do
      false ->
        do_iterate(ets_msg, :ets.next(ets_msg, key), acc_msg, acc_decode, acc_size)

      _true ->
        {Map.values(acc_msg), Map.values(acc_decode)}
    end
  end

  @spec verify_file!(map) :: :ok | :error
  def verify_file!(%{
        "count" => count,
        "creator" => creator_id,
        "hash" => hash,
        "hashfile" => hashfile,
        "height" => height,
        "hostname" => hostname,
        "prev" => prev,
        "pubkey" => pubkey,
        "signature" => signature,
        "size" => size,
        "timestamp" => timestamp,
        "vsn" => version
      }) do
    try do
      remote_url = Block.url(hostname, creator_id, height)
      output_path = Block.block_path(creator_id, height)
      file_exists = File.exists?(output_path)
      filename = Path.basename(output_path)

      unless file_exists do
        :ok = Download.from(remote_url, output_path, @max_block_size)
      else
        {:ok, filestat} = File.stat(output_path)

        if filestat.size != size do
          :ok = Download.from(remote_url, output_path, @max_block_size)
        end
      end

      {:ok, filestat} = File.stat(output_path)

      cond do
        filestat.size > @max_block_size or filestat.size != size ->
          raise IppanError, "Invalid block size"

        hash != Block.compute_hash(creator_id, height, prev, hashfile, timestamp) ->
          raise(IppanError, "Invalid block hash")

        hashfile != hash_file(output_path) ->
          raise(IppanError, "Hash block file is invalid")

        Cafezinho.Impl.verify(signature, hash, pubkey) != :ok ->
          raise(IppanError, "Invalid block signature")

        @version != version ->
          raise(IppanError, "Invalid block version")

        true ->
          {:ok, content} = File.read(output_path)
          %{"vsn" => vsn, "data" => messages} = decode_file!(content)

          if vsn != version do
            raise(IppanError, "Invalid blockfile version")
          end

          conn = :persistent_term.get(:asset_conn)
          stmts = :persistent_term.get(:asset_stmt)

          validator =
            SqliteStore.lookup_map(
              :validator,
              conn,
              stmts,
              "get_validator",
              creator_id,
              Validator
            )

          decode_msgs =
            Enum.reduce(messages, %{}, fn [msg, sig], acc ->
              hash = Blake3.hash(msg)
              size = byte_size(msg) + byte_size(sig)

              try do
                msg =
                  TxHandler.valid_from_file!(
                    conn,
                    stmts,
                    hash,
                    msg,
                    sig,
                    size,
                    creator_id,
                    validator
                  )

                IO.inspect(msg)

                Map.put(acc, hash, msg)
              catch
                _ -> acc
              end
            end)
            |> Map.values()

          if count != length(decode_msgs) do
            raise IppanError, "Invalid block messages count"
          end

          export_path = Path.join(:persistent_term.get(:decode_dir), filename)

          :ok = File.write(export_path, encode_file!(%{"data" => decode_msgs, "vsn" => version}))
      end
    rescue
      error ->
        IO.inspect(error)
        :error
    end
  end

  def verify_file!(_), do: :error
end
