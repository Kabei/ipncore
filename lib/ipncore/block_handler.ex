defmodule Ippan.BlockHandler do
  alias Ippan.{ClusterNodes, Block, Validator}

  import Ippan.Block,
    only: [decode_file!: 1, encode_file!: 1]

  require BalanceStore
  require Sqlite
  require Validator
  require Block
  require Logger

  @app Mix.Project.config()[:app]
  @version Application.compile_env(@app, :version)
  # @max_size Application.compile_env(@app, :max_block_data_size)
  @max_block_size Application.compile_env(@app, :max_block_size)

  # Generate local block and decode block file
  @spec generate_files(creator_id :: integer(), height :: integer(), prev :: binary() | nil) ::
          map() | nil
  def generate_files(creator_id, height, prev) do
    block_path =
      Block.block_path(creator_id, height)

    decode_path =
      Block.decode_path(creator_id, height)

    pool = MemPool.get()

    cond do
      File.exists?(decode_path) and File.exists?(block_path) ->
        IO.inspect("Already exists blockFile")
        {:ok, file_info} = File.stat(block_path)

        {:ok, content} = File.read(block_path)

        %{"txs" => messages, "vsn" => version} = decode_file!(content)

        filehash = Block.compute_hashfile(block_path)
        timestamp = :os.system_time(:millisecond)
        hash = Block.compute_hash(creator_id, height, prev, filehash, timestamp)
        {:ok, signature} = Block.sign(hash)

        %{
          count: length(messages),
          creator: creator_id,
          hash: hash,
          filehash: filehash,
          height: height,
          prev: prev,
          signature: signature,
          size: file_info.size,
          timestamp: timestamp,
          vsn: version
        }

      MemPool.size(pool) != 0 ->
        IO.inspect("MSG Size > 0")

        {acc_bs, acc_decode, cref} =
          MemPool.select(pool, @max_block_size)

        content = encode_file!(%{"txs" => acc_bs, "vsn" => @version})
        File.write(block_path, content)

        content = encode_file!(%{"txs" => acc_decode, "vsn" => @version})
        File.write(decode_path, content)

        {:ok, file_info} = File.stat(block_path)

        count = :counters.get(cref, 2)
        filehash = Block.compute_hashfile(block_path)
        timestamp = :os.system_time(:millisecond)
        hash = Block.compute_hash(creator_id, height, prev, filehash, timestamp)
        {:ok, signature} = Block.sign(hash)

        ClusterNodes.broadcast(%{
          "event" => "mempool",
          # "data" => %{"count" => count, "height" => height, "starts" => first, "ends" => ends}
          "data" => %{"count" => count, "height" => height}
        })

        %{
          count: count,
          creator: creator_id,
          hash: hash,
          filehash: filehash,
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

  def check(
        %{
          creator: creator_id,
          hash: hash,
          filehash: filehash,
          height: height,
          prev: prev,
          signature: signature,
          size: size,
          timestamp: timestamp,
          vsn: version
        },
        db_ref
      ) do
    try do
      %{hostname: hostname, pubkey: pubkey} = Validator.get(creator_id)
      remote_url = Block.url(hostname, creator_id, height)
      output_path = Block.block_path(creator_id, height)
      file_exists = File.exists?(output_path)

      if file_exists do
        {:ok, filestat} = File.stat(output_path)

        if filestat.size != size do
          File.rm(output_path)
          DownloadTask.start(remote_url, output_path, max_size: @max_block_size)
        else
          :ok
        end
      else
        DownloadTask.start(remote_url, output_path, max_size: @max_block_size)
      end
      |> case do
        :ok ->
          {:ok, filestat} = File.stat(output_path)

          cond do
            # match?(%{hash: ^prev}, Block.last_created(creator_id)) == false ->
            #   :error

            filestat.size > @max_block_size or filestat.size != size ->
              IO.puts("Error filesize")
              :error

            hash != Block.compute_hash(creator_id, height, prev, filehash, timestamp) ->
              IO.puts("Error hash")
              IO.inspect(creator_id)
              IO.inspect(height)
              IO.inspect(prev)
              IO.inspect(filehash)
              IO.inspect(timestamp)
              :error

            filehash != Block.compute_hashfile(output_path) ->
              IO.puts("Error filehash")
              :error

            Cafezinho.Impl.verify(signature, hash, pubkey) != :ok ->
              IO.puts("Error block signature")
              :error

            @version != version ->
              IO.puts("Error version")
              :error

            true ->
              :ok
          end

        _error ->
          IO.puts("Error download file")
          :error
      end
    rescue
      err ->
        Logger.error(Exception.format(:error, err, __STACKTRACE__))
        :error
    end
  end

  def check(_, _db_ref), do: :error
end
