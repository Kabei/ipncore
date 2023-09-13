defmodule BlockHandler do
  # alias Ippan.Block

  import Ippan.Block,
    only: [encode_file!: 1, hash_file: 1]

  @block_extension Application.compile_env(:ipncore, :block_extension)

  # Generate local block and decode block file
  def generate_files(creator_id, block_id) do
    filename = "#{creator_id}.#{block_id}.#{@block_extension}"
    block_path = Path.join(Application.get_env(:ipncore, :block_dir), filename)
    decode_path = Path.join(Application.get_env(:ipncore, :decode_dir), filename)
    ets_msg = :ets.whereis(:msg)
    ets_dmsg = :ets.whereis(:dmsg)

    messages = :ets.tab2list(ets_msg)
    decode_messages = :ets.tab2list(ets_dmsg)

    last_key =
      List.last(messages)
      |> elem(1)

    last_dkey =
      List.last(decode_messages)
      |> elem(1)

    File.write(block_path, encode_file!(messages))

    File.write(decode_path, encode_file!(decode_messages))

    ets_delete_while(ets_msg, last_key, :ets.first(ets_msg))
    ets_delete_while(ets_dmsg, last_dkey, :ets.first(ets_dmsg))

    {:ok, file_info} = File.stat(block_path)

    %{
      count: length(messages),
      creator: creator_id,
      hash: hash_file(block_path),
      height: block_id,
      size: file_info.size
    }
  end

  defp ets_delete_while(_table, _target, :"$end_of_table"), do: :ok

  defp ets_delete_while(table, target, key) do
    if key != target do
      next = :ets.next(table, key)
      :ets.delete(table, key)
      ets_delete_while(table, target, next)
    else
      :ets.delete(table, key)
    end
  end
end
