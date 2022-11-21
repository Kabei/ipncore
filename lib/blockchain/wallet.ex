defmodule Ipncore.Wallet do
  alias Ipncore.Address

  @base "wallet"
  @file_extension ".db"
  @partitions 16

  def open do
    folder_path = Application.get_env(:ipncore, :wallet_path)

    for number <- 0..(@partitions - 1) do
      base = String.to_atom(IO.iodata_to_binary([@base, number]))
      path = Path.join(folder_path, IO.iodata_to_binary([to_string(base), @file_extension]))
      DetsPlus.open_file(base, name: path, auto_save: 60_000, auto_save_memory: 1_000_000)
    end
  end

  def close do
    # folder_path = Application.get_env(:ipncore, :wallet_path)

    for number <- 0..(@partitions - 1) do
      base = String.to_existing_atom(IO.iodata_to_binary([@base, number]))
      # path = Path.join(folder_path, IO.iodata_to_binary([base, @file_extension]))
      DetsPlus.close(base)
    end
  end

  def put_multi!(pubkeys) do
    Enum.each(pubkeys, fn pubkey ->
      put!(pubkey)
    end)
  end

  def put!(pubkey) do
    hash = Address.hash(pubkey)
    base = get_base(hash)

    case DetsPlus.insert_new(base, {hash, pubkey}) do
      true ->
        :ok

      false ->
        :error
    end
  end

  def get(address_hash) do
    [{_hash, pubkey}] = DetsPlus.lookup(get_base(address_hash), address_hash)
    pubkey
  end

  def has_key?(address_hash) do
    DetsPlus.member?(get_base(address_hash), address_hash)
  end

  def check!(hash, pubkey, signature) do
    cond do
      byte_size(pubkey) != 897 ->
        throw("Invalid pubkey")

      Falcon.verify(hash, signature, pubkey) == :error ->
        throw("Invalid signature")

      exists?(pubkey) ->
        throw("Pubkey already exists")

      true ->
        :ok
    end
  end

  def exists?(address_hash) do
    DetsPlus.member?(get_base(address_hash), address_hash)
  end

  defp get_base(<<first::8, _rest::binary>>) do
    number = rem(first, @partitions)

    [@base, number]
    |> IO.iodata_to_binary()
    |> String.to_existing_atom()
  end
end
