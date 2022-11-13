defmodule Ipncore.Wallet do
  alias Ipncore.Address

  @base "wallet"
  @file_extension ".db"
  @partitions 8

  def open(_channel) do
    folder_path = Application.get_env(:ipncore, :dir_path)

    for number <- 0..(@partitions - 1) do
      base = String.to_atom("#{@base}#{number}")
      path = Path.join(folder_path, base <> @file_extension)
      DetsPlus.open_file(base, name: path, auto_save: :infinity)
    end
  end

  def close(_) do
    folder_path = Application.get_env(:ipncore, :dir_path)

    for number <- 0..(@partitions - 1) do
      base = String.to_existing_atom("#{@base}#{number}")
      path = Path.join(folder_path, base <> @file_extension)
      DetsPlus.close(base)
    end
  end

  def put(pubkey) do
    hash = Address.hash(pubkey)
    base = get_base(hash)

    case DetsPlus.insert_new(base, {hash, pubkey}) do
      true ->
        DetsPlus.sync(base)

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

  defp get_base(<<first::8, _rest::binary>>) do
    number = rem(first, @partitions)

    [@base, number]
    |> IO.iodata_to_binary()
    |> String.to_existing_atom()
  end
end
