defmodule Ipncore.Wallet do
  alias Ipncore.Address

  @base "wallet"
  @file_extension ".db"
  @partitions 16

  defmacrop get_base(hash) do
    quote do
      <<first::8, _rest::binary>> = unquote(hash)
      number = rem(first, @partitions)

      [@base, to_string(number)]
      |> IO.iodata_to_binary()
      |> String.to_existing_atom()
    end
  end

  def open do
    folder_path = Application.get_env(:ipncore, :wallet_path, "data/wallets")
    File.mkdir_p(folder_path)

    for number <- 0..(@partitions - 1) do
      base = String.to_atom(IO.iodata_to_binary([@base, to_string(number)]))
      path = Path.join(folder_path, IO.iodata_to_binary([to_string(base), @file_extension]))
      DetsPlus.open_file(base, file: path, auto_save: 60_000, auto_save_memory: 1_000_000)
    end
  end

  def close do
    for number <- 0..(@partitions - 1) do
      base = String.to_existing_atom(IO.iodata_to_binary([@base, to_string(number)]))
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
    case DetsPlus.lookup(get_base(address_hash), address_hash) do
      [{_hash, pubkey}] ->
        pubkey

      _ ->
        nil
    end
  end

  def fetch!(address_hash) do
    case DetsPlus.lookup(get_base(address_hash), address_hash) do
      [{_hash, pubkey}] ->
        pubkey

      _ ->
        throw("Pubkey not exists")
    end
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
end
