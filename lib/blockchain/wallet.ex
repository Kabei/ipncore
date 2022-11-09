defmodule Ipncore.Wallet do
  @db :wallet
  @filename "wallet.db"

  def open(_channel) do
    folder_path = Application.get_env(:ipncore, :blockchain_path)
    path = Path.join(folder_path, @filename)
    DetsPlus.open_file(@db, name: path, auto_save: :infinity)
  end

  def put(pubkey) do
    hash = Address.hash(pubkey)

    case DetsPlus.insert_new(@db, {hash, pubkey}) do
      true ->
        DetsPlus.sync(@db)

      false ->
        :error
    end
  end

  def get(address_hash) do
    DetsPlus.lookup(@db, address_hash)
  end

  def has_key?(address_hash) do
    DetsPlus.member?(@db, address_hash)
  end
end
