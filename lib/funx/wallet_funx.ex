defmodule Ippan.Funx.Wallet do
  alias Ippan.{Address, Wallet}
  require SqliteStore

  def subscribe(%{conn: conn, stmts: stmts, timestamp: timestamp}, pubkey, validator_id, sig_type) do
    pubkey = Fast64.decode64(pubkey)

    wallet =
      %Wallet{
        id: Address.hash(sig_type, pubkey),
        pubkey: pubkey,
        validator: validator_id,
        created_at: timestamp
      }
      |> Wallet.to_list()

    SqliteStore.step(conn, stmts, "insert_wallet", wallet)
  end

  def unsubscribe(%{
        id: account_id,
        conn: conn,
        stmts: stmts
      }) do
    SqliteStore.step(conn, stmts, "delete_wallet", [account_id])
  end
end
