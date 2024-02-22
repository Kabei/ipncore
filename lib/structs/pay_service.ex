defmodule PayService do
  require Sqlite

  def get(db_ref, id) do
    case Sqlite.fetch("get_paysrv", [id]) do
      nil ->
        nil

      result ->
        to_map(result)
    end
  end

  def exists?(db_ref, id) do
    Sqlite.exists?("exists_paysrv", [id])
  end

  def create(db_ref, id, name, extra, round_id) do
    Sqlite.step("insert_paysrv", [id, name, CBOR.encode(extra), round_id])
  end

  def update(db_ref, map, id) do
    Sqlite.update("pay.serv", map, id: id)
  end

  def delete(db_ref, id) do
    Sqlite.step("delete_paysrv", [id])
    Sqlite.step("delete_all_subpay", [id])
  end

  def to_map([id, name, extra, created_at, updated_at]) do
    extra = :erlang.element(1, CBOR.Decoder.decode(extra))

    %{id: id, name: name, created_at: created_at, updated_at: updated_at}
    |> Map.merge(extra)
  end
end

defmodule SubPay do
  require Sqlite

  def subscribe(db_ref, id, payer, token, extra, last_round) do
    Sqlite.step("insert_subpay", [id, payer, token, CBOR.encode(extra), last_round])
  end

  def has?(db_ref, id, payer, token) do
    Sqlite.exists?("exists_subpay", [id, payer, token])
  end

  def get(db_ref, id, payer, token) do
    case Sqlite.fetch("get_subpay", [id, payer, token]) do
      nil -> nil
      result -> to_map(result)
    end
  end

  def unsubscribe(db_ref, id, payer, token) do
    Sqlite.step("delete_subpay", [id, payer, token])
  end

  def to_map([id, payer, token, created_at, last_round, extra]) do
    extra = :erlang.element(1, CBOR.Decoder.decode(extra))

    %{
      id: id,
      payer: payer,
      token: token,
      created_at: created_at,
      extra: extra,
      last_round: last_round
    }
  end
end
