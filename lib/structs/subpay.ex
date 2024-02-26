defmodule SubPay do
  require Sqlite

  def subscribe(db_ref, id, payer, token, extra, created_at) do
    Sqlite.step("insert_subpay", [id, payer, token, CBOR.encode(extra), created_at])
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

  def update(db_ref, id, payer, token, last_round) do
    Sqlite.step("up_subpay", [id, payer, token, last_round])
  end

  def unsubscribe(db_ref, id, payer, token) do
    Sqlite.step("delete_subpay", [id, payer, token])
  end

  def total(db_ref, payer) do
    Sqlite.one("total_subpay_payer", [payer], 0)
  end

  def to_map([id, payer, token, extra, created_at, last_round]) do
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
