defmodule SubPay do
  require Sqlite

  def subscribe(db_ref, id, payer, token, created_at, every, maxAmount, extra \\ %{}) do
    Sqlite.step("insert_subpay", [
      id,
      payer,
      token,
      every,
      maxAmount,
      Jason.encode!(extra),
      created_at
    ])
  end

  def has?(db_ref, id, payer) do
    Sqlite.exists?("exists_subpay", [id, payer])
  end

  def has?(db_ref, id, payer, token) do
    Sqlite.exists?("exists_subpay_token", [id, payer, token])
  end

  def get(db_ref, id, payer, token) do
    case Sqlite.fetch("get_subpay", [id, payer, token]) do
      nil -> nil
      result -> to_map(result)
    end
  end

  def spent(db_ref, id, payer, token, div, spent, last_pay) do
    Sqlite.step("up_subpay", [id, payer, token, div, spent, last_pay])
  end

  def reset_spent(db_ref, id, payer, token, div, spent, last_pay) do
    Sqlite.step("reset_subpay", [id, payer, token, div, spent, last_pay])
  end

  def unsubscribe(db_ref, id, payer) do
    Sqlite.step("delete_subpay", [id, payer])
  end

  def unsubscribe(db_ref, id, payer, token) do
    Sqlite.step("delete_subpay_token", [id, payer, token])
  end

  def total(db_ref, payer) do
    Sqlite.one("total_subpay_payer", [payer], 0)
  end

  def to_map([id, payer, token, lastPay, div, every, spent, extra, status, created_at]) do
    extra = Jason.decode!(extra)

    %{
      id: id,
      payer: payer,
      token: token,
      created_at: created_at,
      extra: extra,
      lastPay: lastPay,
      div: div,
      every: every,
      spent: spent,
      status: status
    }
  end
end
