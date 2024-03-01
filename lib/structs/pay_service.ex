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

  def create(db_ref, id, name, image, extra, round_id) do
    Sqlite.step("insert_paysrv", [id, name, image, CBOR.encode(extra), round_id])
  end

  def update(db_ref, map, id) do
    Sqlite.update("pay.serv", map, id: id)
  end

  def delete(db_ref, id) do
    Sqlite.step("delete_paysrv", [id])
    Sqlite.step("delete_all_subpay", [id])
  end

  def to_map([id, name, image, extra, created_at, updated_at]) do
    extra = :erlang.element(1, CBOR.Decoder.decode(extra))

    %{
      id: id,
      image: image,
      name: name,
      created_at: created_at,
      extra: extra,
      updated_at: updated_at
    }
  end
end
