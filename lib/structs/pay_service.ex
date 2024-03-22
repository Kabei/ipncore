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

  def owner?(db_ref, id, owner) do
    Sqlite.exists?("owner_paysrv", [id, owner])
  end

  def create(db_ref, id, name, owner, image, descrip, extra, round_id) do
    Sqlite.step("insert_paysrv", [id, name, owner, image, descrip, Jason.encode!(extra), round_id])
  end

  def update(db_ref, map, id) do
    Sqlite.update("srv.serv", map, id: id)
  end

  def count(db_ref, id, count) do
    Sqlite.step("count_subs", [id, count])
  end

  def delete(db_ref, id) do
    Sqlite.step("delete_paysrv", [id])
    Sqlite.step("delete_all_subpay", [id])
  end

  def to_map([id, name, owner, image, descrip, extra, subs, status, created_at, updated_at]) do
    extra = :erlang.element(1, Jason.decode!(extra))

    %{
      id: id,
      image: image,
      name: name,
      owner: owner,
      descrip: descrip,
      status: status,
      created_at: created_at,
      extra: extra,
      subs: subs,
      updated_at: updated_at
    }
  end
end
