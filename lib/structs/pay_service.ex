defmodule PayService do


  def get(db_ref, id) do
    case Sqlite.fetch(db_ref, "get_paysrv", [id]) do
      nil ->
        nil

      result ->
        to_map(result)
    end
  end

  def exists?(db_ref, id) do
    Sqlite.exists?(db_ref, "exists_paysrv", [id])
  end

  def owner?(db_ref, id, owner) do
    Sqlite.exists?(db_ref, "owner_paysrv", [id, owner])
  end

  def create(db_ref, id, name, owner, image, descrip, extra, round_id) do
    Sqlite.step(db_ref, "insert_paysrv", [id, name, owner, image, descrip, Jason.encode!(extra), round_id])
  end

  def update(db_ref, map, id) do
    Sqlite.update(db_ref, "srv.serv", map, id: id)
  end

  def count(db_ref, id, count) do
    Sqlite.step(db_ref, "count_subs", [id, count])
  end

  def delete(db_ref, id) do
    Sqlite.step(db_ref, "delete_paysrv", [id])
    Sqlite.step(db_ref, "delete_all_subpay", [id])
  end

  def to_map([id, name, owner, image, descrip, extra, subs, status, created_at, updated_at]) do
    extra = Jason.decode!(extra)

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
