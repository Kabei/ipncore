defmodule Ippan.Node do

  @behaviour Ippan.Struct

  @type t :: %__MODULE__{
          id: binary,
          hostname: charlist(),
          port: non_neg_integer(),
          class: [binary] | nil,
          pubkey: binary,
          net_pubkey: binary,
          image: binary | nil,
          created_at: integer(),
          updated_at: integer()
        }

  defstruct [
    :id,
    :hostname,
    :port,
    :pubkey,
    :net_pubkey,
    :image,
    :created_at,
    :updated_at,
    class: ""
  ]

  # @fields __MODULE__.__struct__() |> Map.keys() |> Enum.map(&to_string(&1)) |> IO.inspect()
  # @spec fields :: [binary()]
  # def fields, do: @fields

  @impl true
  def editable, do: ~w(hostname port class image)

  @impl true
  def optionals, do: ~w(image)

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.port,
      x.class,
      x.pubkey,
      x.net_pubkey,
      x.image,
      x.created_at,
      x.updated_at
    ]
  end

  @impl true
  def list_to_tuple([id | _] = x) do
    {id, list_to_map(x)}
  end

  @impl true
  def to_tuple(x) do
    {x.id, x}
  end

  @impl true
  def list_to_map([
        id,
        hostname,
        port,
        class,
        pubkey,
        net_pubkey,
        image,
        created_at,
        updated_at
      ]) do
    %{
      id: id,
      hostname: hostname,
      port: port,
      class: class,
      pubkey: pubkey,
      net_pubkey: net_pubkey,
      image: image,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  @impl true
  def to_map({_id, x}), do: x

  def insert(db_ref, map) do
    Sqlite.step(db_ref, "insert_node", to_list(map))
  end

  def get(db_ref, id) do
    Sqlite.fetch(db_ref, "get_node", [id])
    |> case do
      nil -> nil
      x -> list_to_map(x)
    end
  end

  def fetch(db_ref, id) do
    Sqlite.fetch(db_ref, "get_node", [id])
  end

  def exists?(db_ref, id) do
    Sqlite.exists?(db_ref, "exists_node", [id])
  end

  def total(db_ref) do
    Sqlite.one(db_ref, "total_nodes", [], 0)
  end

  def update(db_ref, map_fields, id) do
    Sqlite.update(db_ref, "nodes", map_fields, id: id)
  end

  def last_mod(db_ref) do
    Sqlite.one(db_ref, "last_mod", [], nil)
  end

  def delete(db_ref, id) do
    Sqlite.step(db_ref, "delete_node", [id])
  end

  def delete_all(db_ref) do
    Sqlite.step(db_ref, "delete_nodes", [])
  end
end
