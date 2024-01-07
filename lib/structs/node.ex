defmodule Ippan.Node do
  require Sqlite
  @behaviour Ippan.Struct

  @type t :: %__MODULE__{
          id: binary,
          hostname: charlist(),
          port: non_neg_integer(),
          class: [binary] | nil,
          pubkey: binary,
          net_pubkey: binary,
          avatar: binary | nil,
          created_at: integer(),
          updated_at: integer()
        }

  defstruct [
    :id,
    :hostname,
    :port,
    :pubkey,
    :net_pubkey,
    :avatar,
    :created_at,
    :updated_at,
    class: ""
  ]

  # @fields __MODULE__.__struct__() |> Map.keys() |> Enum.map(&to_string(&1)) |> IO.inspect()
  # @spec fields :: [binary()]
  # def fields, do: @fields

  @impl true
  def editable, do: ~w(hostname port class avatar)

  @impl true
  def optionals, do: ~w(avatar)

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.port,
      x.class,
      x.pubkey,
      x.net_pubkey,
      x.avatar,
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
        avatar,
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
      avatar: avatar,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  @impl true
  def to_map({_id, x}), do: x

  defmacro insert(args) do
    quote do
      Sqlite.step("insert_node", unquote(args))
    end
  end

  defmacro get(id) do
    quote location: :keep do
      Sqlite.fetch("get_node", [unquote(id)])
      |> case do
        nil -> nil
        x -> Ippan.Node.list_to_map(x)
      end
    end
  end

  defmacro fetch(id) do
    quote location: :keep do
      Sqlite.fetch("get_node", [unquote(id)])
    end
  end

  defmacro exists?(id) do
    quote location: :keep do
      Sqlite.exists?("exists_node", [unquote(id)])
    end
  end

  defmacro total do
    quote location: :keep do
      Sqlite.one("total_nodes", [])
    end
  end

  defmacro update(map_fields, id) do
    quote location: :keep do
      Sqlite.update("nodes", unquote(map_fields), id: unquote(id))
    end
  end

  defmacro last_mod do
    quote location: :keep do
      Sqlite.one("last_mod", [])
    end
  end

  defmacro delete(id) do
    quote location: :keep do
      Sqlite.step("delete_node", [unquote(id)])
    end
  end

  defmacro delete_all do
    quote location: :keep do
      Sqlite.step("delete_nodes", [])
    end
  end
end
