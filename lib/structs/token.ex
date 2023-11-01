defmodule Ippan.Token do
  require BigNumber

  @behaviour Ippan.Struct
  @app Mix.Project.config()[:app]
  @json Application.compile_env(@app, :json)

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          owner: binary(),
          avatar: String.t(),
          decimal: non_neg_integer(),
          symbol: String.t(),
          max_supply: non_neg_integer(),
          props: list() | nil,
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  @impl true
  def optionals, do: ~w(avatar props)

  @impl true
  def editable, do: ~w(avatar name owner)

  def props, do: ~w(burn coinbase lock)

  defstruct id: nil,
            name: nil,
            owner: nil,
            avatar: nil,
            decimal: 0,
            symbol: nil,
            max_supply: 0,
            props: [],
            created_at: nil,
            updated_at: nil

  @impl true
  def to_list(x) do
    [
      x.id,
      x.owner,
      x.name,
      x.avatar,
      x.decimal,
      x.symbol,
      BigNumber.to_bin(x.max_supply),
      @json.encode!(x.props),
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
  def to_map({_id, x}), do: x

  @impl true
  def list_to_map([
        id,
        owner,
        name,
        avatar,
        decimal,
        symbol,
        max_supply,
        props,
        created_at,
        updated_at
      ]) do
    %{
      id: id,
      name: name,
      owner: owner,
      avatar: avatar,
      decimal: decimal,
      symbol: symbol,
      max_supply: BigNumber.to_int(max_supply),
      props: @json.decode!(props),
      created_at: created_at,
      updated_at: updated_at
    }
  end

  def has_prop?(%{props: props}, prop), do: prop in props

  def has_prop?(_, _), do: false

  defmacro insert(args) do
    quote location: :keep do
      Sqlite.step("insert_token", unquote(args))
    end
  end

  defmacro get(id) do
    quote location: :keep do
      Sqlite.get(:token, "get_token", unquote(id), Ippan.Token)
    end
  end

  defmacro exists?(id) do
    quote location: :keep do
      Sqlite.exists?("exists_token", [unquote(id)])
    end
  end

  defmacro owner?(id, owner) do
    quote bind_quoted: [id: id, owner: owner], location: :keep do
      Sqlite.exists?("owner_token", [id, owner])
    end
  end

  defmacro total do
    quote location: :keep do
      Sqlite.one("total_tokens", [], 0)
    end
  end

  defmacro delete(id, owner) do
    quote bind_quoted: [id: id, owner: owner], location: :keep do
      :ets.delete(:token, id)
      Sqlite.step("delete_token", [id, owner])
    end
  end

  defmacro update(map, id) do
    quote location: :keep do
      :ets.delete(:token, id)
      Sqlite.update("assets.token", unquote(map), id: unquote(id))
    end
  end
end
