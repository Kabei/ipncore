defmodule Ippan.Token do
  alias __MODULE__
  require BigNumber

  @behaviour Ippan.Struct
  @app Mix.Project.config()[:app]
  @json Application.compile_env(@app, :json)

  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          owner: binary(),
          image: String.t(),
          decimal: non_neg_integer(),
          symbol: String.t(),
          max_supply: non_neg_integer(),
          props: list() | nil,
          env: map(),
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  @impl true
  def optionals, do: ~w(image props env)

  @impl true
  def editable, do: ~w(image name owner)

  def props, do: ~w(burn coinbase drop lock reload stream)

  defstruct id: nil,
            name: nil,
            owner: nil,
            image: nil,
            decimal: 0,
            symbol: nil,
            max_supply: 0,
            props: [],
            env: %{},
            created_at: nil,
            updated_at: nil

  @impl true
  def to_list(x) do
    [
      x.id,
      x.owner,
      x.name,
      x.image,
      x.decimal,
      x.symbol,
      BigNumber.to_bin(x.max_supply),
      @json.encode!(x.props),
      CBOR.encode(x.env),
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
        image,
        decimal,
        symbol,
        max_supply,
        props,
        env,
        created_at,
        updated_at
      ]) do
    %{
      id: id,
      name: name,
      owner: owner,
      image: image,
      decimal: decimal,
      symbol: symbol,
      max_supply: BigNumber.to_int(max_supply),
      props: @json.decode!(props),
      env: :erlang.element(1, CBOR.Decoder.decode(env)),
      created_at: created_at,
      updated_at: updated_at
    }
  end

  def has_prop?(%{props: props}, prop), do: prop in props

  def has_prop?(_, _), do: false

  def insert(db_ref, map) do
    Sqlite.step(db_ref, "insert_token", to_list(map))
  end

  def get(db_ref, id) do
    Sqlite.get(db_ref, :token, "get_token", id, Token)
  end

  def exists?(db_ref, id) do
    Sqlite.has?(db_ref, :token, "exists_token", [id])
  end

  def owner?(db_ref, id, owner) do
    Sqlite.exists?(db_ref, "owner_token", [id, owner])
  end

  def total(db_ref) do
    Sqlite.one(db_ref, "total_tokens", [], 0)
  end

  def update(db_ref, map, id) do
    :ets.delete(:token, id)
    Sqlite.update(db_ref, "assets.token", map, id: id)
  end

  def delete(db_ref, id, owner) do
    :ets.delete(:token, id)
    Sqlite.step(db_ref, "delete_token", [id, owner])
  end
end
