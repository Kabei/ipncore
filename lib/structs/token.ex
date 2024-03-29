defmodule Ippan.Token do
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

  defmacro update(map, id) do
    quote bind_quoted: [map: map, id: id], location: :keep do
      :ets.delete(:token, id)
      Sqlite.update("assets.token", map, id: id)
    end
  end

  defmacro delete(id, owner) do
    quote bind_quoted: [id: id, owner: owner], location: :keep do
      :ets.delete(:token, id)
      Sqlite.step("delete_token", [id, owner])
    end
  end
end
