defmodule Ippan.Token do
  @json Application.compile_env(:ipncore, :json)
  @type t :: %__MODULE__{
          id: String.t(),
          name: String.t(),
          owner: binary(),
          avatar: String.t(),
          decimal: non_neg_integer(),
          symbol: String.t(),
          enabled: boolean(),
          supply: non_neg_integer(),
          burned: non_neg_integer(),
          props: list() | nil,
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  def optionals, do: ~w(avatar props)a

  def editable, do: ~w(avatar name owner)a

  def props, do: ~w(burn coinbase lock vote)

  defstruct id: nil,
            name: nil,
            owner: nil,
            avatar: nil,
            decimal: 0,
            symbol: nil,
            enabled: true,
            supply: 0,
            burned: 0,
            props: [],
            created_at: nil,
            updated_at: nil

  def to_list(x) do
    [
      x.id,
      x.name,
      x.owner,
      x.avatar,
      x.decimal,
      x.symbol,
      x.enabled,
      x.supply,
      x.burned,
      @json.encode!(x.props),
      x.created_at,
      x.updated_at
    ]
  end

  def to_tuple(x) do
    {x.id, x.name, x.owner, x.avatar, x.decimal, x.symbol, x.enabled, x.supply, x.burned, x.props,
     x.created_at, x.updated_at}
  end

  def to_map(
        {id, name, owner, avatar, decimal, symbol, enabled, supply, burned, props, created_at,
         updated_at}
      ) do
    %{
      id: id,
      name: name,
      owner: owner,
      avatar: avatar,
      decimal: decimal,
      symbol: symbol,
      enabled: enabled,
      supply: supply,
      burned: burned,
      props: props,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  def to_map([
        id,
        name,
        owner,
        avatar,
        decimal,
        symbol,
        enabled,
        supply,
        burned,
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
      enabled: enabled,
      supply: supply,
      burned: burned,
      props: @json.decode!(props),
      created_at: created_at,
      updated_at: updated_at
    }
  end
end
