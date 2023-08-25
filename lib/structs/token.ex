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
          max_supply: non_neg_integer(),
          props: list() | nil,
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  def optionals, do: ~w(avatar props)

  def editable, do: ~w(avatar name owner)

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
            max_supply: 0,
            props: [],
            created_at: nil,
            updated_at: nil

  def to_list(
        {id, owner, name, avatar, decimal, symbol, enabled, supply, burned, max_supply, props,
         created_at, updated_at}
      ) do
    [
      id,
      owner,
      name,
      avatar,
      decimal,
      symbol,
      enabled,
      supply,
      burned,
      max_supply,
      @json.encode!(props),
      created_at,
      updated_at
    ]
  end

  def to_list(x) do
    [
      x.id,
      x.owner,
      x.name,
      x.avatar,
      x.decimal,
      x.symbol,
      x.enabled,
      x.supply,
      x.burned,
      x.max_supply,
      @json.encode!(x.props),
      x.created_at,
      x.updated_at
    ]
  end

  def to_tuple([
        id,
        owner,
        name,
        avatar,
        decimal,
        symbol,
        enabled,
        supply,
        burned,
        max_supply,
        props,
        created_at,
        updated_at
      ]) do
    {id, owner, name, avatar, decimal, symbol, enabled, supply, burned, max_supply, props,
     created_at, updated_at}
  end

  def to_tuple(x) do
    {x.id, x.owner, x.name, x.avatar, x.decimal, x.symbol, x.enabled, x.supply, x.burned,
     x.max_supply, x.props, x.created_at, x.updated_at}
  end

  def to_map(
        {id, owner, name, avatar, decimal, symbol, enabled, supply, burned, max_supply, props,
         created_at, updated_at}
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
      max_supply: max_supply,
      props: @json.decode!(props),
      created_at: created_at,
      updated_at: updated_at
    }
  end

  def to_map([
        id,
        owner,
        name,
        avatar,
        decimal,
        symbol,
        enabled,
        supply,
        burned,
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
      enabled: enabled,
      supply: supply,
      burned: burned,
      max_supply: max_supply,
      props: @json.decode!(props),
      created_at: created_at,
      updated_at: updated_at
    }
  end
end
