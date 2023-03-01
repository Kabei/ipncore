defmodule ValidatorUpdate do
  alias Ipncore.Validator
  import Guards
  import Ecto.Query

  def valid!(from_address, host, params, timestamp)
      when is_wallet_address(from_address) and is_map(params) and is_positive(timestamp) do
    with true <- Match.domain?(host) do
      Validator.fetch!(host, from_address)
    else
      _ ->
        raise ArgumentError
    end
  end

  def run!(from_address, host, params, timestamp) do
    map_params =
      params
      |> Map.take(@edit_fields)
      |> MapUtil.validate_not_empty()
      |> MapUtil.validate_length("name", 100)
      |> MapUtil.validate_value("fee", :gt, 0)
      |> MapUtil.validate_range("fee_type", 0..2)
      |> MapUtil.validate_length("avatar", 255)
      |> MapUtil.validate_address("owner")
      |> MapUtil.to_atoms()
      |> Map.put(:updated_at, timestamp)

    kw_params =
      map_params
      |> MapUtil.to_keywords()

    Validator.fetch!(host, from_address)
    |> Map.merge(map_params)
    |> Validator.put()

    kw_params
  end

  def multi!(multi, host, from_address, kw_params, chain) do
    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^from_address)

    Ecto.Multi.update_all(multi, :update, queryable, [set: kw_params],
      returning: false,
      prefix: chain
    )
  end
end
