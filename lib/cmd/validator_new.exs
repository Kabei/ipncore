# defmodule Command do
#         @callback valid?(args :: list()) :: :ok | :error
#         @callback run() :: term()
# end

defmodule ValidatorNew do
  alias Ipncore.Validator
  import Guards

  def valid!(host, name, from_address, avatar, fee_type, fee)
      when is_binary(host) and
             is_binary_max_length(name, 100) and
             is_binary_max_length(avatar, 255) and
             is_fee_type(fee_type) and
             is_float_positive(fee) do
    with(
      true <- Match.url?(host),
      true <- Platform.owner?(from_address),
      true <- Validator.exists?(host)
    ) do
      :ok
    else
      _ ->
        raise ArgumentError
    end
  end

  def run!(_from_address, host, name, owner, avatar, fee_type, fee, timestamp) do
    validator = %{
      host: host,
      name: name,
      owner: owner,
      avatar: avatar,
      fee: fee,
      fee_type: fee_type,
      created_at: timestamp,
      updated_at: timestamp
    }

    Validator.put_new!(validator)

    validator
  end

  def multi!(validator, multi, chain) do
    multi
    |> Ecto.Multi.insert_all(:validator, Validator, [validator],
      returning: false,
      prefix: chain
    )
  end
end
