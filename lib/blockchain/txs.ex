defmodule Ipncore.Tx do
  use Ecto.Schema
  require Logger
  import Ipnutils.Macros, only: [deftypes: 1, defstatus: 1]
  import Ecto.Query, only: [from: 1, from: 2, where: 3, select: 3, order_by: 3, join: 5]
  import Ipnutils.Filters
  alias Ipncore.{Address, Block, Chain, Txo, Txi, Balance, Token}
  alias __MODULE__

  deftypes do
    [
      {100, "tx.coinbase"},
      {101, "tx.send"},
      {102, "tx.msend"},
      {103, "tx.refund"},
      {104, "tx.distro"},
      {105, "tx.jackpot"},
      {200, "token.new"},
      {201, "token.update"},
      {202, "token.delete"}
    ]
  else
    {false, false}
  end

  defstatus do
    [
      {0, "pending"},
      {1, "approved"},
      {2, "confirmed"},
      {-1, "cancelled"},
      {-2, "timeout"}
    ]
  else
    {false, false}
  end

  @output_type_send "S"

  @primary_key {:id, :binary, []}
  schema "tx" do
    field(:from, {:array, :binary})
    field(:out_count, :integer)
    field(:in_count, :integer)
    field(:amount, :integer)
    field(:time, :integer)
    field(:validator, :binary)
    field(:fee, :integer, default: 0)
    field(:refundable, :boolean, default: false)
    field(:memo, :string)
  end

  defmacro map_select do
    quote do
      %{
        id: tx.id,
        out_count: tx.out_count,
        in_count: tx.in_count,
        amount: tx.amount,
        time: tx.time,
        validator: tx.validator,
        fee: tx.fee,
        refundable: tx.refundable,
        memo: tx.memo
      }
    end
  end

  def send(
        channel,
        event,
        from_address,
        [
          token,
          amount,
          to_address,
          validator_address,
          fee,
          refundable,
          memo
        ],
        next_index,
        genesis_time
      ) do
    txid = event.id
    if amount <= 0, do: throw(RuntimeError, "Invalid amount to send")

    inputs = [%{id: txid, ix: 0, address: from_address, token: token, value: amount}]

    outputs = [
      %{
        id: txid,
        ix: 0,
        address: to_address,
        token: token,
        value: amount,
        type: @output_type_send
      }
    ]

    validator = Validator.fetch!(validator_address)

    fees = calc_fee(amount, validator.fee, validator.percent, event.size)

    tx = %{
      id: txid,
      from: [from_address],
      fee: fees,
      refundable: refundable,
      time: event.time,
      memo: memo,
      validator: validator_address,
      out_count: length(outputs),
      in_count: length(inputs),
      amount: amount
    }

    Ecto.Multi.new()
    |> Event.multi_insert(event, channel)
    |> multi_insert(tx, channel)
    |> Txi.multi_insert(inputs, channel)
    |> Txo.multi_insert(outputs, channel)
    |> Balances.multi_incomes()
  end

  @doc """
  0 -> by size
  1 -> percent
  2 -> fixed price
  """
  def calc_fees(0, _amount, validator_fee, size),
    do: validator_fee * size

  def calc_fees(1, amount, validator_fee, _size),
    do: :math.ceil(amount * (validator_fee / 100)) |> trunc()

  def calc_fees(2, _amount, validator_fee, 2, _size), do: validator_fee |> trunc()

  def multi_insert_(
        multi,
        %{outputs: outputs, time: time} = params,
        token_id,
        total,
        channel
      ) do
    multi
    |> Ecto.Multi.insert(:tx, tx, prefix: channel, returning: false)
    |> Ecto.Multi.insert_all(:txo, Txo, outputs, prefix: channel, returning: false)
    |> Balance.multi_upsert_incomes(:incomes, outputs, tx.time, channel)
    |> Token.multi_update_stats(:token, token_id, total, time, channel)
  end

  def multi_insert_coinbase(
        multi,
        %{outputs: outputs, time: time} = params,
        token_id,
        total,
        channel
      ) do
    tx =
      struct(Tx, params)
      |> Map.drop([:outputs])

    multi
    |> Ecto.Multi.insert(:tx, tx, prefix: channel, returning: false)
    |> Ecto.Multi.insert_all(:txo, Txo, outputs, prefix: channel, returning: false)
    |> Balance.multi_upsert_incomes(:incomes, outputs, tx.time, channel)
    |> Token.multi_update_stats(:token, token_id, total, time, channel)
  end
end
