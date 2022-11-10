defmodule Ipncore.Tx do
  use Ecto.Schema
  require Logger
  import Ipnutils.Macros, only: [deftypes: 1, defstatus: 1]
  import Ecto.Query, only: [from: 1, from: 2, where: 3, select: 3, order_by: 3, join: 5]
  import Ipnutils.Filters
  alias Ipncore.{Address, Balance, Block, Chain, Txo, Txi, Balance, Token, Validator}
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
  @output_type_coinbase "C"
  @output_type_fee "%"

  @primary_key {:id, :binary, []}
  schema "tx" do
    field(:from, {:array, :binary})
    field(:time, :integer)
    field(:out_count, :integer)
    field(:in_count, :integer)
    field(:amount, :integer)
    field(:token_value, :map)
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
          refundable,
          memo
        ],
        multi
      ) do
    txid = event.id
    timestamp = event.time
    if amount <= 0, do: throw("Invalid amount to send")

    from_address = Address.from_text(from_address)
    validator = Validator.fetch!(validator_address)
    fees = calc_fees(validator.fee_type, validator.fee, amount, event.size)

    outputs = [
      %{
        id: txid,
        ix: 0,
        address: to_address,
        token: token,
        value: amount,
        type: @output_type_send
      },
      %{
        id: txid,
        ix: 1,
        address: validator_address,
        token: token,
        value: fees,
        type: @output_type_fee
      }
    ]

    inputs = [%{id: txid, ix: 0, address: from_address, token: token, value: -amount - fees}]

    Balance.update!(
      [{from_address, token}, {to_address, token}, {validator_address, token}],
      %{
        {from_address, token} => -amount - fees,
        {to_address, token} => amount,
        {validator_address, token} => fees
      }
    )

    tx = %{
      id: txid,
      from: [from_address],
      fee: fees,
      refundable: refundable,
      time: timestamp,
      memo: memo,
      validator: validator_address,
      out_count: length(outputs),
      in_count: length(inputs),
      amount: amount
    }

    multi
    |> multi_insert(tx, channel)
    |> Txi.multi_insert_all(:txi, inputs, channel)
    |> Txo.multi_insert_all(:txo, outputs, channel)
    |> Balance.multi_upsert_outgoings(:outgoings, inputs, timestamp, channel)
    |> Balance.multi_upsert_incomes(:incomes, outputs, timestamp, channel)
  end

  def coinbase(
        channel,
        event,
        from_address,
        [
          token_id,
          init_outputs,
          memo
        ],
        multi
      ) do
    txid = event.id
    timestamp = event.time

    {outputs, keys_entries, entries, token_value, amount} =
      outputs_extract_coinbase!(txid, init_outputs, token_id)

    if Token.owner?(token_id, from_address, channel) != false,
      do: throw("Invalid owner")

    Balance.update!(keys_entries, entries)

    tx = %{
      id: event.id,
      fee: 0,
      token_value: token_value,
      refundable: false,
      time: timestamp,
      memo: memo,
      out_count: length(outputs),
      in_count: 0,
      amount: amount
    }

    multi
    |> multi_insert(tx, channel)
    |> Txo.multi_insert(outputs, channel)
    |> Token.multi_update_stats(:token, token_id, amount, timestamp, channel)
    |> Balance.multi_upsert_incomes(:incomes, outputs, timestamp, channel)
  end

  def send_fee(channel, event, from_address, validator_address, multi) do
    txid = event.id
    timestamp = event.time
    validator = Validator.fetch!(validator_address)
    fees = calc_fees(0, 1, 0, event.size)

    inputs = [%{id: txid, ix: 0, address: from_address, token: @token, value: fees}]

    outputs = [
      %{
        id: txid,
        ix: 0,
        address: validator_address,
        token: @token,
        value: fees,
        type: @output_type_fee
      }
    ]

    tx = %{
      id: txid,
      from: [from_address],
      fee: fees,
      refundable: false,
      time: timestamp,
      validator: validator_address,
      out_count: length(outputs),
      in_count: length(inputs),
      amount: 0
    }

    multi
    |> multi_insert(tx, channel)
    |> Txi.multi_insert_all(:txi, inputs, channel)
    |> Txo.multi_insert_all(:txo, outputs, channel)
    |> Balance.multi_upsert_outgoings(:outgoings, inputs, timestamp, channel)
    |> Balance.multi_upsert_incomes(:incomes, outputs, timestamp, channel)
  end

  defp outputs_extract_coinbase!(txid, txos, token) do
    {txos, key_entries, entries, amount, _ix} =
      Enum.reduce(txos, {[], [], %{}, 0, 0}, fn [address, value],
                                                {acc_txos, acc_keys, acc_entries, acc_amount,
                                                 acc_ix} ->
        if value <= 0, do: throw("Output has value zero")
        bin_address = Address.from_text(address)

        output = %{
          id: txid,
          ix: acc_ix,
          token: token,
          address: bin_address,
          type: @output_type_coinbase,
          value: value
        }

        entry = {bin_address, token}
        acc_entries = Map.put(acc_entries, entry, value)

        {acc_txos ++ [output], acc_keys ++ [entry], acc_entries, acc_amount + value, acc_ix + 1}
      end)

    token_value = Map.new() |> Map.put(token, amount)

    {txos, key_entries, entries, token_value, amount}
  end

  @doc """
  0 -> by size
  1 -> by percent
  2 -> fixed price
  """
  def calc_fees(0, fee_amount, _tx_amount, size),
    do: trunc(fee_amount) * size

  def calc_fees(1, fee_amount, tx_amount, _size),
    do: :math.ceil(tx_amount * (fee_amount / 100)) |> trunc()

  def calc_fees(2, fee_amount, _tx_amount, _size), do: trunc(fee_amount)

  def calc_fees(_, _, _, _), do: throw("Wrong fee type")

  def multi_insert(multi, tx, channel) do
    Ecto.Multi.insert_all(multi, :tx, Tx, [tx],
      prefix: channel,
      returning: false
    )
  end
end
