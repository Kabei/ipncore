defmodule Ipncore.Tx do
  use Ecto.Schema
  require Logger
  import Ecto.Query, only: [from: 2, where: 3, select: 3, order_by: 3, join: 5]
  import Ipnutils.Filters
  import Ipncore.Util, only: [empty?: 1]
  alias Ipncore.{Address, Balance, Block, Event, Txo, Balance, Token, Validator, Repo, Util}
  alias __MODULE__

  # global
  @unit_time Default.unit_time()
  @token Default.token()
  @imposible Default.imposible_address()
  # reasons
  @output_reason_send "S"
  @output_reason_coinbase "C"
  @output_reason_fee "%"
  @output_reason_refund "R"
  # limits
  @memo_max_size 100
  # refund
  @base_refunds :refunds
  @filename_refunds "refunds.db"

  @primary_key {:id, :binary, []}
  schema "tx" do
    field(:out_count, :integer)
    field(:token_value, :map)
    field(:fee, :integer, default: 0)
    field(:memo, :string)
  end

  def open do
    dir_path = Default.data_dir()
    refunds_file = Path.join([dir_path, @filename_refunds])
    DetsPlus.open_file(@base_refunds, file: refunds_file, auto_save: 5_000)
  end

  def close do
    DetsPlus.close(@base_refunds)
  end

  def refund_exists?(time, hash) do
    DetsPlus.member?(@base_refunds, {time, hash})
  end

  def check_refund_exists!(time, hash) do
    case refund_exists?(time, hash) do
      true ->
        throw("Refund already exists")

      false ->
        :ok
    end
  end

  def put_refund!(time, hash) do
    case DetsPlus.insert_new(@base_refunds, {{time, hash}}) do
      false ->
        throw("Refund already exists")

      true ->
        true
    end
  end

  defmacro map_select do
    quote do
      %{
        id: fragment("encode(?, 'hex')", tx.id),
        out_count: tx.out_count,
        time: ev.time,
        token_value: tx.token_value,
        fee: tx.fee,
        memo: tx.memo
      }
    end
  end

  def check_send!(
        from_address,
        to_address,
        token,
        amount,
        validator_host,
        event_size
      )
      when token == @token do
    if amount <= 0, do: throw("Invalid amount to send")

    if from_address == to_address or to_address == @imposible,
      do: throw("Invalid address to send")

    validator = Validator.fetch!(validator_host)
    fee_total = calc_fees(validator.fee_type, validator.fee, amount, event_size)

    Balance.check!({from_address, token}, amount + fee_total)
    :ok
  end

  def check_send!(
        from_address,
        to_address,
        token,
        amount,
        validator_host,
        event_size
      )
      when token != @token do
    if amount <= 0, do: throw("Invalid amount to send")

    if from_address == to_address or to_address == @imposible,
      do: throw("Invalid address to send")

    validator = Validator.fetch!(validator_host)
    fee_total = calc_fees(validator.fee_type, validator.fee, amount, event_size)

    Balance.check_multi!([{from_address, token}, {from_address, @token}], %{
      {from_address, token} => amount,
      {from_address, @token} => fee_total
    })

    :ok
  end

  def check_fee!(
        from_address,
        fee_total
      ) do
    if fee_total <= 0, do: throw("Invalid fee amount to send")
    Balance.check!({from_address, @token}, fee_total)
    :ok
  end

  def check_refund!(from_address, tx_time, tx_hash) do
    check_refund_exists!(tx_time, tx_hash)

    case Event.lookup(tx_hash, tx_time) do
      [{_hash, _time, _block_index, _version, type_number, from, body, _signature}] ->
        [_token, my_to_address, _amount, _validator_host, _memo] = body

        cond do
          from == from_address or Address.from_text(my_to_address) != from_address ->
            throw("Action is not allowed")

          type_number == 211 ->
            :ok

          type_number == 212 ->
            # not support yet
            throw("Invalid event-type to refund")

          true ->
            throw("Invalid event-type to refund")
        end

      _ ->
        throw("Transaction does not exist")
    end
  end

  def send!(
        multi,
        txid,
        token_id,
        from_address,
        to_address,
        amount,
        validator_host,
        event_size,
        memo,
        refund,
        timestamp,
        channel
      ) do
    if not empty?(memo) and byte_size(memo) > @memo_max_size, do: throw("Invalid memo size")

    token = Token.fetch!(token_id)
    validator = Validator.fetch!(validator_host)
    validator_address = validator.owner

    {outputs, fee_total} =
      if not refund do
        fee_total = calc_fees(validator.fee_type, validator.fee, amount, event_size)

        Balance.update!(
          [
            {from_address, token_id},
            {to_address, token_id},
            {from_address, @token},
            {validator_address, @token}
          ],
          %{
            {from_address, token_id} => -(amount + fee_total),
            {to_address, token_id} => amount,
            {from_address, @token} => -fee_total,
            {validator_address, @token} => fee_total
          }
        )

        outputs = [
          %{
            txid: txid,
            ix: 0,
            from: from_address,
            to: to_address,
            value: amount,
            token: token_id,
            reason: @output_reason_send
          },
          %{
            txid: txid,
            ix: 1,
            token: @token,
            from: from_address,
            to: validator_address,
            value: fee_total,
            reason: @output_reason_fee
          }
        ]

        {outputs, fee_total}
      else
        Balance.update!(
          [
            {from_address, token_id},
            {to_address, token_id}
          ],
          %{
            {from_address, token_id} => -amount,
            {to_address, token_id} => amount
          }
        )

        outputs = [
          %{
            txid: txid,
            ix: 0,
            from: from_address,
            to: to_address,
            value: amount,
            token: token_id,
            reason: @output_reason_refund
          }
        ]

        {outputs, 0}
      end

    tx = %{
      id: txid,
      fee: fee_total,
      token_value: token_values(outputs, token.decimals),
      out_count: length(outputs),
      memo: memo
    }

    multi
    |> multi_insert(tx, channel)
    |> Txo.multi_insert_all(:txo, outputs, channel)
    |> Balance.multi_upsert(:balances, outputs, timestamp, channel)
  end

  defp token_values(outputs, decimals) do
    Enum.reduce(outputs, %{}, fn %{token: token, value: new_value}, acc ->
      value_dec = Util.to_decimal(new_value, decimals)

      case Map.get(acc, token) do
        nil ->
          Map.put(acc, token, value_dec)

        old_value ->
          Map.put(acc, token, old_value + value_dec)
      end
    end)
  end

  def check_coinbase!(from_address, token_id, memo) do
    if not empty?(memo) and byte_size(memo) > @memo_max_size, do: throw("Invalid memo size")

    token = Token.fetch!(token_id, from_address)
    unless Token.check_opts(token, "coinbase"), do: throw("Operation not allowed")

    :ok
  end

  def coinbase!(
        multi,
        txid,
        token_id,
        from_address,
        init_outputs,
        memo,
        timestamp,
        channel
      ) do
    {outputs, keys_entries, entries, token_value, amount} =
      outputs_extract_coinbase!(txid, init_outputs, token_id)

    # check max supply
    token = Token.fetch!(token_id, from_address)
    new_supply = Token.check_max_supply!(token, amount)

    Balance.update!(keys_entries, entries)

    tx = %{
      id: txid,
      fee: 0,
      token_value: token_value,
      memo: memo,
      out_count: length(outputs)
    }

    Token.put_supply(token, new_supply)

    multi
    |> multi_insert(tx, channel)
    |> Txo.multi_insert_all(:txo, outputs, channel)
    |> Token.multi_update_stats(:token, token_id, [supply: amount], timestamp, channel)
    |> Balance.multi_upsert_coinbase(:balances, outputs, timestamp, channel)
  end

  def send_fee!(multi, txid, from_address, validator_host, fee_amount, timestamp, channel) do
    validator = Validator.fetch!(validator_host)
    validator_address = validator.owner

    Balance.update!(
      [{from_address, @token}, {validator_address, @token}],
      %{
        {from_address, @token} => -fee_amount,
        {validator_address, @token} => fee_amount
      }
    )

    outputs = [
      %{
        txid: txid,
        ix: 0,
        from: from_address,
        to: validator_address,
        token: @token,
        value: fee_amount,
        reason: @output_reason_fee
      }
    ]

    tx = %{
      id: txid,
      fee: fee_amount,
      token_value: %{@token => fee_amount},
      out_count: length(outputs)
    }

    multi
    |> multi_insert(tx, channel)
    |> Txo.multi_insert_all(:txo, outputs, channel)
    |> Balance.multi_upsert(:balances, outputs, timestamp, channel)
  end

  def refund!(multi, hash, from_address, tx_time, tx_hash, event_size, timestamp, channel) do
    check_refund_exists!(tx_time, tx_hash)

    [{_hash, _time, _block_index, _version, type_number, to_address, body, _signature}] =
      Event.lookup(tx_hash, tx_time)

    case type_number do
      211 ->
        [token, _my_to_address, amount, validator_host, memo] = body

        multi =
          send!(
            multi,
            hash,
            token,
            from_address,
            Address.from_text(to_address),
            amount,
            validator_host,
            event_size,
            memo,
            true,
            timestamp,
            channel
          )

        put_refund!(tx_time, tx_hash)
        multi

      212 ->
        # refund to tx.sendmulti not support yet
        throw("Invalid event-type to refund")

      _ ->
        throw("Invalid event-type to refund")
    end
  end

  defp outputs_extract_coinbase!(txid, txos, token) do
    {txos, key_entries, entries, amount, _ix} =
      Enum.reduce(txos, {[], [], %{}, 0, 0}, fn [address, value],
                                                {acc_txos, acc_keys, acc_entries, acc_amount,
                                                 acc_ix} ->
        if value <= 0, do: throw("Output has value zero")
        bin_address = Address.from_text(address)

        output = %{
          txid: txid,
          ix: acc_ix,
          token: token,
          to: bin_address,
          reason: @output_reason_coinbase,
          value: value
        }

        entry = {bin_address, token}
        acc_entries = Map.put(acc_entries, entry, value)

        {acc_txos ++ [output], acc_keys ++ [entry], acc_entries, acc_amount + value, acc_ix + 1}
      end)

    token_value = Map.new() |> Map.put(token, amount)

    {txos, key_entries, entries, token_value, amount}
  end

  # 0 -> by size
  # 1 -> by percent
  # 2 -> fixed price
  defp calc_fees(0, fee_amount, _tx_amount, size),
    do: trunc(fee_amount) * size

  defp calc_fees(1, fee_amount, tx_amount, _size),
    do: :math.ceil(tx_amount * (fee_amount / 100)) |> trunc()

  defp calc_fees(2, fee_amount, _tx_amount, _size), do: trunc(fee_amount)

  defp calc_fees(_, _, _, _), do: throw("Wrong fee type")

  defp multi_insert(multi, tx, channel) do
    Ecto.Multi.insert_all(multi, :tx, Tx, [tx],
      prefix: channel,
      returning: false
    )
  end

  def one(txid, params) do
    from(tx in Tx,
      join: ev in Event,
      on: ev.hash == tx.id,
      where: tx.id == ^txid,
      select: map_select(),
      limit: 1
    )
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
  end

  def one_by_hash(hash, params) do
    from(tx in Tx,
      join: ev in Event,
      on: ev.hash == tx.id,
      where: ev.hash == ^hash,
      select: map_select(),
      limit: 1
    )
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
  end

  def all(params) do
    from(tx in Tx, join: ev in Event, on: ev.hash == tx.id)
    |> filter_index(params)
    |> filter_select(params)
    |> filter_offset(params)
    |> filter_date(params)
    |> filter_limit(params, 50, 100)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
  end

  defp filter_index(query, %{"hash" => hash}) do
    where(query, [_tx, ev], ev.hash == ^hash)
  end

  defp filter_index(query, %{"block_index" => block_index}) do
    where(query, [_tx, ev], ev.block_index == ^block_index)
  end

  defp filter_index(query, %{"block_height" => block_height}) do
    query
    |> join(:inner, [_tx, ev], b in Block, on: b.index == ev.block_index)
    |> where([_tx, ev, b], b.height == ^block_height)
  end

  defp filter_index(query, %{"q" => q}) do
    binq = Utils.decode16(q)

    where(query, [tx], tx.hash == ^binq)
  end

  defp filter_index(query, _), do: query

  def filter_date(query, %{"date_from" => from_date, "date_to" => to_date}) do
    date_start = Utils.from_date_to_time(from_date, :start, @unit_time)

    date_end = Utils.from_date_to_time(to_date, :end, @unit_time)

    where(query, [_tx, ev], ev.time >= ^date_start and ev.time <= ^date_end)
  end

  def filter_date(query, %{"date_from" => from_date}) do
    date_start = Utils.from_date_to_time(from_date, :start, @unit_time)

    where(query, [_tx, ev], ev.time >= ^date_start)
  end

  def filter_date(query, %{"date_to" => to_date}) do
    date_end = Utils.from_date_to_time(to_date, :end, @unit_time)

    where(query, [_tx, ev], ev.time <= ^date_end)
  end

  def filter_date(query, _), do: query

  defp filter_select(query, _), do: select(query, [tx, ev], map_select())

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [_tx, ev], asc: ev.block_index, asc: ev.time, desc: ev.hash)

      _ ->
        order_by(query, [_tx, ev], desc: ev.block_index, desc: ev.time, asc: ev.hash)
    end
  end
end
