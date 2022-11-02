defmodule Ipncore.Balance do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Repo, Token, Tx, Txi, Txo, Event, Address}
  alias __MODULE__

  @output_type_send "S"
  @output_type_fee "%"

  @base :balances

  schema "balances" do
    field(:address, :binary)
    field(:token, :binary)
    field(:amount, Ecto.Amount, default: 0)
    field(:locked, Ecto.Amount, default: 0)
    field(:out_count, Ecto.Amount, default: 0)
    field(:in_count, Ecto.Amount, default: 0)
    field(:tx_count, Ecto.Amount, default: 0)
    field(:created_at, :integer, default: 0)
    field(:updated_at, :integer, default: 0)
  end

  defmacrop balance_select() do
    quote do
      %{
        address: b.address,
        amount: b.amount,
        locked: b.locked,
        token: b.token,
        decimal: tk.decimals,
        symbol: tk.symbol,
        out_count: b.out_count,
        in_count: b.in_count,
        tx_count: b.tx_count,
        created_at: tk.created_at
      }
    end
  end

  @doc """
  keys_values = %{{address, token} => integer_postive_or_negative, ...}
  """
  def update!(keys, keys_values) do
    CubDB.get_and_update_multi(@base, keys, fn entries ->
      new_balances =
        for {k, v} <- entries, into: %{} do
          new_val = keys_values[k]
          if new_val < 0 and v < new_val, do: raise(RuntimeError, message: "balance is too low")
          {k, v + new_val}
        end

      {:ok, new_balances, []}
    end)
  end

  def activity(address58, params) do
    address = Address.from_text(address58)

    # veces que he pagado
    iquery =
      from(txi in Txi,
        join: tx in Tx,
        on: tx.id == txo.txid,
        join: ev in Event,
        on: ev.id == tx.id,
        where: txi.address == ^address,
        select: %{
          id: tx.id,
          type: ev.type,
          otype: @output_type_send,
          address: txi.address,
          memo: tx.memo,
          token: txi.token,
          value: txi.value,
          time: tx.time,
          ix: txi.ix,
          status: ev.status,
          received: false
        }
      )

    # veces que me han pagado
    oquery =
      from(txo in Txo,
        join: tx in Tx,
        on: tx.id == txo.txid,
        join: ev in Event,
        on: ev.id == tx.id,
        where: txo.address == ^address,
        select: %{
          id: tx.id,
          type: ev.type,
          otype: txo.type,
          address: txo.address,
          memo: tx.memo,
          token: txo.token,
          value: txo.value,
          time: tx.time,
          ix: txo.ix,
          status: ev.status,
          received: fragment("CASE WHEN ? = '%' THEN FALSE ELSE TRUE", txo.type)
        },
        union: ^iquery
      )

    from(s in subquery(oquery),
      select: %{
        id: s.id,
        type: s.type,
        otype: s.otype,
        address: s.address,
        memo: s.memo,
        token: s.token,
        value: s.value,
        time: s.time,
        ix: s.ix,
        status: s.status,
        received: s.received
      }
      # group_by: [s.address, s.status, s.token, s.time, s.received]
    )
    |> filter_token(params)
    |> Tx.filter_date(params)
    |> filter_operation(params)
    |> filter_type(params)
    |> filter_limit(params, 50, 100)
    |> filter_offset(params)
    |> activity_sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> Enum.map(fn x ->
      %{
        id: Event.encode_id(x.id),
        address: Address.to_text(x.address),
        memo: x.memo,
        status: Event.status_name(x.status),
        type: Event.type_name(x.type),
        otype: x.otype,
        token: x.token,
        value: x.value,
        time: x.time,
        fee: x.otype == @output_type_fee,
        ix: x.ix,
        received: x.received
      }
    end)
  end

  defp filter_token(query, %{"token" => token}) do
    where(query, [s], s.token == ^token)
  end

  defp filter_token(query, _), do: query

  defp filter_type(query, %{"type" => type}) do
    where(query, [s], s.type == ^Event.type_index(type))
  end

  defp filter_type(query, _), do: query

  defp filter_operation(query, %{"operation" => "in"}) do
    where(query, [s], s.received == true)
  end

  defp filter_operation(query, %{"operation" => "out"}) do
    where(query, [s], s.received == false)
  end

  defp filter_operation(query, _), do: query

  defp activity_sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [s], asc: fragment("length(?)", s.id), asc: s.id, asc: s.ix)

      _ ->
        order_by(query, [s], desc: fragment("length(?)", s.id), desc: s.id, asc: s.ix)
    end
  end

  defp transform(nil), do: nil

  defp transform(x) when is_list(x) do
    Enum.map(x, &%{&1 | address: Address.to_text(&1.address)})
  end

  defp transform(x) do
    %{x | address: Address.to_text(x.address)}
  end

  def fetch_balance(address, token, channel) do
    from(b in Balance,
      join: tk in Token,
      on: tk.id == b.token,
      where: b.address == ^address and b.token == ^token,
      select: balance_select()
    )
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all_balance(address, params) do
    from(b in Balance,
      join: tk in Token,
      on: tk.id == b.token,
      where: b.address == ^address
    )
    |> balance_values(address, params)
    |> balance_types(params)
    |> filter_limit(params, 10, 100)
    |> filter_offset(params)
    |> balance_select(params)
    |> balance_sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> balance_transform()
  end

  defp balance_values(query, address, %{"show" => "all"}) do
    subset =
      from(b in Balance,
        distinct: true,
        join: tk in Token,
        on: tk.id == b.token,
        where: b.address == ^address,
        select: tk.id
      )

    sql =
      from(tk in Token,
        where: tk.id not in subquery(subset),
        select: %{
          address: ^address,
          amount: fragment("0::NUMERIC"),
          locked: fragment("0::NUMERIC"),
          token: tk.id,
          decimal: tk.decimals,
          symbol: tk.symbol,
          out_count: fragment("0::NUMERIC"),
          in_count: fragment("0::NUMERIC"),
          tx_count: fragment("0::NUMERIC"),
          created_at: tk.created_at
        }
      )

    query
    |> union(^sql)
  end

  defp balance_values(query, address, _) do
    join(query, :inner, [b], tk in Token, on: tk.id == b.token)
    |> where([b], b.address == ^address)
  end

  defp balance_types(query, %{"type" => name}) do
    normalized =
      name
      |> String.downcase()
      |> String.trim()

    type = Token.type_index(normalized)
    where(query, [_b, tk], tk.type == ^type)
  end

  defp balance_types(query, _), do: query

  defp balance_select(query, _) do
    select(query, [b, tk], balance_select())
  end

  defp balance_sort(query, %{"show" => "all"} = params) do
    case Map.get(params, "sort") do
      "newest_token" ->
        order_by(query, [], desc: fragment("created_at"))

      "oldest_token" ->
        order_by(query, [], asc: fragment("created_at"))

      "most_value" ->
        order_by(query, [], desc: fragment("amount"))

      "less_value" ->
        order_by(query, [], asc: fragment("amount"))

      _ ->
        order_by(query, [], asc: fragment("created_at"))
    end
  end

  defp balance_sort(query, params) do
    case Map.get(params, "sort") do
      "newest_token" ->
        order_by(query, [_b, tk], desc: tk.created_at)

      "oldest_token" ->
        order_by(query, [_b, tk], asc: tk.created_at)

      "recent" ->
        order_by(query, [b, _tk], desc: b.updated_at)

      "most_value" ->
        order_by(query, [b], desc: b.amount)

      "less_value" ->
        order_by(query, [b], asc: b.amount)

      _ ->
        order_by(query, [_b, tk], desc: tk.created_at)
    end
  end

  defp balance_transform(nil), do: nil

  defp balance_transform(x) when is_list(x) do
    Enum.map(x, fn x ->
      %{x | address: Address.to_text(x.address)}
      |> Map.delete(:created_at)
    end)
  end

  defp balance_transform(x) do
    %{x | address: Address.to_text(x.address)}
    |> Map.delete(:created_at)
  end

  def multi_upsert_outgoings(multi, _name, nil, _time, _channel), do: multi
  def multi_upsert_outgoings(multi, _name, [], _time, _channel), do: multi

  def multi_upsert_outgoings(multi, name, txis, time, channel) do
    structs =
      txis
      |> Enum.map(fn x ->
        %{
          address: x.address,
          token: x.token,
          amount: -x.value,
          locked: 0,
          in_count: 0,
          tx_count: 1,
          out_count: x.value,
          created_at: time,
          updated_at: time
        }
      end)

    upsert_query =
      from(w in Balance,
        where:
          w.address == fragment("EXCLUDED.address") and w.token == fragment("EXCLUDED.token"),
        update: [
          inc: [
            tx_count: 1,
            amount: fragment("EXCLUDED.amount"),
            out_count: fragment("EXCLUDED.out_count")
          ],
          set: [
            updated_at: ^time
          ]
        ]
      )

    Ecto.Multi.insert_all(multi, name, Balance, structs,
      on_conflict: upsert_query,
      conflict_target: [:address, :token],
      prefix: channel,
      returning: false
    )
  end

  def multi_upsert_incomes(multi, _name, nil, _time, _channel), do: multi
  def multi_upsert_incomes(multi, _name, [], _time, _channel), do: multi

  def multi_upsert_incomes(multi, name, txos, time, channel) do
    structs =
      txos
      |> Enum.map(fn x ->
        %{
          address: x.address,
          token: x.token,
          amount: x.value,
          locked: 0,
          in_count: x.value,
          tx_count: 1,
          out_count: 0,
          created_at: time,
          updated_at: time
        }
      end)

    IO.inspect(structs)

    upsert_query =
      from(w in Balance,
        where:
          w.address == fragment("EXCLUDED.address") and w.token == fragment("EXCLUDED.token"),
        update: [
          inc: [
            tx_count: 1,
            amount: fragment("EXCLUDED.amount"),
            in_count: fragment("EXCLUDED.in_count")
          ],
          set: [
            updated_at: ^time
          ]
        ]
      )

    Ecto.Multi.insert_all(multi, name, Balance, structs,
      on_conflict: upsert_query,
      conflict_target: [:address, :token],
      # stale_error_field: :address,
      prefix: channel,
      returning: false
    )
  end
end
