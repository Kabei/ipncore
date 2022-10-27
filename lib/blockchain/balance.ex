defmodule Ipncore.Balance do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Repo, Token, Tx, Txi, Txo, TxData}
  alias __MODULE__

  @type t :: %__MODULE__{
          address: binary(),
          tid: binary(),
          amount: pos_integer(),
          out_count: pos_integer(),
          in_count: pos_integer(),
          tx_count: pos_integer(),
          created_at: pos_integer(),
          updated_at: pos_integer()
        }

  @output_type_fee "%"

  @primary_key {:address, :binary, []}
  schema "balances" do
    field(:tid, :binary)
    field(:amount, Ecto.Amount, default: 0)
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
        token: b.tid,
        decimal: tk.decimals,
        symbol: tk.symbol,
        out_count: b.out_count,
        in_count: b.in_count,
        tx_count: b.tx_count,
        created_at: tk.created_at
      }
    end
  end

  defmacrop is_fees(oindex, ifees) do
    quote do
      fragment(
        unquote("EXISTS(SELECT ? IN coalesce(?,?))"),
        unquote(oindex),
        unquote(ifees),
        unquote([])
      )
    end
  end

  def activity(address58, params) do
    address = Base58Check.decode(address58)

    # veces que he pagado
    iquery =
      from(txi in Txi,
        join: tx in Tx,
        on: tx.index == txi.txid,
        left_join: txd in TxData,
        on: tx.memo and txi.txid == txd.txid,
        join: txo in Txo,
        on: txo.id == txi.oid,
        join: txo2 in Txo,
        on: fragment("substring(?::bytea from 1 for length(?)) = ?", txo2.id, tx.index, tx.index),
        # and txo2.address != ^address,
        where: txo.address == ^address and txo2.address != ^address,
        select: %{
          id: tx.index,
          type: tx.type,
          otype: txo2.type,
          address: txo2.address,
          memo: txd.data,
          token: txo2.tid,
          value: txo2.value,
          time: tx.time,
          oid: txo2.id,
          status: tx.status,
          fees: false,
          received: false
        }
      )

    # veces que me han pagado
    oquery =
      from(txo in Txo,
        join: tx in Tx,
        on: fragment("substring(?::bytea from 1 for length(?)) = ?", txo.id, tx.index, tx.index),
        left_join: txd in TxData,
        on: tx.memo and tx.index == txd.txid,
        left_join: txi in Txi,
        on: txi.txid == tx.index,
        left_join: otxi in Txo,
        on: otxi.id == txi.oid,
        # and txo.address == ^address, #txo.address == ^address and (is_nil(txi.oid) or (not is_nil(txi.oid) and otxi.address != ^address)),
        where:
          txo.address == ^address and
            (is_nil(txi.oid) or
               (not is_nil(txi.oid) and otxi.address != ^address)),
        select: %{
          id: tx.index,
          type: tx.type,
          otype: txo.type,
          address: otxi.address,
          memo: txd.data,
          token: txo.tid,
          value: txo.value,
          time: tx.time,
          oid: txo.id,
          status: tx.status,
          fees: false,
          received: true
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
        status: s.status,
        time: s.time,
        fees: s.fees,
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
        id: Base62.encode(x.id),
        address:
          if(is_nil(x.address),
            do: String.capitalize(Tx.type_name(x.type)),
            else: Base58Check.encode(x.address)
          ),
        memo: x.memo,
        status: Tx.status_name(x.status),
        type: Tx.type_name(x.type),
        token: x.token,
        value: x.value,
        time: x.time,
        fees: x.otype == @output_type_fee,
        received: x.received
      }
    end)
  end

  defp filter_token(query, %{"token" => token}) do
    where(query, [s], s.token == ^token)
  end

  defp filter_token(query, _), do: query

  defp filter_type(query, %{"type" => type_name}) do
    where(query, [s], s.type == ^Tx.type_index(type_name))
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
        order_by(query, [s], asc: fragment("length(?)", s.oid), asc: s.oid)

      _ ->
        order_by(query, [s], desc: fragment("length(?)", s.oid), desc: s.oid)
    end
  end

  defp transform(nil), do: nil

  defp transform(x) when is_list(x) do
    Enum.map(x, &%{&1 | address: Base58Check.encode(&1.address)})
  end

  defp transform(x) do
    %{x | address: Base58Check.encode(x.address)}
  end

  def fetch_balance(address, token, channel) do
    from(b in Balance,
      join: tk in Token,
      on: tk.id == b.tid,
      where: b.address == ^address and b.tid == ^token,
      select: balance_select()
    )
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all_balance(address, params) do
    from(b in Balance,
      join: tk in Token,
      on: tk.id == b.tid,
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
        on: tk.id == b.tid,
        where: b.address == ^address,
        select: tk.id
      )

    sql =
      from(tk in Token,
        where: tk.id not in subquery(subset),
        select: %{
          address: ^address,
          amount: fragment("0::NUMERIC"),
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
    join(query, :inner, [b], tk in Token, on: tk.id == b.tid)
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
      %{x | address: Base58Check.encode(x.address)}
      |> Map.delete(:created_at)
    end)
  end

  defp balance_transform(x) do
    %{x | address: Base58Check.encode(x.address)}
    |> Map.delete(:created_at)
  end

  def multi_upsert_outgoings(multi, _name, nil, _time, _channel), do: multi
  def multi_upsert_outgoings(multi, _name, [], _time, _channel), do: multi

  def multi_upsert_outgoings(multi, name, utxos, time, channel) do
    structs =
      utxos
      |> Enum.map(fn x ->
        %{
          address: x.address,
          tid: x.tid,
          amount: x.value,
          in_count: 0,
          tx_count: 1,
          out_count: abs(x.value),
          created_at: time,
          updated_at: time
        }
      end)

    upsert_query =
      from(w in Balance,
        where: w.address == fragment("EXCLUDED.address") and w.tid == fragment("EXCLUDED.tid"),
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
      conflict_target: [:address, :tid],
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
          tid: x.tid,
          amount: x.value,
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
        where: w.address == fragment("EXCLUDED.address") and w.tid == fragment("EXCLUDED.tid"),
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
      conflict_target: [:address, :tid],
      # stale_error_field: :address,
      prefix: channel,
      returning: false
    )
  end
end
