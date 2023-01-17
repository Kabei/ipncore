defmodule Ipncore.Balance do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Repo, Token, Tx, Txo, Event, Address, Util}
  alias __MODULE__

  # @output_reason_send "S"
  # @output_reason_fee "%"

  @base :balances

  schema "balances" do
    field(:address, :binary)
    field(:token, :binary)
    field(:amount, Ecto.Amount, default: 0)
    field(:locked, :boolean, default: false)
    field(:out_count, Ecto.Amount, default: 0)
    field(:in_count, Ecto.Amount, default: 0)
    field(:tx_count, Ecto.Amount, default: 0)
    field(:created_at, :integer, default: 0)
    field(:updated_at, :integer, default: 0)
  end

  def open do
    dir_path = Application.get_env(:ipncore, :balance_path, "data/balances")
    CubDB.start_link(data_dir: dir_path, auto_compact: true, auto_file_sync: true, name: @base)
  end

  def sync do
    CubDB.file_sync(@base)
  end

  def close do
    CubDB.stop(@base)
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

  @spec check!(Tuple.t(), pos_integer) :: boolean
  def check!({_address, _token} = balance, amount) do
    case CubDB.get(@base, balance) do
      {_m, true} -> throw("Balance is locked")
      {x, _false} when x >= amount -> true
      _ -> throw("Balance is too low")
    end
  end

  @spec check_multi!(List.t(), Map.t()) :: boolean
  def check_multi!(list_keys, map_balance_to_check) do
    map_result = CubDB.get_multi(@base, list_keys)

    cond do
      %{} == map_result ->
        throw("Balance is too low")

      true ->
        Enum.each(map_balance_to_check, fn {key, amount} ->
          case Map.get(map_result, key) do
            {_m, true} -> throw("Balance is locked")
            {x, _false} when x >= amount -> true
            _ -> throw("Balance is too low")
          end
        end)
    end
  end

  @doc """
  keys = [{address, token}, ...]
  keys_values = %{{address, token} => integer_postive_or_negative, ...}
  """
  @spec update!(List.t(), Map.t()) :: :ok
  def update!(keys, keys_values) do
    CubDB.get_and_update_multi(@base, keys, fn entries ->
      new_balances =
        for key <- keys, into: %{} do
          {old_val, old_locked} = Map.get(entries, key, {0, false})
          new_val = Map.get(keys_values, key)

          if old_locked, do: throw("Address is locked")

          if old_val < 0 and new_val < old_val,
            do: throw("Insufficient balance")

          {key, {old_val + new_val, old_locked}}
        end

      {:ok, new_balances, []}
    end)
  end

  def activity(address58, params) do
    address = Address.from_text(address58)

    from(txo in Txo,
      join: ev in Event,
      on: ev.hash == txo.txid,
      join: tk in Token,
      on: tk.id == txo.token,
      left_join: d in Domain,
      on: d.owner == txo.to,
      left_join: df in Domain,
      on: d.owner == txo.from,
      select: %{
        id: txo.txid,
        type: ev.type,
        reason: txo.reason,
        token: txo.token,
        from: txo.from,
        to: txo.to,
        value: txo.value,
        decimals: tk.decimals,
        time: ev.time,
        domain_to: d.owner,
        domain_from: df.owner
      }
    )
    |> filter_operation(address, params)
    |> filter_token(params)
    |> Tx.filter_date(params)
    |> filter_reason(params)
    |> filter_type(params)
    |> filter_limit(params, 50, 100)
    |> filter_offset(params)
    |> activity_sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> Enum.map(fn x ->
      %{
        id: Event.encode_id(x.id),
        from: x.domain_from || Address.to_text(x.from),
        reason: x.reason,
        time: x.time,
        to: x.domain_to || Address.to_text(x.to),
        token: x.token,
        type: Event.type_name(x.type),
        value: Util.to_decimal(x.value, x.decimals)
      }
    end)
  end

  defp filter_token(query, %{"token" => token}) do
    where(query, [s], s.token == ^token)
  end

  defp filter_token(query, _), do: query

  defp filter_type(query, %{"type" => type}) do
    ev_type = Event.type_index(type) || ""
    where(query, [_o, ev], ev.type == ^ev_type)
  end

  defp filter_type(query, _), do: query

  defp filter_reason(query, %{"reason" => reason}) do
    where(query, [s], s.reason == ^reason)
  end

  defp filter_reason(query, _), do: query

  defp filter_operation(query, address, %{"operation" => "in"}) do
    where(query, [s], s.to == ^address)
  end

  defp filter_operation(query, address, %{"operation" => "out"}) do
    where(query, [s], s.from == ^address)
  end

  defp filter_operation(query, address, _) do
    where(query, [s], s.from == ^address or s.to == ^address)
  end

  defp activity_sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [s, ev], asc: ev.block_index, asc: ev.time, desc: ev.hash, desc: s.ix)

      _ ->
        order_by(query, [s, ev], desc: ev.block_index, desc: ev.time, asc: ev.hash, asc: s.ix)
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
          locked: fragment("FALSE"),
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

  def multi_upsert_coinbase(multi, _name, nil, _time, _channel), do: multi
  def multi_upsert_coinbase(multi, _name, [], _time, _channel), do: multi

  def multi_upsert_coinbase(multi, name, txs, time, channel) do
    structs =
      Enum.reduce(txs, [], fn x, acc ->
        acc ++
          [
            %{
              address: x.to,
              token: x.token,
              amount: x.value,
              locked: false,
              in_count: x.value,
              tx_count: 1,
              out_count: 0,
              created_at: time,
              updated_at: time
            }
          ]
      end)

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
      prefix: channel,
      returning: false
    )
  end

  def multi_upsert(multi, _name, nil, _time, _channel), do: multi
  def multi_upsert(multi, _name, [], _time, _channel), do: multi

  def multi_upsert(multi, name, txs, time, channel) do
    structs =
      Enum.reduce(txs, [], fn x, acc ->
        acc ++
          [
            %{
              address: x.from,
              token: x.token,
              amount: -x.value,
              locked: false,
              in_count: 0,
              tx_count: 1,
              out_count: x.value,
              created_at: time,
              updated_at: time
            },
            %{
              address: x.to,
              token: x.token,
              amount: x.value,
              locked: false,
              in_count: x.value,
              tx_count: 1,
              out_count: 0,
              created_at: time,
              updated_at: time
            }
          ]
      end)
      |> Enum.group_by(fn x -> {x.address, x.token} end)
      |> Map.values()
      |> Enum.reduce([], fn x, acc ->
        acc ++
          case x do
            [_y] = g ->
              g

            z ->
              {amount, in_count, out_count} =
                Enum.reduce(z, {0, 0, 0}, fn z, {acc, acc2, acc3} ->
                  {z.amount + acc, z.in_count + acc2, z.out_count + acc3}
                end)

              [
                z
                |> List.first()
                |> Map.merge(%{amount: amount, in_count: in_count, out_count: out_count})
              ]
          end
      end)

    upsert_query =
      from(w in Balance,
        where:
          w.address == fragment("EXCLUDED.address") and w.token == fragment("EXCLUDED.token"),
        update: [
          inc: [
            tx_count: 1,
            amount: fragment("EXCLUDED.amount"),
            out_count: fragment("EXCLUDED.out_count"),
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
      prefix: channel,
      returning: false
    )
  end

  def check_lock!(from, to, token_id, value) when is_boolean(value) do
    token = Token.fetch!(token_id, from)

    unless Token.check_opts(token, "lock"), do: throw("Operation not allowed")

    case CubDB.get(@base, {to, token_id}) do
      {_amount, x} ->
        if x == value do
          case value do
            true ->
              throw("Balance is already locked")

            false ->
              throw("Balance is already unlocked")
          end
        else
          :ok
        end

      _ ->
        throw("Balance not exists")
    end
  end

  def lock!(multi, to, token, value, time, channel) do
    CubDB.get_and_update(@base, {to, token}, fn {amount, _locked} ->
      {
        :ok,
        {amount, value}
      }
    end)

    multi_locked(multi, :balance, to, token, value, time, channel)
  end

  def multi_locked(multi, name, address, token, value, time, channel) do
    query = from(w in Balance, where: w.address == ^address and w.token == ^token)

    Ecto.Multi.update_all(multi, name, query, [set: [locked: value, updated_at: time]],
      prefix: channel,
      returning: false
    )
  end
end
