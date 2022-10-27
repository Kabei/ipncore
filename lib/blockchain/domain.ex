defmodule Ipncore.Domain do
  alias __MODULE__
  use Ecto.Schema
  import Ecto.Query
  alias Ipncore.Repo

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(name address email avatar)
  @edit_fields ~w(address enabled email avatar)

  @primary_key {:name, :string, []}
  schema "domain" do
    field(:address, :binary)
    field(:email, :string)
    field(:avatar, :string)
    field(:enabled, :boolean, default: true)
    field(:records, :integer, default: 0)
    field(:created_at, :integer)
    field(:renewed_at, :integer)
    field(:updated_at, :integer)
  end

  def cast(params, filter, time) do
    Map.take(params, filter)
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
    |> Keyword.merge(%{created_at: time, updated_at: time})
  end

  def new(%{"name" => name, "address" => address} = params, time) do
    data = cast(params, @fields)

    struct(Domain, data)
  end

  def new(_), do: throw(0)

  def fetch!(name, channel) do
    from(d in Domain, where: d.name == ^name and d.enabled)
    |> Repo.one!(prefix: channel)
  end

  def fetch_and_check_delay(name, time, channel) do
    from(d in Domain,
      where: d.name == ^name and d.enabled and d.updated_at + @delay_edit < ^time
    )
    |> Repo.one(prefix: channel)
  end

  def get(name, channel) do
    from(d in Domain, where: d.name == ^name and d.enabled)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def exists?(name, channel) do
    from(d in Domain, where: d.enabled and d.name == ^name)
    |> Repo.exists?(prefix: channel)
  end

  def multi_insert(multi, name, data, time, channel) do
    data_struct = new(data, time)

    Ecto.Multi.insert(
      multi,
      name,
      data_struct,
      returning: false,
      prefix: channel
    )
  end

  def multi_update(multi, multi_name, name, params, time, channel, opts \\ []) do
    query =
      case Keyword.get(opts, :check_delay, false) do
        true ->
          from(d in Domain, where: d.name == ^name)

        false ->
          from(d in Domain,
            where: d.name == ^name and d.enabled and d.updated_at + @delay_edit < ^time
          )
      end

    params = cast(params, @edit_fields, time)

    Ecto.Multi.update_all(
      multi,
      multi_name,
      query,
      [set: params],
      returning: false,
      prefix: channel
    )
  end

  def multi_delete(multi, multi_name, name, channel) do
    query = from(d in Domain, where: d.name == ^name)

    Ecto.Multi.delete_all(
      multi,
      multi_name,
      query,
      returning: false,
      prefix: channel
    )
  end

  # tx register
  def processing(
        %{
          "channel" => channel,
          "inputs" => inputs,
          "outputs" => outputs,
          "sigs" => sigs,
          "data" => data,
          "time" => time,
          "type" => type,
          "pool" => pool_hostname,
          "version" => version
        } = params
      ) do
    # check size inputs
    in_count = length(inputs)
    if in_count == 0 or in_count > @max_inputs, do: throw(40203)

    # check size outputs
    out_count = length(outputs)
    if out_count == 0 or out_count > @max_outputs, do: throw(40204)

    # get utxo
    input_references = Txi.decode_references(inputs)
    utxo = Utxo.get(input_references, channel)
    if length(utxo) != in_count, do: throw(40206)
    IO.inspect(utxo)

    {txo, _txo_ids, txo_tokens, txo_address, txo_token_values, txo_total} = Txo.extract!(outputs)

    {txi, utxo_ids, utxo_tokens, utxo_address, utxo_token_values, utxo_total} =
      Utxo.extract!(utxo)

    # check utxo and outputs totals
    if utxo_total != txo_total, do: throw(40207)
    # check utxo and outputs token-values
    if utxo_token_values != txo_token_values, do: throw(40207)
    # check default token exists
    if @token not in utxo_tokens, do: throw(40213)
    # check utxo and outputs token list 
    if utxo_tokens != txo_tokens, do: throw(40213)
    # check utxo and outputs address list
    if Enum.sort(utxo_address) == Enum.sort(txo_address), do: throw(40235)

    # fetch pool data from hostname
    pool = Pool.fetch!(pool_hostname, channel)

    next_index = Block.next_index(time)
    genesis_time = Chain.genesis_time()

    # extract outgoings and incomes
    {outgoings, incomes} = extract_balances(utxo, txo)

    # check output has domain price
    case type do
      1300 ->
        domain_size = length(domain)

        domain_price =
          cond do
            domain_size < 6 ->
              100

            domain_size < 9 ->
              75

            true ->
              5
          end

        send_txo = %{tid: "USD", address: PlatformOwner.address(), type: "S", value: domain_price}
        if send_txo not in txo, do: throw(40207)

      true ->
        nil
    end

    tx =
      %{
        inputs: txi,
        outputs: txo,
        block_index: next_index,
        in_count: in_count,
        out_count: out_count,
        total_input: txo_total,
        time: time,
        type: type,
        vsn: version,
        status: Tx.status_approved(),
        outgoings: outgoings,
        incomes: incomes
      }
      |> Tx.put_hash()
      |> Tx.put_signatures(sigs, utxo_address)
      |> Tx.put_size()
      |> Tx.put_index(genesis_time)
      |> Tx.put_outputs_index()
      |> Tx.put_inputs_index()
      |> Tx.put_fees_data(utxo_address, pool.address, pool.fee, pool.percent, data)

    Ecto.Multi.new()
    |> Tx.multi_insert(tx, channel)
    |> multi_insert(:domain, data, time, channel)
    |> Repo.transaction()
    |> case do
      {:ok, _} ->
        # set available txos
        Txo.update_txid_avail(tx.index, channel, true)

        {:ok, tx}

      err ->
        Logger.error("Tx error database")
        IO.inspect(err)
        {:error, 500}
    end
  end
end
