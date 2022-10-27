defmodule Ipncore.Utxo do
  alias Ipncore.{Repo, Txo}

  @spec get([binary()], String.t()) :: [Txo.t()]
  def get(oids, channel) do
    %{rows: rows} =
      Ecto.Adapters.SQL.query!(Repo, "SELECT * FROM sys.utxo($1, $2)", [oids, "\"#{channel}\""])

    Enum.map(rows, fn [id, tid, type, value, address] ->
      %Txo{
        id: id,
        tid: tid,
        value: value,
        type: type,
        address: address,
        avail: true
      }
    end)
  end

  @spec get_by_address([binary()], String.t(), pos_integer(), String.t()) :: [Txo.t()]
  def get_by_address(addresses, token_id, total, channel) do
    %{rows: rows, num_rows: num_rows} =
      Ecto.Adapters.SQL.query!(Repo, "SELECT * FROM sys.uoutputs($1, $2, $3, $4)", [
        addresses,
        token_id,
        total,
        "\"#{channel}\""
      ])

    case num_rows do
      0 ->
        []

      num_rows ->
        {outptus_ids, balance} =
          Enum.reduce(rows, {[], 0}, fn [id, address, tid, type, value, balance], {acc, _} ->
            {acc ++
               [
                 %{
                   address: address,
                   id: id,
                   tid: tid,
                   type: type,
                   value: value
                 }
               ], balance}
          end)

        %{
          balance: balance,
          total: num_rows,
          utxo: outptus_ids
        }
    end
  end

  @doc """
  Fetch UTXO by address format
  """
  @spec fetch_by_address([binary()], String.t(), pos_integer(), String.t()) :: Map.t()
  # def fetch_by_address(address, token_id, total, channel) when is_binary(address),
  #   do: fetch_by_address([address], token_id, total, channel)

  def fetch_by_address(addresses, token_id, total, channel) do
    %{rows: rows, num_rows: num_rows} =
      Ecto.Adapters.SQL.query!(Repo, "SELECT * FROM sys.uoutputs($1, $2, $3, $4)", [
        addresses,
        token_id,
        total,
        "\"#{channel}\""
      ])

    case num_rows do
      0 ->
        []

      num_rows ->
        {outptus_ids, balance} =
          Enum.reduce(rows, {[], 0}, fn [id, address, tid, type, value, balance], {acc, _} ->
            {acc ++
               [
                 %{
                   address: Base58Check.encode(address),
                   id: Base62.encode(id),
                   tid: tid,
                   type: type,
                   value: value
                 }
               ], balance}
          end)

        %{
          balance: balance,
          total: num_rows,
          utxo: outptus_ids
        }
    end
  end

  def fetch_by_address_multi(addresses, token_id, total, prefix) do
    %{rows: rows, num_rows: num_rows} =
      Ecto.Adapters.SQL.query!(Repo, "SELECT * FROM sys.uoutputs_multi($1, $2, $3, $4)", [
        addresses,
        token_id,
        total,
        prefix
      ])

    case num_rows do
      0 ->
        []

      num_rows ->
        {outptus_ids, balance} =
          Enum.reduce(rows, {[], 0}, fn [id, address, tid, type, value, channel, balance],
                                        {acc, _} ->
            {acc ++
               [
                 %{
                   address: Base58Check.encode(address),
                   channel: channel,
                   id: Base62.encode(id),
                   tid: tid,
                   type: type,
                   value: value
                 }
               ], balance}
          end)

        %{
          balance: balance,
          total: num_rows,
          utxo: outptus_ids
        }
    end
  end

  def extract!(utxos) do
    {inputs, ids, tokens, address, token_value, value} =
      Enum.reduce(utxos, {[], [], [], [], %{}, 0}, fn x,
                                                      {inputs, ids, tokens, addresses, tv, values} ->
        # convert address base58 to binary
        # addr = Base58Check.decode(x.address)

        # check output value major than zero
        if x.value <= 0, do: throw(40207)

        # check output types
        if x.type not in [@output_type_send, @output_type_fee, @output_type_return],
          do: throw(40207)

        input = Txi.create(x)

        value = Map.get(tv, x.tid, 0) + x.value
        token_value = Map.put(tv, x.tid, value)

        {inputs ++ [input], ids ++ [x.id], tokens ++ [x.tid], addresses ++ [x.address],
         token_value, values + x.value}
      end)

    {inputs, ids, Enum.uniq(tokens) |> Enum.sort(), Enum.uniq(address), token_value, value}
  end
end
