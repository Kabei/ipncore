defmodule Ipncore.Txo do
  use Ecto.Schema
  alias Ipncore.{Token, Tx, Repo}
  alias Ipnutils.Address
  import Ecto.Query, only: [from: 1, from: 2, where: 3, order_by: 3, select: 3, join: 5]
  import Ipnutils.Filters
  alias __MODULE__

  @type t :: %__MODULE__{
          id: binary(),
          tid: binary,
          value: pos_integer(),
          address: binary(),
          avail: boolean()
          # used: boolean()
        }

  # @channel Application.get_env(:ipncore, :channel)
  # @token Application.get_env(:ipncore, :token)

  @primary_key false
  # @derive Jason.Encoder
  schema "txo" do
    field(:id, :binary)
    field(:tid, :string)
    field(:address, :binary)
    field(:value, :integer)
    field(:avail, :boolean, default: false)
    # belongs_to(:tx, Tx, foreign_key: :txid, references: :index, type: :binary)
  end

  @spec create(binary, binary, integer, binary, integer) :: t
  def create(token_id, address, value, tx_index, index \\ 0)

  def create(_token_id, _address, value, _tx_index, index)
      when value <= 0 or index < 0,
      do: throw("outputs does not have zero or negative value or negative index")

  def create(token_id, address, value, tx_index, index) do
    %Txo{
      id: generate_index(tx_index, index),
      tid: token_id,
      address: address,
      value: value
    }
  end

  @spec order([t], integer) :: [t]
  def order(outputs, tx_index, index \\ 0)
  def order([], _tx_index, _index), do: []

  def order([output | rest], tx_index, index) do
    [
      %{
        id: generate_index(tx_index, index),
        tid: output.tid,
        address: output.address,
        value: output.value,
        avail: output.avail
      }
    ] ++
      order(rest, tx_index, index + 1)
  end

  # @spec order([t], integer) :: [t]
  # def coinbase_order(outputs, tx_index, index \\ 0)
  # def coinbase_order([], _tx_index, _index), do: []

  # def coinbase_order([output | rest], tx_index, index) do
  #   [
  #     %{
  #       id: generate_index(tx_index, index),
  #       tid: output.tid,
  #       address: output.address,
  #       value: output.value,
  #       avail: true
  #     }
  #   ] ++
  #     coinbase_order(rest, tx_index, index + 1)
  # end

  @spec compute_sum([Txo.t()]) :: integer
  def compute_sum([]), do: 0

  def compute_sum([o | rest]) do
    o.value + compute_sum(rest)
  end

  @spec extract([t]) :: {List.t(), List.t(), Map.t(), integer}
  def extract(outputs) do
    {tokens, address, token_value, value} =
      Enum.reduce(outputs, {[], [], %{}, 0}, fn x, {t, a, tv, v} ->
        value = Map.get(tv, x.tid, 0) + x.value
        token_value = Map.put(tv, x.tid, value)

        {t ++ [x.tid], a ++ [x.address], token_value, v + x.value}
      end)

    {tokens |> Enum.uniq() |> Enum.sort(), Enum.uniq(address), token_value, value}
  end

  # def get_all(txid, channel_id) do
  #   txlen = byte_size(txid)

  #   from(txo in Txo,
  #     where: fragment("substring(?::bytea from 1 for ?) = ?", txo.id, ^txlen, ^txid)
  #   )
  #   |> Repo.all(prefix: channel_id)
  # end

  # @spec valid?(t | [t]) :: :ok | {:error, atom()}
  # def valid?([]), do: true

  # def valid?([output | rest]), do: if(valid?(output), do: valid?(rest), else: false)

  # def valid?(output) do
  #   with :ok <- valid_amount?(output),
  #        :ok <- valid_index?(output) do
  #     :ok
  #   else
  #     err -> err
  #   end
  # end

  @spec valid_amount?(Txo) :: :ok | {:error, atom()}
  def valid_amount?(output) do
    if output.value > 0, do: :ok, else: {:error, :invalid_output_value}
  end

  @spec valid_index?(Txo) :: :ok | {:error, atom()}
  def valid_index?(output) do
    if output.index > 0, do: :ok, else: {:error, :invalid_output_index}
  end

  def from_request([]), do: []

  def from_request([o | rest]) do
    [
      from_request(o)
    ] ++ from_request(rest)
  end

  def from_request(%{"address" => address, "tid" => token, "value" => value}) when value > 0 do
    %Txo{
      address: Base58Check.decode(address),
      tid: token,
      value: value,
      avail: false
    }
  end

  def from_request(_) do
    throw(40207)
  end

  @spec from_request_coinbase!([t]) :: {List.t(), List.t(), List.t(), pos_integer()}
  def from_request_coinbase!(outputs) do
    {outputs, tokens, address, total} =
      Enum.reduce(outputs, {[], [], [], 0}, fn %{
                                                 "address" => address,
                                                 "tid" => tid,
                                                 "value" => value
                                               },
                                               {o, t, a, v} ->
        addr = Base58Check.decode(address)

        if value <= 0 do
          throw(40207)
        end

        case Address.internal_address?(addr) do
          true ->
            output = %Txo{
              address: addr,
              tid: tid,
              value: value
              # avail: true
            }

            {o ++ [output], t ++ [tid], a ++ [address], v + value}

          false ->
            throw(40216)
        end
      end)

    {outputs, Enum.uniq(tokens), Enum.uniq(address), total}
  end

  @spec from_request_extract([t]) :: {List.t(), List.t(), List.t(), pos_integer()}
  def from_request_extract(outputs) do
    {outputs, tokens, address, total} =
      Enum.reduce(outputs, {[], [], [], 0}, fn %{
                                                 "address" => address,
                                                 "tid" => tid,
                                                 "value" => value
                                               } = x,
                                               {o, t, a, v} ->
        {o ++ [from_request(x)], t ++ [tid], a ++ [address], v + value}
      end)

    {outputs, Enum.uniq(tokens), Enum.uniq(address), total}
  end

  @spec generate_index(binary, pos_integer()) :: binary()
  def generate_index(tx_index, index) do
    [tx_index, <<index::24>>] |> IO.iodata_to_binary()
  end

  def calc_size([]), do: 0

  def calc_size([o | rest]) do
    calc_size(o) + calc_size(rest)
  end

  def calc_size(o) do
    byte_size(o.id) + byte_size(o.tid) + 8 + byte_size(o.address)
  end

  def calc_size(txos, output_index_size) do
    Enum.reduce(txos, 0, fn o, acc ->
      output_index_size + byte_size(o.tid) + 8 + byte_size(o.address) + acc
    end)
  end

  def update_avail(txid, channel_id) do
    from(txo in Txo,
      where: fragment("substring(?::bytea from 1 for ?)", txo.id, ^byte_size(txid)) == ^txid,
      update: [set: [avail: true]]
    )
    |> Repo.update_all([], prefix: channel_id)
  end

  def all(params) do
    from(Txo)
    |> where([o], o.avail == true)
    |> filter_index(params)
    |> filter_address(params)
    |> filter_token(params)
    |> filter_offset(params)
    |> filter_limit(params, 50, 100)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> transform()
  end

  defp filter_index(query, %{"hash" => hash16}) do
    hash = Base.decode16!(hash16, case: :mixed)

    sub = from(tx in Tx, where: tx.hash == ^hash, select: tx.index)

    where(
      query,
      [txo],
      fragment("substring(?::bytea from 1 for ?)", txo.id, fragment("length(?)", subquery(sub))) ==
        subquery(sub)
    )
  end

  defp filter_index(query, %{"txid" => txid}) do
    txid = Base62.decode(txid)

    where(
      query,
      [txo],
      fragment("substring(?::bytea from 1 for ?)", txo.id, ^byte_size(txid)) == ^txid
    )
  end

  defp filter_index(query, _params), do: query

  defp filter_address(query, %{"address" => address}) do
    bin_address = Base58Check.decode(address)
    where(query, [txo], txo.address == ^bin_address)
  end

  defp filter_address(query, _params), do: query

  defp filter_token(query, %{"token" => token}) do
    where(query, [txo], txo.tid == ^token)
  end

  defp filter_token(query, _params), do: query

  defp filter_select(query, %{"show" => "token_meta"}) do
    query
    |> join(:inner, [o], tk in Token, on: tk.id == o.tid)
    |> select([o, tk], %{
      id: o.id,
      token: o.tid,
      value: o.value,
      address: o.address,
      meta: tk.meta
    })
  end

  defp filter_select(query, _params) do
    select(query, [o], %{
      id: o.id,
      token: o.tid,
      value: o.value,
      address: o.address
    })
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "newest" ->
        order_by(query, [txo], desc: txo.id)

      _ ->
        order_by(query, [txo], asc: txo.id)
    end
  end

  defp transform(txos) do
    Enum.map(txos, fn x ->
      %{x | id: Base62.encode(x.id), address: Base58Check.encode(x.address)}
    end)
  end
end
