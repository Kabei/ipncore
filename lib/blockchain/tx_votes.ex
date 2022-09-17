defmodule Ipncore.TxVote do
  use Ecto.Schema
  alias Ipncore.Repo
  alias Ipnutils.Address
  alias __MODULE__

  @type t :: %__MODULE__{
          id: binary(),
          address: binary,
          sign: binary,
          time: pos_integer,
          vote: boolean
        }

  # @channel Application.get_env(:ipncore, :channel)

  @primary_key {:id, :binary, []}
  schema "tx_votes" do
    field(:index, :binary)
    field(:address, :binary)
    field(:sign, :binary)
    field(:time, :integer)
    field(:vote, :boolean)
  end

  def build(votes) do
    tx_votes =
      for vote <- votes do
        new(vote)
      end

    try do
      {:ok, _} = Repo.insert_all(tx_votes, prefix: Default.channel(), returning: false)

      {approved, cancelled} =
        Enum.reduce(votes, {0, 0}, fn x, {acc_p, acc_c} ->
          if x.vote == true do
            {acc_p + 1, acc_c}
          else
            {acc_p, acc_c + 1}
          end
        end)

      cond do
        approved > cancelled ->
          :ok

        approved < cancelled ->
          :error

        true ->
          :error
      end
    rescue
      [MatchError, Postgrex.Error, DBConnection.ConnectionError] ->
        :error
    end
  end

  def new_approved(index, address, pubkey, privkey) do
    time = :erlang.system_time(:millisecond)
    hash = hash(index, address, time, "YES")
    signature = Falcon.sign(privkey, hash)

    %{
      "address" => address,
      "pubkey" => pubkey,
      "index" => index,
      "sign" => signature,
      "time" => time
    }
  end

  def new_cancelled(index, address, pubkey, privkey) do
    time = :erlang.system_time(:millisecond)
    hash = hash(index, address, time, "NO")
    signature = Falcon.sign(privkey, hash)

    %{
      "address" => address,
      "pubkey" => pubkey,
      "index" => index,
      "sign" => signature,
      "time" => time
    }
  end

  @doc """
  address param from to session records
  """
  def new(%{
        "address" => address,
        "index" => index,
        "pubkey" => pubkey,
        "sign" => sign,
        "time" => time,
        "vote" => result
      })
      when result in ["YES", "NO"] do
    unless Address.check_address_pubkey(address, pubkey), do: throw(40210)

    hash = hash(index, address, time, result)

    vote = if result == "YES", do: true, else: false

    case Falcon.verify(hash, sign, pubkey) do
      :ok ->
        %TxVote{
          id: generate_id(index, hash),
          index: index,
          sign: sign,
          time: time,
          vote: vote
        }

      :error ->
        throw(40228)
    end
  end

  def new(_), do: throw(40227)

  defp hash(index, address, time, result) do
    [
      index,
      address,
      time,
      result
    ]
    |> IO.iodata_to_binary()
    |> Crypto.hash(:sha256)
  end

  def generate_id(index, hash) do
    index <> :binary.part(hash, 6)
  end
end
