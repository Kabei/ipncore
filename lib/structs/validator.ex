defmodule Ippan.Validator do
  alias __MODULE__
  alias Ippan.Utils
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: String.t(),
          hostname: String.t(),
          port: integer(),
          name: String.t(),
          owner: binary(),
          class: String.t(),
          pubkey: binary(),
          net_pubkey: binary(),
          image: String.t() | nil,
          fa: integer(),
          fb: integer(),
          active: boolean(),
          failures: integer(),
          env: map(),
          created_at: non_neg_integer(),
          updated_at: non_neg_integer(),
          subs: non_neg_integer()
        }

  defstruct [
    :id,
    :hostname,
    :port,
    :name,
    :owner,
    :pubkey,
    :net_pubkey,
    :image,
    :fa,
    :fb,
    :created_at,
    :updated_at,
    active: false,
    failures: 0,
    env: %{},
    class: "",
    subs: 0
  ]

  @suffix "V-"

  @impl true
  def editable, do: ~w(hostname port name image fa fb owner pubkey net_pubkey class)
  @impl true
  def optionals, do: ~w(image env)

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.port,
      x.name,
      x.owner,
      x.class,
      x.pubkey,
      x.net_pubkey,
      x.image,
      x.fa,
      x.fb,
      if(x.active == true, do: 1, else: 0),
      x.failures,
      CBOR.encode(x.env),
      x.created_at,
      x.updated_at,
      x.subs
    ]
  end

  @impl true
  def list_to_tuple([id | _] = x) do
    {id, list_to_map(x)}
  end

  @impl true
  def to_tuple(x) do
    {x.id, x}
  end

  @impl true
  def list_to_map([
        id,
        hostname,
        port,
        name,
        owner,
        class,
        pubkey,
        net_pubkey,
        image,
        fa,
        fb,
        active,
        failures,
        env,
        created_at,
        updated_at,
        subs
      ]) do
    %{
      id: id,
      hostname: hostname,
      port: port,
      name: name,
      owner: owner,
      class: class,
      image: image,
      pubkey: pubkey,
      net_pubkey: net_pubkey,
      fb: fb,
      fa: fa,
      active: if(active == 1, do: true, else: false),
      failures: failures,
      env: :erlang.element(1, CBOR.Decoder.decode(env)),
      created_at: created_at,
      updated_at: updated_at,
      subs: subs
    }
  end

  @impl true
  def to_map({_id, x}) do
    x
  end

  def suffix, do: @suffix

  def to_text(x = %{pubkey: pk, net_pubkey: npk}) do
    %{x | pubkey: Utils.encode64(pk), net_pubkey: Utils.encode64(npk)}
  end

  def calc_price(total), do: (total + 1) * EnvStore.validator_price()

  def insert(db_ref, map) do
    Sqlite.step(db_ref, "insert_validator", to_list(map))
  end

  def get(db_ref, id) do
    Sqlite.get(db_ref, :validator, "get_validator", id, Validator)
  end

  def get_host(db_ref, hostname) do
    match = [{{:"$1", %{hostname: hostname}}, [], [:"$_"]}]

    case :ets.select(:validator, match) do
      [] ->
        case Sqlite.fetch(db_ref, "get_host_validator", [hostname]) do
          nil -> nil
          result -> list_to_map(result)
        end

      [{_id, map}] ->
        map
    end
  end

  @spec next_id :: String.t()
  def next_id do
    stats = Stats.new()
    n = Stats.get(stats, "seq_validators", 0)
    Stats.put(stats, "seq_validators", n + 1)

    [@suffix, "#{n}"] |> IO.iodata_to_binary()
  end

  def exists?(db_ref, id) do
    Sqlite.has?(db_ref, :validator, "exists_validator", [id])
  end

  def active?(db_ref, id) do
    Sqlite.exists?(db_ref, "exists_active_validator", [id])
  end

  def exists_host?(db_ref, hostname) do
    Sqlite.exists?(db_ref, "exists_host_validator", [hostname])
  end

  def owner?(db_ref, id, owner) do
    Sqlite.exists?(db_ref, "owner_validator", [id, owner])
  end

  def total(db_ref) do
    Sqlite.one(db_ref, "total_validators", [], 0)
  end

  def update(db_ref, map, id) do
    :ets.delete(:validator, id)
    Sqlite.update(db_ref, "assets.validator", map, id: id)
  end

  def count_sub(db_ref, id, value) do
    :ets.delete(:validator, id)
    Sqlite.step(db_ref, "count_sub_validator", [id, value])
  end

  def enable(db_ref, id, round_id) do
    :ets.delete(:validator, id)

    Sqlite.update(db_ref, "assets.validator", %{"active" => false, "updated_at" => round_id},
      id: id
    )
  end

  def disable(db_ref, id, round_id) do
    :ets.delete(:validator, id)

    Sqlite.update(db_ref, "assets.validator", %{"active" => false, "updated_at" => round_id},
      id: id
    )
  end

  def incr_failure(db_ref, id, value, round_id) do
    case get(db_ref, id) do
      nil ->
        nil

      validator ->
        :ets.delete(:validator, id)
        Sqlite.step(db_ref, "inc_fail_validator", [id, value, round_id])
        validator.failures + value
    end
  end

  def delete(db_ref, id) do
    # if id == :persistent_term.get(:vid) do
    # Logger.warning("Delete validator #{id}")
    # System.halt()
    # end

    :ets.delete(:validator, id)
    Sqlite.step(db_ref, "delete_validator", [id])
  end
end
