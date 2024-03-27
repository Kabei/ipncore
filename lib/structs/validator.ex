defmodule Ippan.Validator do
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
          updated_at: non_neg_integer()
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
    class: ""
  ]

  @suffix "V-"
  @pad_number 6

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
      x.updated_at
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
        updated_at
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
      updated_at: updated_at
    }
  end

  @impl true
  def to_map({_id, x}) do
    x
  end

  def to_text(x = %{pubkey: pk, net_pubkey: npk}) do
    %{x | pubkey: Utils.encode64(pk), net_pubkey: Utils.encode64(npk)}
  end

  def calc_price(total), do: (total + 1) * EnvStore.validator_price()

  require Sqlite

  defmacro insert(args) do
    quote location: :keep do
      Sqlite.step("insert_validator", unquote(args))
    end
  end

  defmacro get(id) do
    quote location: :keep do
      Sqlite.get(:validator, "get_validator", unquote(id), Ippan.Validator)
    end
  end

  def get_host(hostname) do
    match = [{{:"$1", %{hostname: hostname}}, [], [:"$_"]}]

    case :ets.select(:validator, match) do
      [] ->
        db_ref = :persistent_term.get(:main_conn)

        case Sqlite.fetch("get_host_validator", [hostname]) do
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

    [@suffix, String.pad_leading("#{n}", @pad_number, "0")] |> IO.iodata_to_binary()
  end

  defmacro exists?(id) do
    quote location: :keep do
      Sqlite.exists?("exists_validator", [unquote(id)])
    end
  end

  defmacro active?(id) do
    quote location: :keep do
      Sqlite.exists?("exists_active_validator", [unquote(id)])
    end
  end

  defmacro exists_host?(hostname) do
    quote location: :keep do
      Sqlite.exists?("exists_host_validator", [unquote(hostname)])
    end
  end

  defmacro owner?(id, owner) do
    quote bind_quoted: [id: id, owner: owner], location: :keep do
      Sqlite.exists?("owner_validator", [id, owner])
    end
  end

  defmacro total do
    quote location: :keep do
      Sqlite.one("total_validators", [], 0)
    end
  end

  defmacro update(map, id) do
    quote bind_quoted: [map: map, id: id], location: :keep do
      :ets.delete(:validator, id)
      Sqlite.update("assets.validator", map, id: id)
    end
  end

  defmacro enable(id, round_id) do
    quote bind_quoted: [id: id, round: round_id], location: :keep do
      :ets.delete(:validator, id)
      Sqlite.update("assets.validator", %{"active" => false, "updated_at" => round}, id: id)
    end
  end

  defmacro disable(id, round_id) do
    quote bind_quoted: [id: id, round: round_id], location: :keep do
      :ets.delete(:validator, id)
      Sqlite.update("assets.validator", %{"active" => false, "updated_at" => round}, id: id)
    end
  end

  defmacro incr_failure(id, value, round_id) do
    quote bind_quoted: [id: id, value: value, round: round_id], location: :keep do
      case Ippan.Validator.get(id) do
        nil ->
          nil

        validator ->
          :ets.delete(:validator, id)
          Sqlite.step("inc_fail_validator", [id, value, round])
          validator.failures + value
      end
    end
  end

  defmacro delete(id) do
    quote bind_quoted: [id: id], location: :keep do
      if id == :persistent_term.get(:vid) do
        Logger.warning("Delete validator #{id}")
        # System.halt()
      end

      :ets.delete(:validator, id)
      Sqlite.step("delete_validator", [id])
    end
  end
end
