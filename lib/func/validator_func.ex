defmodule Ippan.Func.Validator do
  import Guards
  alias Ippan.Validator
  require SqliteStore
  require BalanceStore

  @type result :: Ippan.Request.result()
  @token Application.compile_env(:ipnworker, :token)
  @max_validators Application.compile_env(:ipnworker, :max_validators)

  def new(
        %{id: account_id, conn: conn, stmts: stmts, dets: dets},
        hostname,
        port,
        owner_id,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        opts \\ %{}
      )
      when byte_size(name) <= 20 and between_size(hostname, 4, 50) and fee_type in 0..2 and
             fee > 0 and is_float(fee) and check_port(port) do
    map_filter = Map.take(opts, Validator.optionals())
    pubkey = Fast64.decode64(pubkey)
    net_pubkey = Fast64.decode64(net_pubkey)
    next_id = SqliteStore.one(conn, stmts, "next_id_validator")

    cond do
      fee_type == 0 and fee < 1 ->
        raise IppanError, "Invalid fee config"

      fee_type == 2 and fee < 1 ->
        raise IppanError, "Invalid fee config"

      byte_size(net_pubkey) > 1278 ->
        raise IppanError, "Invalid net_pubkey size #{byte_size(net_pubkey)}"

      byte_size(pubkey) > 897 ->
        raise IppanError, "Invalid pubkey size"

      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      not Match.hostname?(hostname) ->
        raise IppanError, "Invalid hostname"

      SqliteStore.exists?(conn, stmts, "exists_host_validator", hostname) ->
        raise IppanError, "Validator already exists"

      @max_validators <= next_id ->
        raise IppanError, "Maximum validators exceeded"

      true ->
        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_url(:avatar)

        price = Validator.calc_price(next_id)
        balance_key = BalanceStore.gen_key(account_id, @token)

        case BalanceStore.has_balance?(dets, balance_key, price) do
          false ->
            raise IppanError, "Insufficient balance"

          true ->
            :ok
        end
    end
  end

  def update(%{id: account_id, conn: conn, stmts: stmts}, id, opts \\ %{}) do
    map_filter = Map.take(opts, Validator.editable())

    cond do
      opts == %{} ->
        raise IppanError, "Options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not SqliteStore.exists?(conn, stmts, "owner_validator", [id, account_id]) ->
        raise IppanError, "Invalid owner"

      true ->
        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_hostname(:hostname)
        |> MapUtil.validate_length_range(:name, 1..20)
        |> MapUtil.validate_url(:url)
        |> MapUtil.validate_value(:fee, :gt, 0)
        |> MapUtil.validate_range(:fee_type, 0..2)
        |> MapUtil.transform(:pubkey, fn x ->
          case Fast64.decode64(x) do
            j when byte_size(j) > 897 ->
              raise IppanError, "Invalid pubkey"

            j ->
              j
          end
        end)
        |> MapUtil.transform(:net_pubkey, fn x ->
          case Fast64.decode64(x) do
            j when byte_size(j) > 897 ->
              raise IppanError, "Invalid net_pubkey"

            j ->
              j
          end
        end)
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, id) do
    if SqliteStore.exists?(conn, stmts, "owner_validator", [id, account_id]) do
      :ok
    else
      raise IppanError, "Invalid owner"
    end
  end
end
