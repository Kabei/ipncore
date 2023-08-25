defmodule Ippan.Command do
  alias Ippan.Events

  # @libsecp256k1 ExSecp256k1.Impl
  # @json Application.compile_env(:ipncore, :json)

  def handle!(conn, stmts, dets, hash, type, timestamp, account_id, validator, size, args, round) do
    case Events.lookup(type) do
      %{deferred: false, mod: module, fun: fun} ->
        source = %{
          conn: conn,
          stmts: stmts,
          dets: dets,
          id: account_id,
          type: type,
          validator: validator,
          hash: hash,
          timestamp: timestamp,
          size: size
        }

        apply(module, fun, [source | args])

      # deferred transactions
      _deferred ->
        key = normalize_key(args, account_id)
        insert_deferred({{key, type}, hash, timestamp, args, validator.id, round})
    end
  end

  # only deferred transactions
  def handle_post!(conn, stmts, dets, hash, type, timestamp, account_id, validator_id, size, args) do
    %{mod: module, fun: fun} = Events.lookup(type)

    source = %{
      id: account_id,
      conn: conn,
      stmts: stmts,
      dets: dets,
      type: type,
      validator: validator_id,
      hash: hash,
      timestamp: timestamp,
      size: size
    }

    apply(module, fun, [source | args])
  end

  defp insert_deferred(
         {{_key, _type} = msg_key, hash, timestamp, _args, _valdiator_id, _round} = msg
       ) do
    case :ets.lookup(:dtx, msg_key) do
      [] ->
        :ets.insert(:dtx, msg)

      [{_msg_key, xhash, xtimestamp, _, _}] ->
        if timestamp < xtimestamp or (timestamp == xtimestamp and hash < xhash) do
          :ets.insert(:dtx, msg)
        else
          :error
        end
    end
  end

  defp normalize_key(nil, from), do: from
  defp normalize_key([], from), do: from
  defp normalize_key(x, _from), do: hd(x) |> to_string()
end
