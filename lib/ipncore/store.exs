# defmodule UDPServer.Store do
#   @table :udp_tls
#   @hash :ripemd160

#   def init(_opts) do
#     :ets.new(@table, [:set, :protected])
#   end

#   def put(conn) do
#     hash = compute_hash(conn)

#     :ets.insert(@table, {hash, conn})
#   end

#   def get(hash) do
#     :ets.select(@table, [{{:"$1", :_}, [{:==, :"$1", hash}], [:"$_"]}])
#     |> List.first()
#   end

#   def delete(hash) do
#     :ets.select_delete(@table, [{{:"$1", :_}, [{:==, :"$1", hash}], [:"$_"]}])
#   end

#   defp compute_hash(%{remote_ip: remote_ip}) do
#     data = :inet.ntoa(remote_ip) |> to_string
#     :crypto.hash(@hash, data)
#   end
# end
