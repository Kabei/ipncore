defmodule Ipncore.DNS do
  @moduledoc """
  DNS.Server
  """
  # @behaviour Ipncore.DNS.Server
  # use Ipncore.DNS.UdpServer
  alias Ipncore.DnsRecord
  require Logger

  def handle(record, _cl) do
    Logger.info(fn -> "#{inspect(record)}" end)
    query = hd(record.qdlist)
    # IO.inspect(query)

    case DnsRecord.lookup(query.domain, query.type) do
      nil ->
        %{record | header: %{record.header | qr: true, rcode: 3}}

      {values, ttl} when is_list(values) ->
        resources =
          Enum.map(values, fn value ->
            %DNS.Resource{
              domain: query.domain,
              class: query.class,
              type: query.type,
              ttl: ttl,
              data: value
            }
          end)

        %{record | anlist: resources, header: %{record.header | qr: true}}

      {value, ttl} ->
        resources = [
          %DNS.Resource{
            domain: query.domain,
            class: query.class,
            type: query.type,
            ttl: ttl,
            data: value
          }
        ]

        %{record | anlist: resources, header: %{record.header | qr: true}}
    end
  end
end
