defmodule Ipncore.DNS do
  @moduledoc """
  DNS.Server
  """
  # @behaviour Ipncore.DNS.Server
  # use Ipncore.DNS.UdpServer
  alias Ipncore.DnsRecord
  require Logger

  @regex ~r/^(cmm|npo|ntw|cyber|ipn|wlt|iwl|ippan|btc|cyb|fin|geo|and|gold|god|lux|yes|bbb|i|u|btw|nws|diy|iot|69|opasy)$/

  @default_dns_resolve_opts [
    alt_nameservers: [
      {{8, 8, 8, 8}, 53},
      {{1, 0, 0, 1}, 53}
    ],
    nameservers: [
      {{1, 1, 1, 1}, 53},
      {{8, 8, 4, 4}, 53}
    ],
    timeout: 5_000
  ]

  @needs_authority [:dnskey, :ds, :nsec, :rrsig, :sshfp]

  @dnswire_opts_binary edns: false, domain_compression: false

  defmacro nx_domain(query_id, domain_list, type) do
    quote do
      :dnsmsg.new(
        %{
          id: unquote(query_id),
          is_response: true,
          recursion_available: true,
          recursion_desired: true,
          return_code: :name_error
        },
        {unquote(domain_list), unquote(type), :in}
      )
      |> :dnswire.to_binary(@dnswire_opts_binary)
      |> elem(2)
    end
  end

  defmacro not_implemented(query_id, domain_list, type) do
    quote do
      :dnsmsg.new(
        %{
          id: unquote(query_id),
          is_response: true,
          recursion_available: true,
          recursion_desired: true,
          return_code: :not_implemented
        },
        {unquote(domain_list), unquote(type), :in}
      )
      |> :dnswire.to_binary(@dnswire_opts_binary)
      |> elem(2)
    end
  end

  defmacrop to_resources(dns_rr) do
    quote do
      Enum.reduce(unquote(dns_rr), [], fn {_dns_rr, domain, type, _in, _cnt, ttl, value, _tm, _bm,
                                           _func},
                                          acc ->
        rvalue = answer_response(type, value)
        acc ++ [:dnslib.resource('#{domain} IN #{ttl} #{type} #{rvalue}')]
      end)
    end
  end

  def handle(data, _cl) do
    # Logger.info(data)
    {:ok, %{ID: query_id, Questions: questions}, _} = :dnswire.from_binary(data)
    question = {domain_list, type, _} = questions |> hd()

    response =
      try do
        case Regex.match?(@regex, List.last(domain_list)) do
          true ->
            local_resolve!(query_id, question)

          false ->
            proxy_resolve!(query_id, question)
        end
      rescue
        FunctionClauseError ->
          not_implemented(query_id, domain_list, type)
      catch
        :nxdomain ->
          nx_domain(query_id, domain_list, type)

        :timeout ->
          nil
      end

    # Logger.info("Response | #{response}")

    response
  end

  defp local_resolve!(query_id, {domain_list, type, _} = query) do
    domain = Enum.join(domain_list, ".") |> to_charlist()
    Logger.info("DNS-Query | #{domain} #{type}")

    resources =
      case DnsRecord.lookup(domain, type) do
        nil ->
          throw(:nxdomain)

        {values, ttl} when is_list(values) ->
          Enum.reduce(values, [], fn x, acc ->
            rvalue = answer_response(type, x)
            acc ++ [:dnslib.resource('#{domain} IN #{ttl} #{type} #{rvalue}')]
          end)

        {value, ttl} ->
          rvalue = answer_response(type, value)
          :dnslib.resource('#{domain} IN #{ttl} #{type} #{rvalue}')
      end

    msg =
      case type do
        x when x in @needs_authority ->
          :dnsmsg.new(
            %{
              id: query_id,
              is_response: true,
              recursion_available: true,
              recursion_desired: true
            },
            [query],
            [],
            resources
          )

        _ ->
          :dnsmsg.new(
            %{
              id: query_id,
              is_response: true,
              recursion_available: true,
              recursion_desired: true
            },
            [query],
            resources,
            []
          )
      end

    # IO.inspect(msg)

    {:ok, _, bin} = :dnswire.to_binary(msg, @dnswire_opts_binary)

    bin
  end

  defp proxy_resolve!(query_id, {domain_list, type, _} = query) do
    domain = Enum.join(domain_list, ".") |> to_charlist()
    tnumber = type_to_number(type)
    Logger.info("DNS-Query | #{domain} #{type}")

    opts = Application.get_env(:ipncore, :dns_resolve_opts, @default_dns_resolve_opts)

    resolve = :inet_res.resolve(domain, :in, tnumber, opts)
    # IO.inspect(resolve)

    resources =
      case resolve do
        {:ok, {:dns_rec, _header, _query, _anlist, dns_rr, _arlist}} ->
          to_resources(dns_rr)

        {:error, {:noquery, {:dns_rec, _header, _query, dns_rr, _nslist, _arlist}}} ->
          to_resources(dns_rr)

        {:error, :timeout} ->
          throw(:timeout)

        {:error, :nxdomain} ->
          throw(:nxdomain)

        {:error, _} ->
          throw(:nxdomain)
      end

    msg =
      case type do
        x when x in @needs_authority ->
          :dnsmsg.new(
            %{
              id: query_id,
              is_response: true,
              recursion_available: true,
              recursion_desired: true
            },
            [query],
            [],
            resources
          )

        _ ->
          :dnsmsg.new(
            %{
              id: query_id,
              is_response: true,
              recursion_available: true,
              recursion_desired: true
            },
            [query],
            resources,
            []
          )
      end

    # IO.inspect(msg)

    {:ok, _, bin} = :dnswire.to_binary(msg, @dnswire_opts_binary)

    bin
  end

  defp answer_response(:a, value), do: :inet.ntoa(value)
  defp answer_response(:aaaa, value), do: :inet.ntoa(value)

  defp answer_response(_type, value) when is_tuple(value),
    do: Tuple.to_list(value) |> Enum.join(" ")

  defp answer_response(_type, value), do: value

  defp type_to_number(:a), do: 1
  defp type_to_number(:ns), do: 2
  defp type_to_number(:cname), do: 5
  defp type_to_number(:soa), do: 6
  defp type_to_number(:wks), do: 11
  defp type_to_number(:ptr), do: 12
  defp type_to_number(:hinfo), do: 13
  defp type_to_number(:mx), do: 15
  defp type_to_number(:txt), do: 16
  defp type_to_number(:aaaa), do: 28
  defp type_to_number(:srv), do: 33
  defp type_to_number(:ds), do: 43
  defp type_to_number(:sshfp), do: 44
  defp type_to_number(:rrsig), do: 46
  defp type_to_number(:nsec), do: 47
  defp type_to_number(:dnskey), do: 48
  defp type_to_number(:all), do: 255
  defp type_to_number(:uri), do: 256
  defp type_to_number(:caa), do: 257
end
