defmodule Ipncore.DNS do
  @moduledoc """
  DNS.Server
  """
  # @behaviour Ipncore.DNS.Server
  # use Ipncore.DNS.UdpServer
  alias Ipncore.DnsRecord
  require Logger

  @regex ~r/^(cmm|npo|ntw|cyber|ipn|wlt|iwl|ippan|btc|cyb|fin|geo|and|gold|god|lux|yes|bbb|i|u|btw|nws|diy|iot|69|opasy)$/

  defmacro nx_domain(domain_list, type) do
    quote do
      %{:dnsmsg.new(%{}, {unquote(domain_list), unquote(type), :in}) | Return_code: 3}
    end
  end

  defmacro not_implemented(domain_list, type) do
    quote do
      %{:dnsmsg.new(%{}, {unquote(domain_list), unquote(type), :in}) | Return_code: 4}
    end
  end

  defmacrop anwser_from_remote(request, domain, type, dns_rr) do
    quote do
      Enum.map(unquote(dns_rr), fn {_dns_rr, _domain, _type, _in, _cnt, ttl, value, _tm, _bm,
                                    _func} ->
        rvalue = answer(value)
        answer = :dnslib.resource('#{unquote(domain)} IN #{ttl} #{unquote(type)} #{rvalue}')

        :dnsmsg.add_response_answer(unquote(request), answer)
        |> :dnsmsg.response()
      end)
    end
  end

  def handle(data, _cl) do
    Logger.info(fn -> "#{inspect(data)}" end)
    {:ok, request, _} = :dnswire.from_binary(data)
    question = {domain_list, _type, _} = request[:Questions] |> hd()

    case Regex.match(@regex, List.last(domain_list)) do
      true ->
        local_resolve(request, question)

      false ->
        proxy_resolve(request, question)
    end
  end

  defp local_resolve(request, {domain_list, type, _}) do
    domain = Enum.join(domain_list, ".")

    {:ok, bin} =
      case DnsRecord.lookup(domain, type) do
        nil ->
          nx_domain(domain_list, type)

        {values, ttl} when is_list(values) ->
          Enum.reduce(values, request, fn x, acc ->
            rvalue = answer_response(type, x)
            answer = :dnslib.resource('#{domain} IN #{ttl} #{type} #{rvalue}')

            :dnsmsg.add_response_answer(acc, answer)
          end)
          |> :dnsmsg.response()

        {value, ttl} ->
          rvalue = answer(value)
          answer = :dnslib.resource('#{domain} IN #{ttl} #{type} #{rvalue}')

          :dnsmsg.add_response_answer(request, answer)
          |> :dnsmsg.response()
      end
      |> :dnswire.to_binary()

    bin
  end

  defp proxy_resolve(record, {domain_list, type, _}) do
    try do
      domain = Enum.join(domain_list, ".") |> to_charlist()
      tnumber = type_to_number(type)

      nameservers =
        Application.get_env(:ipncore, :dns_nameservers, [{"1.1.1.1", 53}, {"8.8.8.8", 53}])

      timeout = Application.get_env(:ipncore, :dns_resolve_timeout, 5_000)

      case :inet_res.resolve(domain, :in, tnumber, [nameservers: nameservers], timeout) do
        {:ok, {:dnsrec, _header, _query, _anlist, dns_rr, _arlist}} ->
          anwser_from_remote(request, domain, type, dns_rr)

        {:error, {:noquery, {:dnsrec, _header, _query, dns_rr, _nslist, _arlist}}} ->
          anwser_from_remote(request, domain, type, dns_rr)

        {:error, :timeout} ->
          nx_domain(domain_list, type)

        {:error, :nxdomain} ->
          nx_domain(domain_list, type)

        {:error, _} ->
          nx_domain(domain_list, type)
      end
    rescue
      FunctionClauseError ->
        not_implemented(domain_list, type)
    end
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
  defp type_to_number(:ptr), do: 12
  defp type_to_number(:mx), do: 15
  defp type_to_number(:txt), do: 16
  defp type_to_number(:aaaa), do: 28
  defp type_to_number(:srv), do: 33
  defp type_to_number(:ds), do: 43
  defp type_to_number(:sshfp), do: 44
  defp type_to_number(:rrsig), do: 46
  defp type_to_number(:nsec), do: 47
  defp type_to_number(:dnskey), do: 48
  defp type_to_number(:uri), do: 256
  defp type_to_number(:caa), do: 257
end
