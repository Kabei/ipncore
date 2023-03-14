defmodule Ipncore.DNS do
  @moduledoc """
  DNS.Server
  """
  # @behaviour Ipncore.DNS.Server
  # use Ipncore.DNS.UdpServer
  alias Ipncore.{Domain, DnsRecord}
  require Logger

  @regex ~r/^(cmm|npo|ntw|cyber|ipn|wlt|iwl|ippan|btc|cyb|fin|geo|and|gold|god|lux|yes|bbb|i|u|btw|nws|diy|iot|69|opasy|ops|avatar|ultra|more|daddy|bro|sister|fck|tribe|mogul|tequila|gpt|soho|voice|eye|hodl|linux|youxi|we|genius|ciao|ok|dns|cyborg|replicant|air|amigo|bbq|burger|diamond|invest|jewel|pop|rap|rice|rod|soft|tkt|toy|vida|zoom|papi|hola)$/

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

  defmacrop nx_domain(query_id, domain_list, type) do
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

  defmacrop not_implemented(query_id, domain_list, type) do
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
        acc ++ [:dnslib.resource('#{domain} IN 3600 #{type} #{rvalue}')]
      end)
    end
  end

  def child_spec do
    :poolboy.child_spec(:worker,
      name: {:local, :dns_worker},
      worker_module: Ipncore.DNS.Worker,
      size: 20,
      max_overflow: 0
    )
  end

  def handle(data, _client) do
    {:ok, %{ID: query_id, Questions: questions}, _} = :dnswire.from_binary(data)
    question = {domain_list, type, _} = questions |> hd()
    # Logger.info(inspect(domain_list))

    response =
      try do
        case Regex.match?(@regex, List.last(domain_list)) do
          true ->
            local_resolve!(query_id, question)

          false ->
            remote_resolve!(query_id, question)
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
    {subdomain, domain} = Domain.split(domain_list)

    Logger.debug("DNS-Query | #{domain} #{type}")
    type_number = DnsRecord.type_atom_to_number(type)

    resources =
      case DnsRecord.lookup(domain, subdomain, type_number) do
        [] ->
          throw(:nxdomain)

        records ->
          Enum.reduce(records, [], fn {x, ttl}, acc ->
            acc ++ [:dnslib.resource('#{domain} IN #{ttl} #{type} #{x}')]
          end)
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

  defp remote_resolve!(query_id, {domain_list, type, _} = query) do
    domain = Enum.join(domain_list, ".") |> to_charlist()
    tnumber = DnsRecord.type_atom_to_number(type)
    Logger.debug("DNS-Query | #{domain} #{type}")

    opts = Application.get_env(:ipncore, :dns_resolve, @default_dns_resolve_opts)

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
end
