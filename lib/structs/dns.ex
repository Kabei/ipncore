defmodule Ippan.DNS do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          domain: String.t(),
          hash: binary(),
          name: String.t(),
          type: non_neg_integer(),
          data: String.t(),
          ttl: non_neg_integer()
        }

  defstruct domain: nil, hash: nil, name: nil, type: nil, data: nil, ttl: 3600

  @impl true
  def editable, do: ~w(data ttl)

  @impl true
  def to_list(x) do
    [
      x.domain,
      x.name,
      x.type,
      x.data,
      x.ttl,
      x.hash
    ]
  end

  @impl true
  def to_tuple(x) do
    {{x.domain, x.hash}, x}
  end

  @impl true
  def to_map({_domain_hash, map}), do: map

  @impl true
  def list_to_tuple(x) do
    map = list_to_map(x)
    {{map.domain, map.hash}, map}
  end

  @impl true
  def list_to_map([
        domain,
        name,
        type,
        data,
        ttl,
        hash
      ]) do
    %{
      domain: domain,
      name: name,
      type: type,
      data: data,
      ttl: ttl,
      hash: hash
    }
  end

  def fun_hash(data) do
    data |> IO.iodata_to_binary() |> Blake3.hash() |> :binary.part(16, 16)
  end

  defmacro insert(args) do
    quote location: :keep do
      Sqlite.step("insert_dns", unquote(args))
    end
  end

  defmacro get(domain, hash) do
    quote location: :keep do
      Sqlite.fetch("get_dns", [unquote(domain), unquote(hash)])
    end
  end

  defmacro exists?(domain, hash) do
    quote bind_quoted: [domain: domain, hash: hash], location: :keep do
      Sqlite.exists?("exists_dns", [domain, hash])
    end
  end

  defmacro update(map, domain, hash) do
    quote location: :keep do
      Sqlite.update("dns.dns", unquote(map), domain: unquote(domain), hash: unquote(hash))
    end
  end

  defmacro delete(domain) do
    quote bind_quoted: [domain: domain], location: :keep do
      Sqlite.step("delete_dns", [domain])
    end
  end

  defmacro delete(domain, name) do
    quote bind_quoted: [domain: domain, name: name], location: :keep do
      Sqlite.step("delete_name_dns", [domain, name])
    end
  end

  defmacro delete_hash(domain, name, hash) do
    quote bind_quoted: [domain: domain, name: name, hash: hash], location: :keep do
      Sqlite.step("delete_hash_dns", [domain, name, hash])
    end
  end

  defmacro delete_type(domain, name, type) do
    quote bind_quoted: [domain: domain, name: name, type: type], location: :keep do
      Sqlite.step("delete_type_dns", [domain, name, type])
    end
  end

  def type_to_number("A"), do: 1
  def type_to_number("NS"), do: 2
  def type_to_number("SOA"), do: 6
  def type_to_number("MX"), do: 15
  def type_to_number("TXT"), do: 16
  def type_to_number("AAAA"), do: 28
  def type_to_number("a"), do: 1
  def type_to_number("ns"), do: 2
  def type_to_number("soa"), do: 6
  def type_to_number("mx"), do: 15
  def type_to_number("txt"), do: 16
  def type_to_number("aaaa"), do: 28
  def type_to_number(1), do: 1
  def type_to_number(2), do: 2
  def type_to_number(6), do: 6
  def type_to_number(15), do: 15
  def type_to_number(16), do: 16
  def type_to_number(28), do: 28
  def type_to_number(x), do: raise(IppanError, "DNS type not support #{inspect(x)}")

  def type_to_alpha(1), do: "A"
  def type_to_alpha(2), do: "NS"
  def type_to_alpha(6), do: "SOA"
  def type_to_alpha(15), do: "MX"
  def type_to_alpha(16), do: "TXT"
  def type_to_alpha(28), do: "AAAA"
  def type_to_alpha("A"), do: "A"
  def type_to_alpha("NS"), do: "NS"
  def type_to_alpha("SOA"), do: "SOA"
  def type_to_alpha("MX"), do: "MX"
  def type_to_alpha("TXT"), do: "TXT"
  def type_to_alpha("AAAA"), do: "AAAA"
  def type_to_alpha("a"), do: "A"
  def type_to_alpha("ns"), do: "NS"
  def type_to_alpha("soa"), do: "SOA"
  def type_to_alpha("mx"), do: "MX"
  def type_to_alpha("txt"), do: "TXT"
  def type_to_alpha("aaaa"), do: "AAAA"
  def type_to_alpha(x), do: raise(IppanError, "DNS type not support #{inspect(x)}")
end
