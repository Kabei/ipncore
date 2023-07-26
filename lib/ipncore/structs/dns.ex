defmodule Ippan.DNS do
  @type t :: %__MODULE__{
          hash: binary(),
          domain: String.t(),
          name: String.t(),
          type: non_neg_integer(),
          data: String.t(),
          ttl: non_neg_integer()
        }

  use Ippan.Struct

  defstruct domain: nil, name: nil, type: nil, data: nil, ttl: 3600, hash: nil

  def editable, do: ~w(data ttl)

  def default_hash(domain, data) do
    :crypto.hash(:md5, "#{domain}#{data}")
  end

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

  def to_tuple(x) do
    {
      {x.domain, x.hash},
      x.name,
      x.type,
      x.data,
      x.ttl
    }
  end

  def to_map({{domain, hash}, name, type, data, ttl}) do
    %{
      domain: domain,
      name: name,
      type: type,
      data: data,
      ttl: ttl,
      hash: hash
    }
  end

  def to_map([
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
    # :crypto.hash(:md5, data)
    data |> IO.iodata_to_binary() |> Blake3.hash() |> :binary.part(16, 16)
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
  def type_to_number(x), do: raise(IppanError, "DNS type not support #{inspect(x)}")

  def type_to_alpha(1), do: "A"
  def type_to_alpha(2), do: "NS"
  def type_to_alpha(6), do: "SOA"
  def type_to_alpha(15), do: "MX"
  def type_to_alpha(16), do: "TXT"
  def type_to_alpha(28), do: "AAAA"
  def type_to_alpha(x), do: raise(IppanError, "DNS type not support #{inspect(x)}")
end
