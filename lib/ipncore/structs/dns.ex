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
    Blake3.hash(data) |> :binary.part(16, 16)
  end
end
