defmodule Ippan.Domain do
  @type t :: %__MODULE__{
          name: String.t(),
          owner: binary(),
          email: String.t(),
          avatar: String.t(),
          records: non_neg_integer(),
          enabled: boolean(),
          created_at: non_neg_integer(),
          renewed_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  use Ippan.Struct

  def optionals, do: ~w(email avatar)

  def editable, do: ~w(owner email avatar)

  @doc "Return subdomain and domain in a tuple from hostname or list hostname"
  def split(hostname_parts) when is_list(hostname_parts) do
    domain = Enum.take(hostname_parts, -2)
    subdomain = hostname_parts -- domain

    {Enum.join(subdomain, "."), Enum.join(domain, ".")}
  end

  def split(hostname) do
    parts = String.split(hostname, ".")

    domain = Enum.take(parts, -2)
    subdomain = parts -- domain

    {Enum.join(subdomain, "."), Enum.join(domain, ".")}
  end

  def price(name, years) do
    x =
      name
      |> String.split(".")
      |> List.first()
      |> String.length()

    base =
      cond do
        x <= 5 ->
          100_000

        x <= 8 ->
          75_000

        true ->
          5_000
      end

    base + (years - 1) * 5_000
  end

  def priceRenew(years) do
    years * 5_000
  end

  defstruct name: nil,
            owner: nil,
            email: nil,
            avatar: nil,
            records: 0,
            enabled: true,
            created_at: nil,
            renewed_at: nil,
            updated_at: nil

  def to_tuple(x) do
    {x.name, x.owner, x.email, x.avatar, x.records, x.enabled, x.created_at, x.renewed_at,
     x.updated_at}
  end

  def to_list(x) do
    [
      x.name,
      x.owner,
      x.email,
      x.avatar,
      x.records,
      x.enabled,
      x.created_at,
      x.renewed_at,
      x.updated_at
    ]
  end

  def to_map({name, owner, email, avatar, records, enabled, created_at, renewed_at, updated_at}) do
    %{
      name: name,
      owner: owner,
      email: email,
      avatar: avatar,
      records: records,
      enabled: enabled,
      created_at: created_at,
      renewed_at: renewed_at,
      updated_at: updated_at
    }
  end

  def to_map([name, owner, email, avatar, records, enabled, created_at, renewed_at, updated_at]) do
    %{
      name: name,
      owner: owner,
      email: email,
      avatar: avatar,
      records: records,
      enabled: enabled,
      created_at: created_at,
      renewed_at: renewed_at,
      updated_at: updated_at
    }
  end
end
