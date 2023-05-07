defmodule Owner do
  use GlobalConst.DummyModule
end

defmodule Platform do
  alias Ippan.Account
  alias Ippan.{Address, Token}

  @token Default.token()

  def start do
    Default.token()
    |> TokenStore.lookup()
    |> put()
  end

  @spec put(Token.t()) :: boolean()
  def put(%{id: @token, owner: id}) do
    account_id = Owner.get(:id, nil)

    cond do
      account_id == nil or account_id != id ->
        account =
          AccountStore.lookup(id)
          |> Account.to_map()

        GlobalConst.new(Owner, %{
          id: account.id,
          address: account.address,
          address58: Address.to_text(account.address),
          pubkey: account.pubkey
        })

        true

      true ->
        false
    end
  end

  def put(_), do: false

  def has_owner? do
    case Owner.get(:id, false) do
      false ->
        false

      _ ->
        true
    end
  end

  def id, do: Owner.get(:id, nil)
  def pubkey, do: Owner.get(:pubkey, nil)
  def address, do: Owner.get(:address, nil)
  def address58, do: Owner.get(:address58, nil)

  def owner?(nil), do: false

  def owner?(id) do
    Owner.get(:id, nil) == id
  end
end
