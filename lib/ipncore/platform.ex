defmodule Platform do
  @token Application.compile_env(:ipncore, :token)

  def start do
    validator_id = Application.get_env(:ipncore, :validator)

    case TokenStore.lookup(@token) do
      nil ->
        GlobalConst.new(Global, %{
          validator: validator_id
        })

      token ->
        account_owner = token[:owner]
        account = AccountStore.lookup(account_owner)

        GlobalConst.new(Global, %{
          owner: token[:owner],
          owner_pubkey: account[:pubkey],
          validator: validator_id
        })
    end
  end

  def has_owner? do
    case Global.get(:owner, false) do
      false ->
        false

      _ ->
        true
    end
  end

  def owner?(nil), do: false

  def owner?(id) do
    Global.get(:owner, nil) == id
  end
end
