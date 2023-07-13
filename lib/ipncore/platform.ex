defmodule Platform do
  @token Application.compile_env(:ipncore, :token)

  def start do
    validator_id = Default.validator_id()

    # load native token data
    case TokenStore.lookup([@token]) do
      nil ->
        GlobalConst.new(Global, %{
          validator: validator_id
        })

        init()

      token ->
        wallet_owner = token.owner
        [_, wallet_pubkey, _wallet_validator] = WalletStore.lookup([wallet_owner])

        GlobalConst.new(Global, %{
          owner: wallet_owner,
          owner_pubkey: wallet_pubkey,
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

  defp init do
    sk =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    seed =
      <<114, 126, 255, 205, 14, 72, 7, 127, 21, 47, 45, 57, 188, 66, 144, 114, 118, 204, 255, 86,
        236, 6, 168, 77, 247, 60, 145, 142, 137, 32, 81, 188, 167, 95, 239, 138, 212, 128, 12,
        211, 239, 154, 118, 40, 154, 90, 156, 28>>

    {pk, sk, address} = Test.gen_ed25519(sk)
    {pkv, _skv, _addressv} = Test.gen_falcon(seed)

    Test.wallet_new(pk, 0) |> Test.run()

    Test.wallet_new(pkv, 1) |> Test.run()

    BlockTimer.mine()
    BlockTimer.round_end()

    Test.token_new(sk, address, "IPN", address, "IPPAN", 9, "Þ", %{
      "avatar" => "https://avatar.com",
      "props" => ["coinbase", "lock", "burn"]
    })
    |> Test.run()

    BlockTimer.mine()
    BlockTimer.round_end()

    Test.validator_new(sk, address, 0, address, "ippan.org", "Speedy", pk, pkv, 1, 0.01)
    |> Test.run()

    Test.validator_new(sk, address, 1, address, "ippan.co.uk", "Raptor", pk, pkv, 1, 0.01)
    |> Test.run()

    BlockTimer.mine()
    BlockTimer.round_end()
  end
end
