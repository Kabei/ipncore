defmodule Platform do
  import Ippan.Utils, only: [to_atom: 1]
  alias Ippan.{Env, Token, Wallet, Validator}

  @token Application.compile_env(:ipncore, :token)

  def start do
    result =
      case TokenStore.lookup_map(@token) do
        nil ->
          load_genesis_file()

        token ->
          %{
            native_token: token,
            net_privkey: Application.get_env(:ipncore, :net_privkey),
            net_pubkey: Application.get_env(:ipncore, :net_pubkey),
            owner: token.owner,
            privkey: Application.get_env(:ipncore, :privkey),
            pubkey: Application.get_env(:ipncore, :pubkey),
            vid: Application.get_env(:ipncore, :vid)
          }
      end

    GlobalConst.new(Default, result)
    result
  end

  defp load_genesis_file do
    {data = %{"tokens" => _, "validators" => _, "wallets" => _}, _binding} =
      Path.join(:code.priv_dir(:ipncore), "genesis.exs")
      |> Code.eval_file()

    for {key, values} <- data do
      case key do
        "wallets" ->
          Enum.each(values, fn x -> WalletStore.insert(Wallet.to_list(x)) end)

        "tokens" ->
          Enum.each(values, fn x -> TokenStore.insert(Token.to_list(x)) end)

        "validators" ->
          Enum.each(values, fn x -> ValidatorStore.insert(Validator.to_list(x)) end)

        "env" ->
          Enum.each(values, fn x -> EnvStore.insert(Env.encode_list(x)) end)
      end
    end

    %{owner: owner} = TokenStore.lookup_map(@token)

    # save all
    TokenStore.sync()
    ValidatorStore.sync()
    WalletStore.sync()
    EnvStore.sync()

    %{
      miner: System.get_env("MINER") |> to_atom(),
      net_privkey: Application.get_env(:ipncore, :net_privkey),
      net_pubkey: Application.get_env(:ipncore, :net_pubkey),
      owner: owner,
      privkey: Application.get_env(:ipncore, :privkey),
      pubkey: Application.get_env(:ipncore, :pubkey),
      vid: Application.get_env(:ipncore, :vid)
    }
  end
end
