defmodule Platform do
  alias Ippan.{Env, Token, Wallet, Validator}
  require SqliteStore

  @token Application.compile_env(:ipncore, :token)

  def start do
    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)

    case SqliteStore.lookup_map(:token, conn, stmts, "get_token", @token, Token) do
      nil ->
        load_genesis_file(conn, stmts)

      token ->
        :persistent_term.put(:owner, token.owner)
    end
  end

  defp load_genesis_file(conn, stmts) do
    {data = %{"tokens" => _, "validators" => _, "wallets" => _}, _binding} =
      Path.join(:code.priv_dir(:ipncore), "genesis.exs")
      |> Code.eval_file()

    for {key, values} <- data do
      case key do
        "wallets" ->
          Enum.each(values, fn x ->
            SqliteStore.step(conn, stmts, "insert_wallet", Wallet.to_list(x))
          end)

        "tokens" ->
          Enum.each(values, fn x ->
            SqliteStore.step(conn, stmts, "insert_token", Token.to_list(x))
          end)

        "validators" ->
          Enum.each(values, fn x ->
            SqliteStore.step(conn, stmts, "insert_validator", Validator.to_list(x))
          end)

        "env" ->
          Enum.each(values, fn x ->
            SqliteStore.step(conn, stmts, "insert_env", Env.to_list(x))
          end)
      end
    end

    %{owner: owner} = SqliteStore.lookup_map(:token, conn, stmts, "get_token", @token, Token)

    # save all
    SqliteStore.sync(conn)

    :persistent_term.put(:owner, owner)
  end
end
