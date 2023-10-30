defmodule Platform do
  alias Ippan.{Env, Token, Validator}
  require Token
  require Validator
  require Sqlite

  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)

  def start do
    db_ref = :persistent_term.get(:main_conn)
    vid = :persistent_term.get(:vid)
    EnvStore.load(db_ref)

    case EnvStore.owner() do
      nil ->
        load_genesis_file(db_ref)
        EnvStore.load(db_ref)

      owner ->
        :persistent_term.put(:owner, owner)
    end

    v = Validator.get(vid)
    :persistent_term.put(:validator, v)

    :ok
  end

  defp load_genesis_file(db_ref) do
    {data = %{"tokens" => _, "validators" => _, "wallets" => _}, _binding} =
      Path.join(:code.priv_dir(@app), "genesis.exs")
      |> Code.eval_file()

    wallet_dets = DetsPlux.get(:wallet)
    wallet_tx = DetsPlux.tx(:wallet)

    for {key, values} <- data do
      case key do
        "wallets" ->
          Enum.each(values, fn x ->
            DetsPlux.put(wallet_tx, {x.id, x.pubkey, x.validator})
          end)

        "tokens" ->
          unless Enum.any?(values, fn x -> x.id == @token end) do
            raise IppanCriticalError, "Native token missing"
          end

          Enum.each(values, fn x ->
            Token.insert(Token.to_list(x))
          end)

        "validators" ->
          Enum.each(values, fn x ->
            Validator.insert(Validator.to_list(x))
          end)

        "env" ->
          Enum.each(values, fn x ->
            Sqlite.step("insert_env", Env.to_list(x))
          end)
      end
    end

    # save all
    IO.puts("Here active")
    Sqlite.sync(db_ref)
    DetsPlux.sync(wallet_dets, wallet_tx)
  end
end
