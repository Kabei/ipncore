defmodule Platform do
  alias Ippan.{Token, Validator}
  require Token
  require Validator
  require Sqlite

  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)

  def start do
    db_ref = :persistent_term.get(:main_conn)
    EnvStore.load(db_ref)

    case EnvStore.owner() do
      nil ->
        load_genesis_file(db_ref)

      _owner ->
        :ok
    end
  end

  defp load_genesis_file(db_ref) do
    filename =
      case Mix.env() do
        :dev -> "genesis-dev.exs"
        _ -> "genesis.exs"
      end

    path = Path.join(:code.priv_dir(@app), filename)

    if File.exists?(path) do
      {data = %{"tokens" => _, "validators" => _, "accounts" => _}, _binding} =
        Code.eval_file(path)

      wallet_db = DetsPlux.get(:wallet)
      nonce_db = DetsPlux.get(:nonce)
      balance_db = DetsPlux.get(:balance)
      wallet_tx = DetsPlux.tx(wallet_db, :wallet)
      balance_tx = DetsPlux.tx(balance_db, :balance)
      nonce_tx = DetsPlux.tx(nonce_db, :nonce)
      stats = Stats.new()

      for {key, values} <- data do
        case key do
          "accounts" ->
            Task.async(fn ->
              :ets.insert(wallet_tx, values)
            end)

          "nonce" ->
            Task.async(fn ->
              :ets.insert(nonce_tx, values)
            end)

          "balances" ->
            Task.async(fn ->
              :ets.insert(balance_tx, values)
            end)

          "tokens" ->
            Task.async(fn ->
              unless Enum.any?(values, fn x -> x.id == @token end) do
                raise IppanCriticalError, "Native token missing"
              end

              Enum.each(values, fn x ->
                Token.insert(Token.to_list(x))
              end)
            end)

          "validators" ->
            Task.async(fn ->
              Enum.each(values, fn x ->
                Validator.insert(Validator.to_list(x))
              end)
            end)

          "env" ->
            Task.async(fn ->
              Enum.each(values, fn %{name: name, value: value} ->
                EnvStore.put(db_ref, name, value)
              end)
            end)

          "stats" ->
            Task.async(fn ->
              :ets.insert(stats.tx, values)
            end)
        end
      end
      |> Task.await_many(:infinity)

      # save all
      [
        Task.async(fn -> Sqlite.sync(db_ref) end),
        Task.async(fn -> DetsPlux.sync(wallet_db, wallet_tx) end),
        Task.async(fn -> DetsPlux.sync(nonce_db, nonce_tx) end),
        Task.async(fn -> DetsPlux.sync(balance_db, balance_tx) end),
        Task.async(fn -> DetsPlux.sync(stats.db, stats.tx) end)
      ]
      |> Task.await_many(:infinity)

      IO.puts("Genesis file loaded")
    end
  end
end
