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

    {data = %{"tokens" => _, "validators" => _, "accounts" => _}, _binding} =
      Path.join(:code.priv_dir(@app), filename)
      |> Code.eval_file()

    wallet_dets = DetsPlux.get(:wallet)
    wallet_tx = DetsPlux.tx(:wallet)

    for {key, values} <- data do
      case key do
        "accounts" ->
          Enum.each(values, fn x ->
            DetsPlux.put(
              wallet_tx,
              {x.id, x.pubkey, x.sig_type, %{vid: x.vid, fa: x.fa, fb: x.fb}}
            )
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
          Enum.each(values, fn %{name: name, value: value} ->
            EnvStore.put(db_ref, name, value)
          end)
      end
    end

    # save all
    Sqlite.sync(db_ref)
    DetsPlux.sync(wallet_dets, wallet_tx)
    IO.puts("Genesis file loaded")
  end
end
