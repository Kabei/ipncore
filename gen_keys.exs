alias IO.ANSI

args = System.argv() |> Enum.filter(&(&1 != "only"))

# Secret key
arg1 = List.first(args)

seed =
  case arg1 do
    nil ->
      :rand.bytes(32)

    secret ->
      case Base.decode64(secret) do
        {:ok, seed} when byte_size(seed) == 32 ->
          seed

        _ ->
          raise RuntimeError, "First argument is a bad base64 or Bad length"
      end
  end

{pk, _sk, account_id} = Builder.gen_ed25519(seed)

IO.puts(ANSI.yellow() <> "Secret Key:" <> ANSI.reset())
IO.puts(Base.encode64(seed))
IO.puts(ANSI.green() <> "Pubkey:" <> ANSI.reset())
IO.puts(Base.encode64(pk))
IO.puts(ANSI.green() <> "Owner:" <> ANSI.reset())
IO.puts(account_id)
IO.puts("")

# Cluster key
arg2 = Enum.at(args, 1, nil)

seed =
  case arg2 do
    nil ->
      :rand.bytes(32)

    secret ->
      case Base.decode64(secret) do
        {:ok, seed} when byte_size(seed) == 32 ->
          seed

        _ ->
          raise RuntimeError, "Second argument is a bad base64"
      end
  end

{:ok, pk, _sk} = NtruKem.gen_key_pair_from_seed(seed)

IO.puts(ANSI.yellow() <> "Cluster Key:" <> ANSI.reset())
IO.puts(Base.encode64(seed))
IO.puts(ANSI.green() <> "Net Pubkey:" <> ANSI.reset())
IO.puts(Base.encode64(pk))
IO.puts("")

IO.puts(
  "(!) Note: For validator registration use the green variables and for env_file use the yellow variables."
)
