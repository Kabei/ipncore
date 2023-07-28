defmodule Global do
  import Ippan.Utils, only: [to_atom: 1]

  defmacro miner do
    quote do
      Default.get(:miner)
    end
  end

  defmacro owner do
    quote do
      Default.get(:owner)
    end
  end

  defmacro pubkey do
    quote do
      Default.get(:privkey)
    end
  end

  defmacro privkey do
    quote do
      Default.get(:privkey)
    end
  end

  defmacro has_owner? do
    quote do
      case Default.get(:owner, false) do
        false ->
          false

        _ ->
          true
      end
    end
  end

  defmacro owner?(id) do
    quote do
      Default.get(:owner, nil) == unquote(id)
    end
  end

  defmacro validator_id do
    quote do
      Default.get(:vid)
    end
  end

  @token Application.compile_env(:ipncore, :token)
  def update do
    token = TokenStore.lookup_map(@token)

    GlobalConst.new(Default, %{
      owner: token.owner,
      miner: System.get_env("MINER") |> to_atom(),
      pubkey: Application.get_env(:ipncore, :pubkey),
      privkey: Application.get_env(:ipncore, :privkey),
      vid: Application.get_env(:ipncore, :vid)
    })
  end
end
