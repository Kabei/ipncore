defmodule Global do
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

  defmacro net_pubkey do
    quote do
      Default.get(:net_pubkey)
    end
  end

  defmacro net_privkey do
    quote do
      Default.get(:net_privkey)
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
      pubkey: Default.get(:pubkey),
      privkey: Default.get(:privkey),
      net_pubkey: Default.get(:net_pubkey),
      net_privkey: Default.get(:net_privkey),
      vid: Default.get(:vid)
    })
  end
end
