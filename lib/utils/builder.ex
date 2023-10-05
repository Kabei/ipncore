defmodule Builder do
  alias Ippan.Address
  require Logger

  @compile {:inline, hash_fun: 1, encode_fun!: 1}
  @type response :: {Client.t(), {binary, binary}}

  defmodule Client do
    @type t :: %Client{
            seed: binary,
            address: binary,
            pk: binary,
            secret: binary,
            sig_type: 0 | 1 | 2,
            nonce: pos_integer()
          }

    @compile {:inline, cont: 1}

    defstruct [:seed, :address, :pk, :secret, :sig_type, nonce: 1]

    @spec new(binary, 0 | 1 | 2) :: t()
    def new(seed, sig_type \\ 0) do
      {pk, sk, address} =
        case sig_type do
          0 ->
            Builder.gen_ed25519(seed)

          1 ->
            Builder.gen_falcon(seed)
        end

      %Client{secret: sk, pk: pk, seed: seed, address: address, sig_type: sig_type}
    end

    @spec cont(t) :: t
    def cont(client = %Client{nonce: nonce}) do
      %{client | nonce: nonce + 1}
    end
  end

  # {client, client2} = Builder.test()
  def test do
    sk =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    sk2 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 84>>

    {Client.new(sk), Client.new(sk2)}
  end

  @spec print({Client.t(), binary, binary}) :: Client.t()
  def print({client, body, sig}) do
    IO.puts(body)
    IO.puts(sig)
    client
  end

  # {pk, sk, address} = Builder.gen_ed25519(seed)
  def gen_ed25519(seed) do
    {:ok, {pk, sk}} = Cafezinho.Impl.keypair_from_seed(seed)

    {pk, sk, Address.hash(0, pk)}
  end

  # {pkv, skv, addressv} = Builder.gen_falcon(seed)
  def gen_falcon(seed) do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    {pk, sk, Address.hash(1, pk)}
  end

  # Builder.wallet_sub(client, 0) |> Builder.print
  def wallet_sub(
        client = %Client{
          address: address,
          nonce: nonce,
          pk: pk,
          sig_type: sig_type
        },
        validator_id
      ) do
    body =
      [
        0,
        :os.system_time(:millisecond),
        nonce,
        address,
        Base.encode64(pk),
        validator_id,
        sig_type
      ]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.wallet_unsub(client) |> Builder.print
  def wallet_unsub(client = %Client{address: address, nonce: nonce}) do
    body =
      [1, :os.system_time(:millisecond), nonce, address]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.env_set(client, "test", "value-test") |> Builder.print
  def env_set(
        client = %Client{address: address, nonce: nonce},
        name,
        value
      ) do
    body =
      [50, :os.system_time(:millisecond), nonce, address, name, value]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.env_delete(client, "test") |> Builder.print
  def env_delete(client = %Client{address: address, nonce: nonce}, name) do
    body =
      [51, :os.system_time(:millisecond), nonce, address, name]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.validator_new(client, "ippan.net", 5815, address, "net core", pkv, 1, 5.0, %{"avatar" => "https://avatar.com"}) |> Builder.print()
  def validator_new(
        client = %Client{address: address, nonce: nonce},
        hostname,
        port,
        owner,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        opts \\ %{}
      )
      when is_float(fee) and fee_type in 0..2 do
    body =
      [
        100,
        :os.system_time(:millisecond),
        nonce,
        address,
        hostname,
        port,
        owner,
        name,
        Fast64.encode64(pubkey),
        Fast64.encode64(net_pubkey),
        fee_type,
        fee,
        opts
      ]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.validator_update(client, 1, %{"fee" => 7.0}) |> Builder.print()
  def validator_update(client = %Client{address: address, nonce: nonce}, id, params) do
    body =
      [101, :os.system_time(:millisecond), nonce, address, id, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.validator_delete(client, 1) |> Builder.print()
  def validator_delete(client = %Client{address: address, nonce: nonce}, id) do
    body =
      [102, :os.system_time(:millisecond), nonce, address, id]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.token_new(client, "IPN", address, "IPPAN", 9, "Þ", 0, %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]})
  # Builder.token_new(client, "USD", address, "DOLLAR", 5, "$", 0, %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]}) |> Builder.print
  def token_new(
        client = %Client{address: address, nonce: nonce},
        token_id,
        owner,
        name,
        decimal,
        symbol,
        max_supply,
        %{
          "avatar" => _avatar_url,
          "props" => _props
        } = opts
      ) do
    body =
      [
        200,
        :os.system_time(:millisecond),
        nonce,
        address,
        token_id,
        owner,
        name,
        decimal,
        symbol,
        max_supply,
        opts
      ]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)
    # IO.inspect(sig)
    {Client.cont(client), body, sig}
  end

  # Builder.token_update(client, "USD", %{"name" => "Dollar"}) |> Builder.print()
  def token_update(client = %Client{address: address, nonce: nonce}, id, params) do
    body =
      [201, :os.system_time(:millisecond), nonce, address, id, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.token_delete(client, "USD") |> Builder.print()
  def token_delete(client = %Client{address: address, nonce: nonce}, id) do
    body =
      [202, :os.system_time(:millisecond), nonce, address, id]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def balance_lock(client = %Client{address: address, nonce: nonce}, to, token_id, amount) do
    body =
      [250, :os.system_time(:millisecond), nonce, address, to, token_id, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def balance_unlock(client = %Client{address: address, nonce: nonce}, to, token_id, amount) do
    body =
      [251, :os.system_time(:millisecond), nonce, address, to, token_id, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.tx_coinbase(client, "IPN", [[address2, 50000000]]) |> Builder.print()
  def tx_coinbase(client = %Client{address: address, nonce: nonce}, token, outputs) do
    body =
      [300, :os.system_time(:millisecond), nonce, address, token, outputs]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.tx_send(client, address, "IPN", 50000) |> Builder.print()
  # Builder.tx_send(client, address, "IPN", 4000) |> Builder.print()
  def tx_send(client = %Client{address: address, nonce: nonce}, to, token, amount) do
    body =
      [301, :os.system_time(:millisecond), nonce, address, to, token, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def tx_send(client = %Client{address: address, nonce: nonce}, to, token, amount, note) do
    body =
      [301, :os.system_time(:millisecond), nonce, address, to, token, amount, note]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def tx_refundable(client = %Client{address: address, nonce: nonce}, to, token, amount) do
    body =
      [302, :os.system_time(:millisecond), nonce, address, to, token, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.tx_refund(client, "21520DCFF38E79472E768E98A0FEFC901F4AADA2633E23E116E74181651290BA") |> Builder.print()
  def tx_refund(client = %Client{address: address, nonce: nonce}, hash) do
    body =
      [303, :os.system_time(:millisecond), nonce, address, hash]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.tx_burn(client, "IPN", 1000) |> Builder.print()
  def tx_burn(client = %Client{address: address, nonce: nonce}, token, amount) do
    body =
      [304, :os.system_time(:millisecond), nonce, address, token, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_new(client, "example.ipn", address, 2, %{"email" => "asd@example.com", "avatar" => "https://avatar.com"}) |> Builder.print()
  def domain_new(
        client = %Client{address: address, nonce: nonce},
        domain_name,
        owner,
        days,
        %{
          "email" => _email,
          "avatar" => _avatar
        } = params
      ) do
    body =
      [400, :os.system_time(:millisecond), nonce, address, domain_name, owner, days, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_update(client, "example.ipn", %{"email" => "pop@email.com"}) |> Builder.print()
  def domain_update(
        client = %Client{address: address, nonce: nonce},
        domain_name,
        params
      ) do
    body =
      [401, :os.system_time(:millisecond), nonce, address, domain_name, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_delete(client, "example.ipn") |> Builder.print()
  def domain_delete(client = %Client{address: address, nonce: nonce}, domain_name) do
    body =
      [402, :os.system_time(:millisecond), nonce, address, domain_name]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_renew(client, "example.ipn", 1000) |> Builder.print()
  def domain_renew(client = %Client{address: address, nonce: nonce}, domain_name, days) do
    body =
      [403, :os.system_time(:millisecond), nonce, address, domain_name, days]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_new(client = %Client{address: address, nonce: nonce}, fullname, type, data, ttl) do
    body =
      [500, :os.system_time(:millisecond), nonce, address, fullname, type, data, ttl]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_update(client = %Client{address: address, nonce: nonce}, fullname, dns_hash16, params) do
    body =
      [501, :os.system_time(:millisecond), nonce, address, fullname, dns_hash16, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_delete(client = %Client{address: address, nonce: nonce}, fullname) do
    body =
      [502, :os.system_time(:millisecond), nonce, address, fullname]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_delete(client = %Client{address: address, nonce: nonce}, fullname, type)
      when is_integer(type) do
    body =
      [502, :os.system_time(:millisecond), nonce, address, fullname, type]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_delete(client = %Client{address: address, nonce: nonce}, address, fullname, hash16) do
    body =
      [502, :os.system_time(:millisecond), nonce, address, fullname, hash16]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def custom(client = %Client{address: address}, type, timestamp, args) do
    {:ok, body} =
      [type, timestamp, address, args]
      |> Jason.encode()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  defp signature64(%Client{secret: secret, sig_type: sig_type}, msg) do
    result =
      case sig_type do
        0 ->
          Cafezinho.Impl.sign(msg, secret)
          |> elem(1)

        1 ->
          Falcon.sign(secret, msg)
          |> elem(1)
      end

    Base.encode64(result)
  end

  defp hash_fun(msg) do
    Blake3.hash(msg)
  end

  defp encode_fun!(data) do
    Jason.encode!(data)
  end
end
