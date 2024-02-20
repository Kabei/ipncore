defmodule Builder do
  alias Ippan.Address
  require Logger

  @type response :: {Client.t(), binary, binary}

  defmodule Client do
    @type t :: %Client{
            seed: binary,
            id: binary,
            pk: binary,
            secret: binary,
            sig_type: 0 | 1 | 2,
            nonce: pos_integer()
          }

    defstruct [:seed, :id, :pk, :secret, :sig_type, nonce: 1]

    @spec new(binary, 0 | 1 | 2) :: t()
    def new(seed, sig_type \\ 0) do
      {pk, sk, account_id} =
        case sig_type do
          0 ->
            Builder.gen_ed25519(seed)

          1 ->
            Builder.gen_secp256k1(seed)

          2 ->
            Builder.gen_falcon(seed)
        end

      %Client{secret: sk, pk: pk, seed: seed, id: account_id, sig_type: sig_type}
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

  @spec print(response) :: Client.t()
  def print({client, body, sig}) do
    IO.puts(body)
    IO.puts(sig)
    client
  end

  @spec post(response, String.t()) ::
          {:ok, Client.t()}
          | {:redirect, String.t()}
          | {:error, integer, String.t()}
          | {:error, HTTPoison.Error.t()}
  def post({client, body, sig64}, hostname) do
    url = "https://#{hostname}/v1/call"

    case HTTPoison.post(url, body, [{"auth", sig64}], hackney: [:insecure]) do
      {:ok, %{status_code: code, body: msg, headers: headers}} ->
        case code do
          200 ->
            {:ok, client}

          302 ->
            {:redirect, Map.new(headers) |> Map.get("location", "")}

          code ->
            {:error, code, msg}
        end

      error ->
        error
    end
  end

  # {pk, sk, account_id} = Builder.gen_ed25519(seed)
  def gen_ed25519(seed) do
    {:ok, {pk, sk}} = Cafezinho.Impl.keypair_from_seed(seed)

    {pk, sk, Address.hash(0, pk)}
  end

  # {pk, sk, account_id} = Builder.gen_secp256k1(seed)
  def gen_secp256k1(seed) do
    {:ok, pk} = ExSecp256k1.Impl.create_public_key(seed)

    {pk, seed, Address.hash(1, pk)}
  end

  # {pk, sk, account_id} = Builder.gen_falcon(seed)
  def gen_falcon(seed) do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    {pk, sk, Address.hash(2, pk)}
  end

  # Builder.account_new(client, 0, 0, 1) |> Builder.print
  def account_new(
        client = %Client{
          id: account_id,
          nonce: nonce,
          pk: pk,
          sig_type: sig_type
        },
        vid,
        fa,
        fb
      ) do
    body =
      [
        0,
        nonce,
        account_id,
        Base.encode64(pk),
        sig_type,
        vid,
        fa,
        fb
      ]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.account_sub(client, 0, 0, 1) |> Builder.print
  def account_sub(
        client = %Client{
          id: account_id,
          nonce: nonce
        },
        validator_id,
        fa,
        fb
      ) do
    body =
      [1, nonce, account_id, validator_id, fa, fb]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.account_editKey(client, <<191, 139, 1, 109, 27, 99, 67, 136, 137, 116, 102, 35, 203, 89, 225, 151, 213, 34, 125, 73, 244, 184, 108, 186, 47, 89, 90, 128, 52, 120, 125, 119>>, 0) |> Builder.print
  def account_sub(
        client = %Client{
          id: account_id,
          nonce: nonce
        },
        pubkey,
        sig_type
      ) do
    body =
      [2, nonce, account_id, Base.encode64(pubkey), sig_type]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.env_set(client, "test", "value-test") |> Builder.print
  def env_set(
        client = %Client{id: account_id, nonce: nonce},
        name,
        value
      ) do
    body =
      [50, nonce, account_id, name, value]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.env_delete(client, "test") |> Builder.print
  def env_delete(client = %Client{id: account_id, nonce: nonce}, name) do
    body =
      [51, nonce, account_id, name]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.validator_join(client, "ippan.net", 5815, account_id, "net core", pkv, 1, 5, %{"avatar" => "https://avatar.com"}) |> Builder.print()
  def validator_join(
        client = %Client{id: account_id, nonce: nonce},
        hostname,
        port,
        owner,
        name,
        pubkey,
        net_pubkey,
        fa \\ 0,
        fb \\ 1,
        opts \\ %{}
      )
      when fa >= 0 and fb >= 0 do
    body =
      [
        100,
        nonce,
        account_id,
        hostname,
        port,
        owner,
        name,
        Fast64.encode64(pubkey),
        Fast64.encode64(net_pubkey),
        fa,
        fb,
        opts
      ]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.validator_update(client, 1, %{"fa" => 7}) |> Builder.print()
  def validator_update(client = %Client{id: account_id, nonce: nonce}, id, params) do
    body =
      [101, nonce, account_id, id, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.validator_leave(client, 1) |> Builder.print()
  def validator_leave(client = %Client{id: account_id, nonce: nonce}, id) do
    body =
      [102, nonce, account_id, id]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def validator_env_put(client = %Client{id: account_id, nonce: nonce}, id, name, value) do
    body =
      [103, nonce, account_id, id, name, value]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def validator_env_delete(client = %Client{id: account_id, nonce: nonce}, id, name) do
    body =
      [104, nonce, account_id, id, name]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def validator_active(client = %Client{id: account_id, nonce: nonce}, id, active) do
    body =
      [105, nonce, account_id, id, active]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.token_new(client, "IPN", client2.id, "IPPAN", 9, "Þ", 0, %{"avatar" => "https://avatar.com", "props" => ["burn", "coinbase", "lock"]})
  # Builder.token_new(client, "USD", client2.id, "DOLLAR", 5, "$", 0, %{"avatar" => "https://avatar.com", "props" => ["burn", "coinbase", "lock"]}) |> Builder.print
  def token_new(
        client = %Client{id: account_id, nonce: nonce},
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
        nonce,
        account_id,
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
  def token_update(client = %Client{id: account_id, nonce: nonce}, id, params) do
    body =
      [201, nonce, account_id, id, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.token_delete(client, "USD") |> Builder.print()
  def token_delete(client = %Client{id: account_id, nonce: nonce}, id) do
    body =
      [202, nonce, account_id, id]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def token_prop_add(client = %Client{id: account_id, nonce: nonce}, id, props) do
    body =
      [203, nonce, account_id, id, props]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def token_prop_drop(client = %Client{id: account_id, nonce: nonce}, id, props) do
    body =
      [204, nonce, account_id, id, props]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def token_env_put(client = %Client{id: account_id, nonce: nonce}, id, name, value) do
    body =
      [205, nonce, account_id, id, name, value]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def token_env_delete(client = %Client{id: account_id, nonce: nonce}, id, name) do
    body =
      [206, nonce, account_id, id, name]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_new(client, "IPN", [[address2, 50000000]]) |> Builder.print()
  def coin_new(client = %Client{id: account_id, nonce: nonce}, token, outputs) do
    body =
      [300, nonce, account_id, token, outputs]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_send(client, client2.id, "IPN", 50000, "Test note", true) |> Builder.print()
  # Builder.coin_send(client, client2.id, "IPN", 4000) |> Builder.print()
  def coin_send(
        client = %Client{id: account_id, nonce: nonce},
        to,
        token,
        amount,
        note \\ "",
        refund \\ false
      ) do
    count = String.length(note)

    body =
      cond do
        count == 0 and refund == false ->
          [301, nonce, account_id, to, token, amount]

        count == 0 and refund ->
          [301, nonce, account_id, to, token, amount, "", refund]

        refund == false ->
          [301, nonce, account_id, to, token, amount, note]

        true ->
          [301, nonce, account_id, to, token, amount, note, true]
      end
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def coin_multisend(client = %Client{id: account_id, nonce: nonce}, token, outputs, note \\ "") do
    body =
      if String.length(note) == 0 do
        [306, nonce, account_id, token, outputs]
      else
        [306, nonce, account_id, token, outputs, note]
      end
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_refund(client, "21520DCFF38E79472E768E98A0FEFC901F4AADA2633E23E116E74181651290BA") |> Builder.print()
  def coin_refund(client = %Client{id: account_id, nonce: nonce}, hash) do
    body =
      [302, nonce, account_id, hash]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def coin_lock(client = %Client{id: account_id, nonce: nonce}, to, token_id, amount) do
    body =
      [303, nonce, account_id, to, token_id, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def coin_unlock(client = %Client{id: account_id, nonce: nonce}, to, token_id, amount) do
    body =
      [304, nonce, account_id, to, token_id, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_burn(client, "IPN", 1000) |> Builder.print()
  def coin_burn(client = %Client{id: account_id, nonce: nonce}, token, amount) do
    body =
      [305, nonce, account_id, token, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_burn(client, "IPN", client2.id, 1000) |> Builder.print()
  def coin_burn(client = %Client{id: account_id, nonce: nonce}, to, token, amount) do
    body =
      [305, nonce, account_id, to, token, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_reload(client, "XPN") |> Builder.print()
  def coin_reload(client = %Client{id: account_id, nonce: nonce}, token) do
    body =
      [307, nonce, account_id, token]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_stream(client, client.id, "XPN", 500)
  def coin_stream(client = %Client{id: account_id, nonce: nonce}, to, token, amount) do
    body =
      [308, nonce, account_id, to, token, amount]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.coin_auth(client, client.id, "XPN", true)
  def coin_auth(client = %Client{id: account_id, nonce: nonce}, to, token, auth) do
    body =
      [309, nonce, account_id, to, token, auth]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_new(client, "example.ipn", account_id, 2, %{"email" => "asd@example.com", "avatar" => "https://avatar.com"}) |> Builder.print()
  def domain_new(
        client = %Client{id: account_id, nonce: nonce},
        domain_name,
        owner,
        days,
        %{
          "email" => _email,
          "avatar" => _avatar
        } = params
      ) do
    body =
      [400, nonce, account_id, domain_name, owner, days, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_update(client, "example.ipn", %{"email" => "pop@email.com"}) |> Builder.print()
  def domain_update(
        client = %Client{id: account_id, nonce: nonce},
        domain_name,
        params
      ) do
    body =
      [401, nonce, account_id, domain_name, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_delete(client, "example.ipn") |> Builder.print()
  def domain_delete(client = %Client{id: account_id, nonce: nonce}, domain_name) do
    body =
      [402, nonce, account_id, domain_name]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.domain_renew(client, "example.ipn", 1000) |> Builder.print()
  def domain_renew(client = %Client{id: account_id, nonce: nonce}, domain_name, days) do
    body =
      [403, nonce, account_id, domain_name, days]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_new(client = %Client{id: account_id, nonce: nonce}, fullname, type, data, ttl) do
    body =
      [500, nonce, account_id, fullname, type, data, ttl]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_update(client = %Client{id: account_id, nonce: nonce}, fullname, dns_hash16, params) do
    body =
      [501, nonce, account_id, fullname, dns_hash16, params]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_delete(client = %Client{id: account_id, nonce: nonce}, fullname) do
    body =
      [502, nonce, account_id, fullname]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_delete(client = %Client{id: account_id, nonce: nonce}, fullname, type)
      when is_integer(type) do
    body =
      [502, nonce, account_id, fullname, type]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def dns_delete(client = %Client{id: account_id, nonce: nonce}, fullname, hash16) do
    body =
      [502, nonce, account_id, fullname, hash16]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # service_new(client, "@ippan", "movies-live", %{"summary" => "Watch movies", "image" => "http://image.com", "max_amount" => 1500})
  def service_new(client = %Client{id: account_id, nonce: nonce}, id, name, extra) do
    body =
      [600, nonce, account_id, id, name, extra]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # service_update(client, "@ippan", %{"name" => "movies-live", "summary" => "Watch movies 2"})
  def service_update(client = %Client{id: account_id, nonce: nonce}, service_id, map) do
    body =
      [601, nonce, account_id, service_id, map]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # service_delete(client, "@ippan")
  def service_delete(client = %Client{id: account_id, nonce: nonce}, service_id) do
    body =
      [602, nonce, account_id, service_id]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # service_subscribe(client, "@ippan", %{"max_amount" => 50000})
  def service_subscribe(client = %Client{id: account_id, nonce: nonce}, service_id, extra) do
    body =
      [603, nonce, account_id, service_id, extra]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # service_unsubscribe(client, "@ippan", %{"max_amount" => 50000})
  def service_unsubscribe(client = %Client{id: account_id, nonce: nonce}, service_id) do
    body =
      [604, nonce, account_id, service_id]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  # Builder.sys_upgrade(client, %{"git" => "pull", "deps" -> "get", "reset" => "all", "compile" => "force"}, ["ipncore", "ipnworker"])
  def sys_upgrade(client = %Client{id: account_id, nonce: nonce}, opts, target_apps) do
    body =
      [900, nonce, account_id, opts, target_apps]
      |> encode_fun!()

    hash = hash_fun(body)

    sig = signature64(client, hash)

    {Client.cont(client), body, sig}
  end

  def custom(client = %Client{id: account_id}, type, args) do
    {:ok, body} =
      [type, account_id, args] |> Jason.encode()

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
          {:ok, {signature, _recovery_id_int}} = ExSecp256k1.Impl.sign_compact(msg, secret)
          signature

        2 ->
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
