defmodule Benchmark do
  alias Ipncore.Event

  @token "IPN"

  @secret_words [
    "eager unusual advice giraffe illness speak despair win sting fade iron poverty tower hobby carry panther start radio radio include rifle wear moon waste much fame possible romance excite reject document second cereal split rude above",
    "input miracle deputy public grit local number street fiber dry tilt enjoy labor drip live flash can helmet combine pause adult theory engine heavy arch celery diet science bright harvest clutch erupt system wool body about",
    "clinic turtle soldier figure tool swing body horse beauty toward brisk mixed clinic display blood dash final film trial pave glory miracle lab stomach forward drill soda hood ski sadness young fish uniform focus maid acid",
    "scan teach cabbage seed hospital zero vague either stool crew melody neutral unveil child protect key maid silver cotton peasant exchange crash body glow hurry short struggle grit immense camera virtual elite beyond bargain argue abuse",
    "midnight local ethics primary tenant million race theory awesome jelly prize lounge nature accuse elephant cup blind hub forest invest frequent coral slogan million woman laugh mutual icon dash label mixed move brain axis bind access",
    "flight smoke profit mosquito canvas consider tray today mask reopen kangaroo swallow blossom athlete journey genius change patch this all item settle trial renew pet noodle legend tag office cage arrest voyage sea left document able",
    "defense rotate question hand citizen jazz pioneer also garage hedgehog test never heavy relax chicken maximum mix draft wage sugar rack expect dinosaur story warrior uncle tilt theme gift immune dinosaur base patient gate lizard actual",
    "joy lunar book visit call ramp admit story clarify curtain wool load toe motor emerge blush crucial sell anger mad beach cabin hammer amateur rabbit hamster achieve plug monkey flash best upper opera denial critic accuse",
    "item account silent pattern vicious income box level subway umbrella strong habit permit state foster avoid recycle term voyage chimney grain peanut liberty exile genre absent blood later volcano meadow solid hat fish budget ankle about",
    "rare develop neck stone hero fence please cloth author notice fossil breeze pair income long screen photo polar arrow glory miss program soft pitch amazing neutral carpet wave weekend faint absurd fame useful foam tube acquire",
    "latin require guess humble admit magic whip defy kitten wall profit drastic differ finger exit trouble leopard glance mail ginger spoil claw emotion arrow carpet insane square first vital tumble length menu warrior exit anxiety absorb"
  ]

  @addresses [
    "1xk7PrW4TCGFmVndMD1Q2F2pvGgfT",
    "1x3FDY6JrGYYG9GdLVyrLudrkuVXfC",
    "1xegU3RwUBAtqBGL3FxWt3MZieRG5",
    "1x3KGc1nHqLi3gUHE4NRj5zGFxD4Gr",
    "1x4EeZJQfr9WbyRjMa3n4tN3R5H6cu",
    "1xmTyLfcuCnzh7P9Q8oFtECcX6M9B",
    "1x3tHsVGxfXoWLKWfTCnsLU2Nc7FWP",
    "1x3GHV8Mg9XqvkZpcHMEX3YxRLQxGz",
    "1x22Ztk7993YWzMopBKjQSzqHyAnmK",
    "1x2gqeoct9VTGgNu4UmkDeMeHRKqKQ",
    "1x46s977C4jd54iPNXxhycv6KGznYt"
  ]

  # {opk, osk, oaddr, oaddr58} = Test.owner_seed |> Test.wallet()
  # result = Test.tx_coinbase(osk, oaddr58, "IPN", 50_000_000_000, "1xk7PrW4TCGFmVndMD1Q2F2pvGgfT", "")
  # apply(Ipncore.Event, :check, result)

  @validator "ippan.uk"

  # Benchmark.send(0, 100, 50, "round-1")

  def send(bot_index, iterations, money, note) do
    addr58 = Enum.at(@addresses, bot_index)

    seed = Mnemonic.to_entropy(Enum.at(@secret_words, bot_index))
    {:ok, _pk, secret} = Falcon.gen_keys_from_seed(seed)

    addresses = List.delete(@addresses, bot_index + 1)
    total_addresses = length(addresses)

    for number <- 0..(iterations - 1) do
      addr58_to = Enum.at(addresses, rem(number, total_addresses))

      [version, type, time, event_body, from58, sig64] =
        Test.tx_send(secret, @token, addr58, addr58_to, money, @validator, note)

      Event.check(version, type, time, event_body, from58, sig64)
    end
  end
end

defmodule Test do
  alias Ipncore.{Address, Event}

  @version 0
  @unit_time Default.unit_time()

  def owner_seed,
    do:
      <<192, 198, 255, 103, 172, 14, 113, 243, 135, 19, 43, 1, 189, 146, 203, 162, 197, 175, 196,
        71, 66, 97, 116, 136, 228, 22, 123, 117, 121, 87, 42, 165, 163, 215, 36, 207, 207, 152,
        67, 166, 12, 43, 142, 237, 27, 77, 90, 177, 33, 19, 176, 175, 248, 195, 13, 161, 180, 81,
        91, 204, 239, 39, 45, 64>>

  def seed,
    do:
      <<126, 94, 236, 64, 158, 61, 121, 128, 15, 118, 103, 214, 90, 196, 11, 42, 2, 12, 65, 98,
        70, 247, 220, 114, 105, 204, 60, 222, 84, 159, 204, 160>>

  def seed2,
    do:
      <<127, 94, 236, 64, 158, 61, 121, 128, 15, 118, 103, 214, 90, 196, 11, 42, 2, 12, 65, 98,
        70, 247, 220, 114, 105, 204, 60, 222, 84, 159, 204, 160>>

  # alias Ipncore.{Address, Block, Event, Token, Balance, Tx, Txo, Domain, DnsRecord, Validator}

  # {opk, osk, oaddr, oaddr58} = Test.owner_seed |> Test.wallet()
  # {pk, sk, addr, addr58} = Test.seed() |> Test.wallet()
  # {pk2, sk2, addr2, addr2_58} = Test.seed2() |> Test.wallet()
  def wallet(seed) do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)
    addr = Address.hash(pk)
    addr58 = Address.to_text(addr)

    {pk, sk, addr, addr58}
  end

  # Test.pubkey_new(opk, osk)
  # Test.pubkey_new(pk, sk)
  # Test.pubkey_new(pk2, sk2)
  def pubkey_new(pk, sk) do
    type_number = Event.type_index("pubkey.new")
    time = :erlang.system_time(@unit_time)
    body = [Base.encode64(pk)]
    hash = Event.calc_hash(type_number, body, time)

    sig64 = signature64(sk, hash)

    [@version, "pubkey.new", time, body, sig64]
  end

  # Test.token_new(osk, oaddr58, Default.token, oaddr58, Default.token_name, 9, Default.token_symbol, "https://avatar.com", %{"opts" => ["burn", "coinbase", "lock"]})
  # Test.token_new(osk, oaddr58, "GBP", oaddr58, "Great British Pound", 5, "£", "https://avatar.com", %{"opts" => ["burn", "coinbase", "lock"]})
  # Test.token_new(osk, oaddr58, "USD", oaddr58, "US Dollar", 5, "$", "https://avatar.com", %{"opts" => ["burn", "coinbase", "lock"]})
  def token_new(sk, from58, token_id, owner58, name, decimals, symbol, avatar, props) do
    type_number = Event.type_index("token.new")
    time = :erlang.system_time(@unit_time)
    body = [token_id, owner58, name, decimals, symbol, avatar, props]
    hash = Event.calc_hash(type_number, body, time)

    sig64 = signature64(sk, hash)
    [@version, "token.new", time, body, from58, sig64]
  end

  # Test.token_update(osk, oaddr58, "IPN", %{"name" => "IPPAN"})
  def token_update(sk, from58, token_id, params) do
    type_number = Event.type_index("token.update")
    time = :erlang.system_time(@unit_time)
    body = [token_id, params]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "token.update", time, body, from58, sig64]
  end

  # Test.token_delete(osk, oaddr58, "IPN")
  def token_delete(sk, from58, token_id) do
    type_number = Event.type_index("token.delete")
    time = :erlang.system_time(@unit_time)
    body = [token_id]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "token.delete", time, body, from58, sig64]
  end

  # Test.validator_new(osk, oaddr58, "ippan.red", "My Pool", oaddr58, "https://avatar.com", 2, 1.0)
  def validator_new(sk, from58, hostname, name, owner58, avatar, fee_type, fee) do
    type_number = Event.type_index("validator.new")
    time = :erlang.system_time(@unit_time)
    body = [hostname, name, owner58, avatar, fee_type, fee]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)

    [@version, "validator.new", time, body, from58, sig64]
  end

  # Test.validator_update(osk, oaddr58, "ippan.red", %{"name" => "Your Pool"})
  def validator_update(sk, from58, hostname, params) do
    type_number = Event.type_index("validator.update")
    time = :erlang.system_time(@unit_time)
    body = [hostname, params]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "validator.update", time, body, from58, sig64]
  end

  # Test.validator_delete(osk, oaddr58, "ippan.red")
  def validator_delete(sk, from58, hostname) do
    type_number = Event.type_index("validator.delete")
    time = :erlang.system_time(@unit_time)
    body = [hostname]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "validator.delete", time, body, from58, sig64]
  end

  # Test.tx_coinbase(osk, oaddr58, "IPN", 1_000_000, addr58, "Texto")
  # Test.tx_coinbase(osk, oaddr58, "GBP", 1_000_000, addr2_58, "")
  def tx_coinbase(sk, from58, token, amount, to_address58, note) do
    type_number = Event.type_index("tx.coinbase")
    time = :erlang.system_time(@unit_time)
    outputs = [[to_address58, amount]]
    body = [token, outputs, note]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "tx.coinbase", time, body, from58, sig64]
  end

  # Test.tx_send(sk, Default.token, addr58, addr2_58, 50, "ippan.red", "")
  # Test.tx_send(sk2, "GBP", addr2_58, addr58, 5000, "ippan.red", "")
  def tx_send(sk, token, from58, to58, amount, validator_host, note) do
    type_number = Event.type_index("tx.send")
    time = :erlang.system_time(@unit_time)
    body = [token, to58, amount, validator_host, note]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "tx.send", time, body, from58, sig64]
  end

  # Test.domain_new(sk, addr58, "my-domain", "test@mail.com", "https://avatar.com", "my-domain-title", 2, "ippan.red")
  # Test.domain_new(sk, addr58, "ippan", "test@mail.com", "https://avatar.com", "IPPAN", 2, "ippan.red")
  def domain_new(sk, from58, name, email, avatar, title, years_to_renew, validator_host) do
    type_number = Event.type_index("domain.new")
    time = :erlang.system_time(@unit_time)
    body = [name, email, avatar, title, years_to_renew, validator_host]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "domain.new", time, body, from58, sig64]
  end

  # Test.domain_update(sk, addr58, "my-domain", "ippan.red", %{"email" => "myemail@email.com"})
  def domain_update(sk, from58, domain, validator_host, params) do
    type_number = Event.type_index("domain.update")
    time = :erlang.system_time(@unit_time)
    body = [domain, validator_host, params]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "domain.update", time, body, from58, sig64]
  end

  # Test.domain_delete(sk, addr58, "ippan")
  def domain_delete(sk, from58, hostname) do
    type_number = Event.type_index("domain.delete")
    time = :erlang.system_time(@unit_time)
    body = [hostname]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "domain.delete", time, body, from58, sig64]
  end

  # Test.domain_renew(sk, addr58, "ippan.com", 1, "ippan.red")
  def domain_renew(sk, from58, hostname, years_to_renew, validator_host) do
    type_number = Event.type_index("domain.renew")
    time = :erlang.system_time(@unit_time)
    body = [hostname, years_to_renew, validator_host]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "domain.renew", time, body, from58, sig64]
  end

  # Test.balance_lock(osk, oaddr58, addr2_58, "GBP", true)
  def balance_lock(sk, from58, to58, token_id, value) do
    type_number = Event.type_index("balance.lock")
    time = :erlang.system_time(@unit_time)
    body = [to58, token_id, value]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "balance.lock", time, body, from58, sig64]
  end

  # Test.dns_set(sk, addr58, "ippan.com", "a", "191.145.20.15", 86400, "ippan.red")
  def dns_set(sk, from58, domain, type, value, ttl, validator_host) do
    type_number = Event.type_index("dns.set")
    time = :erlang.system_time(@unit_time)
    body = [domain, type, value, ttl, validator_host]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "dns.set", time, body, from58, sig64]
  end

  # Test.dns_push(sk, addr58, "ippan.com", "a", "44.203.142.247", 86400, "ippan.red")
  # Test.dns_push(sk, addr58, "ippan.com", "a", "191.145.20.15", 86400, "ippan.red")
  def dns_push(sk, from58, domain, type, value, ttl, validator_host) do
    type_number = Event.type_index("dns.push")
    time = :erlang.system_time(@unit_time)
    body = [domain, type, value, ttl, validator_host]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "dns.push", time, body, from58, sig64]
  end

  # Test.dns_drop(sk, addr58, "ippan.com")
  def dns_drop(sk, from58, domain) do
    type_number = Event.type_index("dns.drop")
    time = :erlang.system_time(@unit_time)
    body = [domain]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "dns.drop", time, body, from58, sig64]
  end

  # Test.dns_drop(sk, addr58, "ippan.com", "a")
  def dns_drop(sk, from58, domain, type) do
    type_number = Event.type_index("dns.drop")
    time = :erlang.system_time(@unit_time)
    body = [domain, type]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "dns.drop", time, body, from58, sig64]
  end

  # Test.tx_refund(sk2, addr2_58, 1500000000, "97385af5e091a7feca8f32f34c8f107c2c243c8e61075607ac183dfbd6995a25")
  def tx_refund(sk, from58, ev_time, ev_hash) do
    type_number = Event.type_index("tx.refund")
    time = :erlang.system_time(@unit_time)
    body = [ev_time, ev_hash]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "tx.refund", time, body, from58, sig64]
  end

  defp signature64(sk, hash) do
    Falcon.sign(sk, hash)
    |> elem(1)
    |> Base.encode64()
  end
end
