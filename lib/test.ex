defmodule Test do
  alias Ipncore.{Address, Event}

  @version 0
  @unit_time Default.unit_time()

  @seed <<126, 94, 236, 64, 158, 61, 121, 128, 15, 118, 103, 214, 90, 196, 11, 42, 2, 12, 65, 98,
          70, 247, 220, 114, 105, 204, 60, 222, 84, 159, 204, 160>>
  @seed2 <<127, 94, 236, 64, 158, 61, 121, 128, 15, 118, 103, 214, 90, 196, 11, 42, 2, 12, 65, 98,
           70, 247, 220, 114, 105, 204, 60, 222, 84, 159, 204, 160>>

  # alias Ipncore.{Address, Block, Event, Token, Balance, Tx, Txo, Domain, Validator}

  # Owner
  # {osk, opk, oaddr, oaddr58} = {PlatformOwner.secret_key, PlatformOwner.pubkey, PlatformOwner.address, PlatformOwner.address58}

  # {pk, sk, addr, addr58} = Test.wallet1
  def wallet1 do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(@seed)
    addr = Address.hash(pk)
    addr58 = Address.to_text(addr)

    {pk, sk, addr, addr58}
  end

  # {pk2, sk2, addr2, addr2_58} = Test.wallet2
  def wallet2 do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(@seed2)
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

  # Test.token_new(osk, oaddr58, Default.token, oaddr58, Default.token_name, 9, Default.token_symbol, %{"opts" => ["burn", "coinbase", "lock"]})
  # Test.token_new(osk, oaddr58, "GBP", oaddr58, "Great British Pound", 5, "£", %{"opts" => ["burn", "coinbase", "lock"]})
  # Test.token_new(osk, oaddr58, "USD", oaddr58, "US Dollar", 5, "$", %{"opts" => ["burn", "coinbase", "lock"]})
  def token_new(sk, from58, token_id, owner58, name, decimals, symbol, props) do
    type_number = Event.type_index("token.new")
    time = :erlang.system_time(@unit_time)
    body = [token_id, owner58, name, decimals, symbol, props]
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

  # Test.validator_new(osk, oaddr58, "ippan.red", "My Pool", oaddr58, 2, 1.0)
  def validator_new(sk, from58, hostname, name, owner58, fee_type, fee) do
    type_number = Event.type_index("validator.new")
    time = :erlang.system_time(@unit_time)
    body = [hostname, name, owner58, fee_type, fee]
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
  def tx_coinbase(sk, from58, token, amount, to_address58, memo) do
    type_number = Event.type_index("tx.coinbase")
    time = :erlang.system_time(@unit_time)
    outputs = [[to_address58, amount]]
    body = [token, outputs, memo]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "tx.coinbase", time, body, from58, sig64]
  end

  # Test.tx_send(sk, Default.token, addr58, addr2_58, 50, "ippan.red", false, "")
  # Test.tx_send(sk2, "GBP", addr2_58, addr58, 5000, "ippan.red", true, "")
  def tx_send(sk, token, from58, to58, amount, validator_host, refundable, memo) do
    type_number = Event.type_index("tx.send")
    time = :erlang.system_time(@unit_time)
    body = [token, to58, amount, validator_host, refundable, memo]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "tx.send", time, body, from58, sig64]
  end

  # Test.domain_new(sk, addr58, "my-domain", "test@mail.com", "https://avatar.com", 2, "ippan.red")
  def domain_new(sk, from58, name, email, avatar, years_to_renew, validator_host) do
    type_number = Event.type_index("domain.new")
    time = :erlang.system_time(@unit_time)
    body = [name, email, avatar, years_to_renew, validator_host]
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

  # Test.domain_delete(sk, addr58, "my-domain")
  def domain_delete(sk, from58, hostname) do
    type_number = Event.type_index("domain.delete")
    time = :erlang.system_time(@unit_time)
    body = [hostname]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)
    [@version, "domain.delete", time, body, from58, sig64]
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

  defp signature64(sk, hash) do
    Falcon.sign(sk, hash)
    |> elem(1)
    |> Base.encode64()
  end
end
