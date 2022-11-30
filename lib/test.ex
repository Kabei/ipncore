defmodule Test do
  alias Ipncore.{Address, Event}

  @version 0
  @unit_time Default.unit_time()

  @seed <<126, 94, 236, 64, 158, 61, 121, 128, 15, 118, 103, 214, 90, 196, 11, 42, 2, 12, 65, 98,
          70, 247, 220, 114, 105, 204, 60, 222, 84, 159, 204, 160>>

  # {pk, sk, addr, addr58} = Test.start
  def start do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(@seed)
    addr = Address.hash(pk)
    addr58 = Address.to_text(addr)

    {pk, sk, addr, addr58}
  end

  # Test.pubkey_new PlatformOwner.pubkey, PlatformOwner.secret_key
  # Test.pubkey_new pk, sk
  def pubkey_new(pk, sk) do
    type_number = 1000
    time = :erlang.system_time(@unit_time)
    body = [Base.encode64(pk)]
    hash = Event.calc_hash(type_number, body, time)

    sig64 = signature64(sk, hash)

    [@version, "pubkey.new", time, body, sig64]
  end

  # Test.token_new PlatformOwner.secret_key, PlatformOwner.address
  def token_new(sk, owner) do
    type_number = Event.type_index("token.new")
    time = :erlang.system_time(@unit_time)
    from = PlatformOwner.address()
    body = ["IPN", Address.to_text(owner), Default.token_name(), 9, Default.token_symbol(), %{}]
    hash = Event.calc_hash(type_number, body, time)

    sig64 = signature64(sk, hash)
    [@version, "token.new", time, body, Address.to_text(from), sig64]
  end

  # Test.validator_new(PlatformOwner.secret_key, "ippan.red", "My Pool", PlatformOwner.address58, 2, 1.0)
  def validator_new(sk, hostname, name, owner, fee_type, fee) do
    type_number = Event.type_index("validator.new")
    time = :erlang.system_time(@unit_time)
    from = PlatformOwner.address()
    body = [hostname, name, owner, fee_type, fee]
    hash = Event.calc_hash(type_number, body, time)
    sig64 = signature64(sk, hash)

    [@version, "validator.new", time, body, Address.to_text(from), sig64]
  end

  defp signature64(sk, hash) do
    Falcon.sign(sk, hash)
    |> elem(1)
    |> Base.encode64()
  end
end
