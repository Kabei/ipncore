defmodule CryptoTest do
  use ExUnit.Case
  doctest Ipncore

  test "" do
    sk = :crypto.strong_rand_bytes(32)
    lib = ExSecp256k1.Impl
    {:ok, pk} = lib.create_public_key(sk)
    {:ok, pk2} = lib.public_key_compress(pk)

    msg = :crypto.strong_rand_bytes(32)
    {:ok, {signature, _recovery_id_int}} = lib.sign_compact(msg, sk)

    Benchee.run(%{
      "secp256k1-compress" => fn ->
        ExSecp256k1.Impl.verify(msg, signature, pk2)
      end,
      "secp256k1" => fn ->
        ExSecp256k1.Impl.verify(msg, signature, pk)
      end
    })
  end
end
