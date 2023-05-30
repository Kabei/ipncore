defmodule CryptoTest do
  use ExUnit.Case
  doctest Ipncore

  test "verify" do
    sk = :crypto.strong_rand_bytes(32)
    lib = ExSecp256k1.Impl
    {:ok, pk} = lib.create_public_key(sk)
    # {:ok, pk2} = lib.public_key_compress(pk)

    msg = :crypto.strong_rand_bytes(32)
    {:ok, {signature, _recovery_id_int}} = lib.sign_compact(msg, sk)

    Benchee.run(%{
      # "secp256k1-compress" => fn ->
      #   ExSecp256k1.Impl.verify(msg, signature, pk2)
      # end,
      "secp256k1" => fn ->
        ExSecp256k1.Impl.verify(msg, signature, pk)
      end
    })
  end

  test "curvy" do
    key = Curvy.generate_key()
    msg = :crypto.strong_rand_bytes(32)
    sig = Curvy.sign(msg, key, compact: true)

    Benchee.run(%{
      "curvy" => fn ->
        Curvy.verify(sig, msg, key)
      end
    })
  end

  test "startbank_ecdsa" do
    privateKey = EllipticCurve.PrivateKey.generate()
    publicKey = EllipticCurve.PrivateKey.getPublicKey(privateKey)

    message = :crypto.strong_rand_bytes(32)

    # Generate Signature
    signature = EllipticCurve.Ecdsa.sign(message, privateKey)

    Benchee.run(%{
      "start-bank" => fn ->
        EllipticCurve.Ecdsa.verify?(message, signature, publicKey)
      end
    })
  end

  test "falcon" do
    seed = :crypto.strong_rand_bytes(48)
    msg = :crypto.strong_rand_bytes(32)
    {:ok, pub, sk} = Falcon.gen_keys_from_seed(seed)
    {:ok, sig} = Falcon.sign(sk, msg)

    Benchee.run(%{
      "falcon" => fn ->
        Falcon.verify(msg, sig, pub)
      end
    })
  end
end
