defmodule CryptoTest do
  use ExUnit.Case
  doctest Ipncore

  # test "verify" do
  #   sk = :crypto.strong_rand_bytes(32)
  #   lib = ExSecp256k1.Impl
  #   {:ok, pk} = lib.create_public_key(sk)
  #   # {:ok, pk2} = lib.public_key_compress(pk)

  #   msg = :crypto.strong_rand_bytes(32)
  #   {:ok, {signature, _recovery_id_int}} = lib.sign_compact(msg, sk)

  #   Benchee.run(%{
  #     # "secp256k1-compress" => fn ->
  #     #   ExSecp256k1.Impl.verify(msg, signature, pk2)
  #     # end,
  #     "secp256k1" => fn ->
  #       ExSecp256k1.Impl.verify(msg, signature, pk)
  #     end
  #   })
  # end

  # test "curvy" do
  #   key = Curvy.generate_key()
  #   msg = :crypto.strong_rand_bytes(32)
  #   sig = Curvy.sign(msg, key, compact: true)

  #   Benchee.run(%{
  #     "curvy" => fn ->
  #       Curvy.verify(sig, msg, key)
  #     end
  #   })
  # end

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

  test "libdecaf" do
    message = :rand.bytes(32)

    {pk, sk} = :libdecaf_curve25519.eddsa_keypair()

    # :libdecaf_curve25519.ed25519_keypair_sign()
    sig = :libdecaf_curve25519.ed25519_sign(message, sk)

    # <<206,59,70,86,114,42,116,11,30,183,149,16,90,90,105,162,112,182,99,62,90,20,207,102,102,98,230,18,23,175,212,150,146,27,83,120,107,135,209,56,75,214,152,204,24,205,48,128,129,191,174,137,11,189,75,14,7,242,3,255,122,176,182,2>>

    sig2 = :libdecaf_curve25519.ed25519ctx_sign(message, sk)

    sig3 = :libdecaf_curve25519.ed25519ph_sign(message, sk)

    # Ed25519 Verify
    Benchee.run(%{
      "libdecaf-ed25519" => fn ->
        :libdecaf_curve25519.ed25519_verify(sig, message, pk)
      end,
      "libdecaf-ed25519ctx" => fn ->
        :libdecaf_curve25519.ed25519ctx_verify(sig2, message, pk)
      end,
      "libdecaf-ed25519ph" => fn ->
        :libdecaf_curve25519.ed25519ph_verify(sig3, message, pk)
      end
    })
  end

  test "ed25519-blake2b" do
    message = :rand.bytes(32)
    {pk, sk} = Cafezinho.Impl.generate()
    {:ok, sig} = Cafezinho.Impl.sign(message, sk)

    sk2 = :rand.bytes(32)
    {:ok, pk2} = Ed25519Blake2b.Native.derive_public_key(sk2)
    {:ok, sig2} = Ed25519Blake2b.Native.sign(sk2 <> pk2, message)

    Benchee.run(%{
      "cafezinho" => fn ->
        Cafezinho.Impl.verify(sig, message, pk)
      end,
      "ed25519-blake2b" => fn ->
        Ed25519Blake2b.Native.verify(message, sig2, pk2)
      end
    })
  end
end
