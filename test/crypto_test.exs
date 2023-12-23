defmodule CryptoTest do
  use ExUnit.Case
  doctest Ipncore

  test "verify" do
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
      "falcon-512" => fn ->
        Falcon.verify(msg, sig, pub)
      end
    })
  end

  test "ed25519" do
    message = :rand.bytes(32)
    {pk, sk} = Cafezinho.Impl.generate()
    {:ok, sig} = Cafezinho.Impl.sign(message, sk)

    seed = :rand.bytes(32)
    {:ok, pk2} = Ed25519Blake2b.Native.derive_public_key(seed)
    {:ok, sig2} = Ed25519Blake2b.Native.sign(seed <> pk2, message)

    sig3 = :libdecaf_curve25519.ed25519_sign(message, sk)

    sig4 = :libdecaf_curve25519.ed25519ctx_sign(message, sk)

    sig5 = :libdecaf_curve25519.ed25519ph_sign(message, sk)

    Benchee.run(%{
      "cafezinho" => fn ->
        Cafezinho.Impl.verify(sig, message, pk)
      end
      # "ed25519-blake2b" => fn ->
      #   Ed25519Blake2b.Native.verify(message, sig2, pk2)
      # end,
      # "libdecaf-ed25519" => fn ->
      #   :libdecaf_curve25519.ed25519_verify(sig3, message, pk)
      # end,
      # "libdecaf-ed25519ctx" => fn ->
      #   :libdecaf_curve25519.ed25519ctx_verify(sig4, message, pk)
      # end,
      # "libdecaf-ed25519ph" => fn ->
      #   :libdecaf_curve25519.ed25519ph_verify(sig5, message, pk)
      # end
    })
  end
end
