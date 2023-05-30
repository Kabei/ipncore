defmodule HashTest do
  use ExUnit.Case
  doctest Ipncore

  # test "map" do
  #   hash = :crypto.strong_rand_bytes(32)

  #   Benchee.run(%{
  #     "raw" => fn ->
  #       %{"hash" => Base.encode16(hash), "asd" => hash}
  #     end,
  #     "map" => fn ->
  #       Map.put(%{}, "hash", Base.encode16(hash))
  #     end
  #   })
  # end

  # test "list_to_tuple" do
  #   data = [
  #     51,
  #     1640_154_148,
  #     [
  #       "MfNh7cwvB9F/CGRfFO+bgd3DmJbzNIIjb6mdSzOKK9wzXtBlLZ/k3JfBZj3EBBhxA+K4AVtAg+32Rs5r8ubjG10TPBfZWmQrRuwLc91OHEpx2mqs5Xs2TxyCOjY7F/Vml8hIDejfUt8oeqWhxXED79CDUZkP4Z7Ac7lAnsViZl8HyUiwOTHF9JHOmsU87ezmcNUay7HgjKKK4Rp3LOL8GlwzbkLK0bax0KOi2lLGPrG5TWwBqKy6yJXG+buuRgzh1AuSgpVLNSyNu60f3se5vQRbPVY+o7cL5IeADBZqOcYaYoEb4W5k21sS1KBSRiWSCV7P/9A6XUYWGBhHShAtTMnLAerc7PW8cyF2kqRwnXHjvJR8ixrr26YTtZdkuWSLjfsHZYmfGHvT6v9/FOjlskRIGTeYDS1hj4rtorFZmlQkiBsBtH/DE8kRYinXGtZK5yAFbu6T4XOy7uelt0goW+7XjBu6vD04Q+dVPpsjdCC4KvbwipuEZtPDTQoXueyfGe/e69lkTaG5R9NvoGzAERCgtqBcYGekvdEXnLVA96BA1f39WX2BjWEG+zKYVg3PLozBDuQTq35Ad2SGk5w4iNLnfKOGzIFIl7X6pDImXoaOzJrLhkOh+FLuzIUlWXjrqSTZDGqqTpnLKqlKxjMUBT11LrQJlGoOCG+etiQcuNSMQpGnCpsAH0EZH4bexSeMPVcBWEvHWxkieGuDtM48jE2/jQtyL2uEpKA6WrVO6saWtnsHdWBSGeTFMXsDCCx8bSfY2BMKo9LD2K3YL4y0a0O3jhdmwWcMEHjA69Cg0c0+qZ/vvjLpudRm8J5AkTSCzA==",
  #       "IPN",
  #       50_000
  #     ]
  #   ]

  #   [a, b, c] = data
  #   d = :crypto.strong_rand_bytes(625)

  #   Benchee.run(%{
  #     "1" => fn ->
  #       List.to_tuple(data)
  #       |> Tuple.append(d)
  #     end,
  #     "2" => fn ->
  #       {a, b, c, d}
  #     end
  #   })
  # end

  test "hashes" do
    data = :crypto.strong_rand_bytes(2000)

    req =
      {50, [5, <<45::256>>],
       "MfNh7cwvB9F/CGRfFO+bgd3DmJbzNIIjb6mdSzOKK9wzXtBlLZ/k3JfBZj3EBBhxA+K4AVtAg+32Rs5r8ubjG10TPBfZWmQrRuwLc91OHEpx2mqs5Xs2TxyCOjY7F/Vml8hIDejfUt8oeqWhxXED79CDUZkP4Z7Ac7lAnsViZl8HyUiwOTHF9JHOmsU87ezmcNUay7HgjKKK4Rp3LOL8GlwzbkLK0bax0KOi2lLGPrG5TWwBqKy6yJXG+buuRgzh1AuSgpVLNSyNu60f3se5vQRbPVY+o7cL5IeADBZqOcYaYoEb4W5k21sS1KBSRiWSCV7P/9A6XUYWGBhHShAtTMnLAerc7PW8cyF2kqRwnXHjvJR8ixrr26YTtZdkuWSLjfsHZYmfGHvT6v9/FOjlskRIGTeYDS1hj4rtorFZmlQkiBsBtH/DE8kRYinXGtZK5yAFbu6T4XOy7uelt0goW+7XjBu6vD04Q+dVPpsjdCC4KvbwipuEZtPDTQoXueyfGe/e69lkTaG5R9NvoGzAERCgtqBcYGekvdEXnLVA96BA1f39WX2BjWEG+zKYVg3PLozBDuQTq35Ad2SGk5w4iNLnfKOGzIFIl7X6pDImXoaOzJrLhkOh+FLuzIUlWXjrqSTZDGqqTpnLKqlKxjMUBT11LrQJlGoOCG+etiQcuNSMQpGnCpsAH0EZH4bexSeMPVcBWEvHWxkieGuDtM48jE2/jQtyL2uEpKA6WrVO6saWtnsHdWBSGeTFMXsDCCx8bSfY2BMKo9LD2K3YL4y0a0O3jhdmwWcMEHjA69Cg0c0+qZ/vvjLpudRm8J5AkTSCzA=="}

    Benchee.run(%{
      "blake3" => fn ->
        Blake3.hash(data)
      end,
      "blake3-2" => fn ->
        {type, arg, sig} = req

        Enum.reduce(arg, "", fn x, acc -> :binary.list_to_bin([acc, "#{x}"]) end)

        Blake3.hash("#{type}#{arg}#{Base.decode64!(sig)}")
      end
    })
  end

  # test "base64" do
  #   a = Base.encode16(:crypto.strong_rand_bytes(897))
  #   b = Base.encode16(:crypto.strong_rand_bytes(32))
  #   c = Base.encode16(:crypto.strong_rand_bytes(32))
  #   data = a <> b <> c

  #   Benchee.run(%{
  #     "separate" => fn ->
  #       Base.decode16!(a)
  #       Base.decode16!(b)
  #       Base.decode16!(c)
  #     end,
  #     "one" => fn ->
  #       x = Base.decode16!(data)
  #       <<_a1::bytes-size(897), _::bytes-size(32), _::bytes-size(32)>> = x
  #     end
  #   })
  # end
end
