defmodule Ippan.MsgBlock do
  def encode(
        %{
          "creator" => creator,
          "height" => height
        } = block
      ) do
    [
      creator,
      height,
      CBOR.encode(Map.drop(block, ["creator", "height"]))
    ]
  end

  def decode([creator, height, data]) do
    %{
      "creator" => creator,
      "height" => height
    }
    |> Map.merge(elem(CBOR.decode(data), 1))
  end
end
