defmodule Ippan.MsgRound do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          creator: non_neg_integer(),
          hash: binary(),
          messages: [map()],
          signature: binary()
        }

  defstruct [:id, :creator, :messages, :hash, :signature]

  def encode(%{
        "id" => id,
        "creator" => creator,
        "messages" => messages,
        "hash" => hash,
        "signature" => signature
      }) do
    [
      id,
      creator,
      CBOR.encode(messages),
      hash,
      signature
    ]
  end

  def decode([id, creator, messages, hash, signature]) do
    %{
      "id" => id,
      "creator" => creator,
      "messages" => elem(CBOR.decode(messages), 1),
      "hash" => hash,
      "signature" => signature
    }
  end
end
