defmodule Ipncore.Mined do
  alias Ipncore.{Block, Chain}

  @doc """
  Returns an address from the list of participants (validators),
  winner the coinbase of the next block.
  """
  @spec proof_of_remainder(Block.t()) :: binary
  def proof_of_remainder(%Block{} = prev_block) do
    addresses = Chain.participants()
    total = Chain.total_participants()
    # check_control = Chain.hash_participants()

    xhash = prev_block.hash |> :binary.decode_unsigned()

    position = rem(xhash, total)

    Enum.at(addresses, position)
  end
end
