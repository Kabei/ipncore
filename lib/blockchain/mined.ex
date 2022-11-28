defmodule Ipncore.Mined do
  alias Ipncore.{Block, Repo, Txo}
  import Ecto.Query, only: [from: 2]

  @output_type_fee "%"

  @doc """
  Returns an address from the list of participants (validators),
  winner the coinbase of the next block.
  """
  @spec proof_of_remainder(Block.t(), binary) :: binary
  def proof_of_remainder(%Block{} = prev_block, channel) do
    bindex = :binary.encode_unsigned(prev_block.hash)

    addresses =
      from(txo in Txo,
        where:
          txo.type == @output_type_fee and
            fragment("substring(?::bytea from 1 for ?)", txo.id, ^byte_size(bindex)) == ^bindex,
        select: txo.address,
        distinct: true,
        order_by: [asc: fragment("length(?)", txo.address), asc: txo.address]
      )
      |> Repo.all(prefix: channel)

    total = length(addresses)

    xhash = prev_block.hash |> :binary.decode_unsigned()

    position = rem(xhash, total)

    Enum.at(addresses, position)
  end
end
