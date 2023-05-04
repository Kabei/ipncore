defmodule Ippan.Round do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          blocks: non_neg_integer(),
          timestamp: non_neg_integer()
        }

  defstruct [:id, :blocks, :timestamp]

  def to_list(x) do
    [x.id, x.blocks, x.timestamp]
  end

  def to_tuple(x) do
    {x.id, x.blocks, x.timestamp}
  end

  def to_map({id, blocks, timestamp}) do
    %{id: id, blocks: blocks, timestamp: timestamp}
  end

  def to_map([id, blocks, timestamp]) do
    %{id: id, blocks: blocks, timestamp: timestamp}
  end
end
