defmodule Ippan.Block do
  @type t :: %__MODULE__{
          height: non_neg_integer(),
          creator: integer(),
          prev: binary(),
          hash: binary(),
          hashfile: binary(),
          signature: binary(),
          round: integer(),
          timestamp: non_neg_integer(),
          ev_count: non_neg_integer(),
          vsn: non_neg_integer(),
          size: non_neg_integer()
        }

  defstruct [
    :height,
    :creator,
    :hash,
    :prev,
    :hashfile,
    :signature,
    :round,
    :timestamp,
    ev_count: 0,
    size: 0,
    vsn: 0
  ]

  def to_list(x) do
    [
      x.height,
      x.creator,
      x.hash,
      x.prev,
      x.hashfile,
      x.signature,
      x.round,
      x.timestamp,
      x.ev_count,
      x.size,
      x.vsn
    ]
  end

  def to_tuple(x) do
    {x.height, x.hash, x.prev, x.hashfile, x.signature, x.timestamp, x.ev_count, x.size, x.vsn}
  end

  def to_map(
        {height, creator, hash, prev, hashfile, signature, round, timestamp, ev_count, size, vsn}
      ) do
    %{
      height: height,
      creator: creator,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      signature: signature,
      round: round,
      timestamp: timestamp,
      ev_count: ev_count,
      vsn: vsn,
      size: size
    }
  end

  def to_map([
        height,
        creator,
        hash,
        prev,
        hashfile,
        signature,
        round,
        timestamp,
        ev_count,
        size,
        vsn
      ]) do
    %{
      height: height,
      creator: creator,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      signature: signature,
      timestamp: timestamp,
      round: round,
      ev_count: ev_count,
      vsn: vsn,
      size: size
    }
  end

  @spec compute_hash(term) :: binary
  def compute_hash(block) do
    [
      to_string(block.height),
      to_string(block.creator),
      block.prev,
      block.hashfile,
      to_string(block.timestamp)
    ]
    |> Enum.filter(fn x -> not is_nil(x) end)
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end
end
