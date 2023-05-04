defmodule Ippan.Block do
  @type t :: %__MODULE__{
          height: non_neg_integer(),
          prev: binary(),
          hash: binary(),
          hashfile: binary(),
          time: non_neg_integer(),
          ev_count: non_neg_integer(),
          vsn: non_neg_integer()
        }
  defstruct [:height, :prev, :hash, :hashfile, :time, vsn: 0, ev_count: 0]

  def to_list(x) do
    [
      x.height,
      x.prev,
      x.hash,
      x.hashfile,
      x.time,
      x.vsn,
      x.ev_count
    ]
  end

  def to_tuple(x) do
    {x.height, x.prev, x.hash, x.hashfile, x.time, x.ev_count, x.vsn}
  end

  def to_map({height, prev, hash, hashfile, time, ev_count, vsn}) do
    %{
      height: height,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      time: time,
      ev_count: ev_count,
      vsn: vsn
    }
  end

  def to_map([height, prev, hash, hashfile, time, ev_count, vsn]) do
    %{
      height: height,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      time: time,
      ev_count: ev_count,
      vsn: vsn
    }
  end
end
