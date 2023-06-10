defmodule Ippan.Block do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          height: non_neg_integer(),
          validator: integer(),
          prev: binary(),
          hash: binary(),
          hashfile: binary(),
          round: integer(),
          time: non_neg_integer(),
          ev_count: non_neg_integer(),
          vsn: non_neg_integer(),
          size: non_neg_integer()
        }

  defstruct [
    :id,
    :height,
    :validator,
    :prev,
    :hash,
    :hashfile,
    :round,
    :time,
    vsn: 0,
    ev_count: 0,
    size: 0
  ]

  def to_list(x) do
    [
      x.id,
      x.height,
      x.validator,
      x.prev,
      x.hash,
      x.hashfile,
      x.round,
      x.time,
      x.vsn,
      x.ev_count,
      x.size
    ]
  end

  def to_tuple(x) do
    {x.id, x.height, x.prev, x.hash, x.hashfile, x.time, x.ev_count, x.vsn, x.size}
  end

  def to_map({id, height, validator, prev, hash, hashfile, round, time, ev_count, vsn, size}) do
    %{
      id: id,
      height: height,
      validator: validator,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      round: round,
      time: time,
      ev_count: ev_count,
      vsn: vsn,
      size: size
    }
  end

  def to_map([id, height, validator, prev, hash, hashfile, round, time, ev_count, size, vsn]) do
    %{
      id: id,
      height: height,
      validator: validator,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      time: time,
      round: round,
      ev_count: ev_count,
      vsn: vsn,
      size: size
    }
  end
end
