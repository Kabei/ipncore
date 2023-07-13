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
      normalize(block.prev),
      block.hashfile,
      to_string(block.timestamp)
    ]
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end

  @spec sign(binary()) :: {:ok, binary()} | {:error, term()}
  def sign(hash) do
    privkey = Application.get_env(:ipncore, :privkey)
    Cafezinho.Impl.sign(hash, privkey)
  end

  def sign_vote(hash, vote) do
    privkey = Application.get_env(:ipncore, :privkey)
    Cafezinho.Impl.sign("#{hash}#{vote}", privkey)
  end

  @file_extension "erl"
  def block_path(validator_id, height) do
    block_dir = Application.get_env(:ipncore, :block_dir)
    Path.join([block_dir, "#{validator_id}.#{height}.#{@file_extension}"])
  end

  def decode_path(validator_id, height) do
    decode_dir = Application.get_env(:ipncore, :decode_dir)
    Path.join([decode_dir, "#{validator_id}.#{height}.#{@file_extension}"])
  end

  def url(hostname, creator_id, height) do
    "https://#{hostname}/v1/download/block/#{creator_id}/#{height}"
  end

  def encode!(content) do
    :erlang.term_to_binary(content)
  end

  def decode!(content) do
    :erlang.binary_to_term(content, [:safe])
  end

  @hash_module Blake3.Native
  def hash_file(path) do
    state = @hash_module.new()

    File.stream!(path, [], 2048)
    |> Enum.reduce(state, &@hash_module.update(&2, &1))
    |> @hash_module.finalize()
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x
end
