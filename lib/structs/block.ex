defmodule Ippan.Block do
  @type t :: %__MODULE__{
          height: non_neg_integer(),
          creator: non_neg_integer(),
          round: non_neg_integer(),
          hash: binary(),
          hashfile: binary() | nil,
          prev: binary() | nil,
          signature: binary(),
          timestamp: non_neg_integer(),
          count: non_neg_integer(),
          size: non_neg_integer(),
          error: boolean(),
          vsn: integer()
        }

  @file_extension "mpk"
  defstruct [
    :height,
    :creator,
    :round,
    :hash,
    :hashfile,
    :prev,
    :signature,
    :timestamp,
    count: 0,
    size: 0,
    error: false,
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
      x.count,
      x.size,
      x.error
    ]
  end

  def to_tuple(x) do
    {{x.creator, x.height}, x.hash, x.prev, x.hashfile, x.signature, x.round, x.timestamp,
     x.count, x.size, x.error}
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
        count,
        size,
        error
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
      count: count,
      error: error,
      size: size
    }
  end

  def to_map(
        {{creator, height}, hash, prev, hashfile, signature, round, timestamp, count, size, error}
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
      count: count,
      error: error,
      size: size
    }
  end

  @spec put_hash(term()) :: term()
  def put_hash(
        block = %{
          creator: creator,
          height: height,
          prev: prev,
          hashfile: hashfile,
          timestamp: timestamp
        }
      ) do
    Map.put(block, :hash, compute_hash(height, creator, prev, hashfile, timestamp))
  end

  @spec put_signature(term()) :: term()
  def put_signature(block) do
    {:ok, sig} = sign(block.hash)
    Map.put(block, :signature, sig)
  end

  @spec compute_hash(integer(), integer(), binary(), binary(), integer()) :: binary
  def compute_hash(height, creator, prev, hashfile, timestamp) do
    [
      to_string(creator),
      to_string(height),
      # to_string(round),
      normalize(prev),
      normalize(hashfile),
      to_string(timestamp)
    ]
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end

  @spec sign(binary()) :: {:ok, binary()} | {:error, term()}
  def sign(hash) do
    privkey = :persistent_term.get(:privkey)
    Cafezinho.Impl.sign(hash, privkey)
  end

  def sign_block_confirm(hash) do
    privkey = :persistent_term.get(:privkey)
    Cafezinho.Impl.sign("#{hash} is valid", privkey)
  end

  def block_path(validator_id, height) do
    block_dir = Application.get_env(:ipnworker, :block_dir)
    Path.join([block_dir, "#{validator_id}.#{height}.#{@file_extension}"])
  end

  def decode_path(validator_id, height) do
    decode_dir = Application.get_env(:ipnworker, :decode_dir)
    Path.join([decode_dir, "#{validator_id}.#{height}.#{@file_extension}"])
  end

  def url(hostname, creator_id, height) do
    "https://#{hostname}/v1/download/block/#{creator_id}/#{height}"
  end

  def cluster_block_url(hostname, creator_id, height) do
    "http://#{hostname}:8080/v1/download/block/#{creator_id}/#{height}"
  end

  def cluster_decode_url(hostname, creator_id, height) do
    "http://#{hostname}:8080/v1/download/block/decoded/#{creator_id}/#{height}"
  end

  def encode_file!(content) do
    CBOR.Encoder.encode_into(content, <<>>)
  end

  def decode_file!(content) do
    elem(CBOR.Decoder.decode(content), 0)
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
