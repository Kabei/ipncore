defmodule Ippan.Block do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer() | nil,
          creator: non_neg_integer(),
          height: non_neg_integer(),
          round: non_neg_integer() | nil,
          hash: binary(),
          hashfile: binary() | nil,
          prev: binary() | nil,
          signature: binary(),
          timestamp: non_neg_integer(),
          count: non_neg_integer(),
          rejected: non_neg_integer(),
          size: non_neg_integer(),
          vsn: non_neg_integer()
        }

  @block_extension Application.compile_env(:ipncore, :block_extension)
  @decode_extension Application.compile_env(:ipncore, :decode_extension)
  @hash_module Blake3.Native

  defstruct [
    :id,
    :creator,
    :height,
    :round,
    :hash,
    :hashfile,
    :prev,
    :signature,
    :timestamp,
    count: 0,
    rejected: 0,
    size: 0,
    vsn: 0
  ]

  @spec fields :: [binary()]
  def fields do
    ~w(id creator height round hash hashfile prev signature timestamp count rejected size vsn)
  end

  @impl true
  def to_list(x) do
    [
      x.id,
      x.creator,
      x.height,
      x.hash,
      x.prev,
      x.hashfile,
      x.signature,
      x.round,
      x.timestamp,
      x.count,
      x.rejected,
      x.size,
      x.vsn
    ]
  end

  @impl true
  def list_to_tuple([id | _] = x) do
    {id, list_to_map(x)}
  end

  @impl true
  def to_tuple(x) do
    {x.id, x}
  end

  @impl true
  def list_to_map([
        id,
        creator,
        height,
        hash,
        prev,
        hashfile,
        signature,
        round,
        timestamp,
        count,
        rejected,
        size,
        vsn
      ]) do
    %{
      id: id,
      height: height,
      creator: creator,
      prev: prev,
      hash: hash,
      hashfile: hashfile,
      signature: signature,
      timestamp: timestamp,
      round: round,
      count: count,
      rejected: rejected,
      size: size,
      vsn: vsn
    }
  end

  @impl true
  def to_map({_id, x}), do: x

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
    Map.put(block, :hash, compute_hash(creator, height, prev, hashfile, timestamp))
  end

  @spec put_signature(term()) :: term()
  def put_signature(block) do
    {:ok, sig} = sign(block.hash)
    Map.put(block, :signature, sig)
  end

  @spec compute_hash(integer(), integer(), binary(), binary(), integer()) :: binary
  def compute_hash(creator, height, prev, hashfile, timestamp) do
    [
      to_string(creator),
      to_string(height),
      normalize(prev),
      normalize(hashfile),
      to_string(timestamp)
    ]
    |> IO.iodata_to_binary()
    |> @hash_module.hash()
  end

  @spec sign(binary()) :: {:ok, binary()} | {:error, term()}
  def sign(hash) do
    privkey = :persistent_term.get(:privkey)
    Cafezinho.Impl.sign(hash, privkey)
  end

  def hashes_and_count_txs_and_size(blocks) do
    Enum.reduce(blocks, {[], 0, 0}, fn x, {acc_hash, acc_tx, acc_size} ->
      {
        acc_hash ++ [x.hash],
        acc_tx + x.count,
        acc_size + x.size
      }
    end)
  end

  def sign_block_confirm(hash) do
    privkey = :persistent_term.get(:privkey)
    Cafezinho.Impl.sign("#{hash} is valid", privkey)
  end

  def block_path(validator_id, height) do
    block_dir = :persistent_term.get(:block_dir)
    Path.join([block_dir, "#{validator_id}.#{height}.#{@block_extension}"])
  end

  def decode_path(validator_id, height) do
    decode_dir = :persistent_term.get(:decode_dir)
    Path.join([decode_dir, "#{validator_id}.#{height}.#{@decode_extension}"])
  end

  def url(hostname, creator_id, height) do
    "https://#{hostname}/v1/dl/block/#{creator_id}/#{height}"
  end

  def cluster_block_url(hostname, creator_id, height) do
    port = Application.get_env(:ipncore, :http)[:port]
    "http://#{hostname}:#{port}/v1/dl/block/#{creator_id}/#{height}"
  end

  def cluster_decode_url(hostname, creator_id, height) do
    port = Application.get_env(:ipncore, :http)[:port]
    "http://#{hostname}:#{port}/v1/dl/decode/#{creator_id}/#{height}"
  end

  def encode_file!(content) do
    CBOR.Encoder.encode_into(content, <<>>)
  end

  def decode_file!(content) do
    elem(CBOR.Decoder.decode(content), 0)
  end

  def hash_file(path) do
    state = @hash_module.new()

    File.stream!(path, [], 2048)
    |> Enum.reduce(state, &@hash_module.update(&2, &1))
    |> @hash_module.finalize()
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x

  defmacro get(id) do
    quote bind_quoted: [id: id], location: :keep do
      Sqlite.fetch("get_block", [id])
    end
  end

  defmacro exists?(id) do
    quote bind_quoted: [id: id], location: :keep do
      Sqlite.exists?("exists_block", [id])
    end
  end

  defmacro exists_local?(creator_id, height) do
    quote bind_quoted: [creator_id: creator_id, height: height], location: :keep do
      Sqlite.exists?("exists_local_block", [creator_id, height])
    end
  end

  defmacro last_created(creator_id, default) do
    quote bind_quoted: [id: creator_id, default: default], location: :keep do
      Sqlite.fetch("last_block_created", [id], default)
    end
  end

  defmacro last_id do
    quote location: :keep do
      Sqlite.one("last_block_id", [], 0)
    end
  end

  defmacro total_created(create_id) do
    quote location: :keep do
      Sqlite.one("total_blocks_created", [unquote(create_id)], 0)
    end
  end

  defmacro insert(args) do
    quote location: :keep do
      Sqlite.step("insert_block", unquote(args))
    end
  end
end
