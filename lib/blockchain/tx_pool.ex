defmodule TxPool do
    use Ipnutils.CubDB, bucket: "txpool", bucket_type: :fs
  end