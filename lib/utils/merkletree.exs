defmodule MerkleTree do
  @spec root([binary]) :: binary
  def root(chunks) do
    chunks
    |> build_tree()
    |> Enum.at(0)
  end

  @spec build_tree([binary], [binary]) :: [binary]
  defp build_tree(leaves, heap \\ [])
  defp build_tree([root], heap), do: [root | heap]

  defp build_tree(leaves, heap) do
    leaves
    |> Enum.chunk_every(2)
    |> Enum.reduce([], &(&2 ++ [concatenate_and_hash(&1)]))
    |> build_tree(leaves ++ heap)
  end

  @spec concatenate_and_hash([binary]) :: binary
  defp concatenate_and_hash([h]), do: concatenate_and_hash([h, ""])

  defp concatenate_and_hash(io_list) do
    Blake3.hash(io_list)
  end
end
