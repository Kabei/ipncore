defmodule Global do
  use GlobalConst.DummyModule

  defmacro miner do
    quote do
      Global.get(:miner)
    end
  end

  defmacro owner do
    quote do
      Global.get(:owner)
    end
  end

  def pubkey do
    get(:privkey)
  end

  def privkey do
    get(:privkey)
  end

  def has_owner? do
    case get(:owner, false) do
      false ->
        false

      _ ->
        true
    end
  end

  def owner?(nil), do: false

  def owner?(id) do
    get(:owner, nil) == id
  end

  def validator_id do
    get(:vid)
  end
end
