defmodule Global do
  defmacro miner do
    quote do
      Default.get(:miner)
    end
  end

  defmacro owner do
    quote do
      Default.get(:owner)
    end
  end

  defmacro pubkey do
    quote do
      Default.get(:privkey)
    end
  end

  defmacro privkey do
    quote do
      Default.get(:privkey)
    end
  end

  defmacro has_owner? do
    quote do
      case Default.get(:owner, false) do
        false ->
          false

        _ ->
          true
      end
    end
  end

  defmacro owner?(id) do
    quote do
      Default.get(:owner, nil) == unquote(id)
    end
  end

  defmacro validator_id do
    quote do
      Default.get(:vid)
    end
  end
end
