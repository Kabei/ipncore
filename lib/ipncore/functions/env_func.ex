defmodule Ippan.Func.Env do
  def set(%{id: account_id, timestamp: timestamp}, name, value)
      when byte_size(name) <= 256 do
    bin = :erlang.term_to_binary(value)

    if Platform.owner?(account_id) and byte_size(bin) <= 4096 do
      EnvStore.insert([name, bin, timestamp])
    else
      raise IppanError, "Invalid operation"
    end
  end

  def delete(%{id: account_id}, name) do
    if Platform.owner?(account_id) do
      EnvStore.delete(name)
    else
      raise IppanError, "Invalid operation"
    end
  end
end
