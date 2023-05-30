defmodule Ippan.Func.Env do
  def set(%{id: account_id, timestamp: timestamp}, name, value)
      when byte_size(name) <= 256 and byte_size(value) <= 4096 do
    if Platform.owner?(account_id) do
      EnvStore.insert([name, :erlang.term_to_binary(value), timestamp])
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
