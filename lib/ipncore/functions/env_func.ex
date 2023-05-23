defmodule Ippan.Func.Env do
  def set(%{account: account, timestamp: timestamp}, name, value)
      when byte_size(name) <= 256 and byte_size(value) <= 4096 do
    if Platform.owner?(account.id) do
      EnvStore.insert([name, value, timestamp])
    else
      raise IppanError, "Invalid operation"
    end
  end

  def delete(%{account: account}, name) do
    if Platform.owner?(account.id) do
      EnvStore.delete(name)
    else
      raise IppanError, "Invalid operation"
    end
  end
end
