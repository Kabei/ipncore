defmodule Ippan.Func.Env do
  require Global

  def pre_set(%{id: account_id, hash: hash, round: round, timestamp: timestamp}, name, value)
      when byte_size(name) <= 256 do
    bin = :erlang.term_to_binary(value)

    if Global.owner?(account_id) and byte_size(bin) <= 4096 do
      MessageStore.approve_df(round, timestamp, hash)
    else
      raise IppanError, "Invalid operation"
    end
  end

  def set(%{timestamp: timestamp}, name, value) do
    bin = :erlang.term_to_binary(value)
    EnvStore.insert([name, bin, timestamp])
  end

  def pre_delete(%{id: account_id, hash: hash, round: round, timestamp: timestamp}, name)
      when byte_size(name) <= 256 do
    if Global.owner?(account_id) do
      MessageStore.approve_df(round, timestamp, hash)
    else
      raise IppanError, "Invalid operation"
    end
  end

  def delete(_source, name) do
    EnvStore.delete(name)
  end
end
