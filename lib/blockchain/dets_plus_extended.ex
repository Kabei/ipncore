defmodule DetsPlus.Ext do
  defmacro handle(pid) do
    quote do
      GenServer.call(unquote(pid), :get_handle, 10_000)
    end
  end

  def filter(pid, fun) do
    Enum.filter(handle(pid), fun)
  end

  def reduce_while(pid, acc, fun) do
    Enum.reduce_while(handle(pid), acc, fun)
  end

  def get_multi(pid, items) do
    Enum.reduce_while(handle(pid), {[], []}, fn {k, _v} = x, {keys, acc} = accs ->
      case k in items do
        true ->
          keys = keys ++ [k]
          acc = acc ++ [x]

          if keys == items do
            {:halt, {keys, acc}}
          else
            {:cont, {keys, acc}}
          end

        false ->
          {:cont, accs}
      end
    end)
  end

  def all?(pid, items) do
    handle(pid)
    |> Enum.reduce_while([], fn {k, _v}, keys ->
      case k in items do
        true ->
          keys = keys ++ [k]

          if keys == items do
            {:halt, true}
          else
            {:cont, keys}
          end

        false ->
          {:cont, keys}
      end
    end)
  end
end
