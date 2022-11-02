defmodule DetsPlus.Ext do
  defmacro get_handle(pid) do
    quote do
      GenServer.call(unquote(pid), :get_handle)
    end
  end

  def filter(pid, fun) when is_pid(pid) or is_atom(pid) do
    Enum.filter(get_handle(pid), fun)
  end

  def reduce_while(pid, acc, fun) when is_pid(pid) or is_atom(pid) do
    Enum.reduce_while(get_handle(pid), acc, fun)
  end

  def get_multi(pid, items) when is_pid(pid) or is_atom(pid) do
    Enum.reduce_while(get_handle(pid), fn {k, _v} = x, {keys, acc} = accs ->
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
    get_handle(pid)
    |> Enum.reduce_while([], fn {k, _v}, keys ->
      case k in list do
        true ->
          keys = keys ++ [x]

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
