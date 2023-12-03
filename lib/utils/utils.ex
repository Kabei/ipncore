defmodule Ippan.Utils do
  @compile :inline_list_funcs
  @compile {:inline, [encode16: 1, encode64: 1]}

  @app Mix.Project.config()[:app]
  @reserve Application.compile_env(@app, :reserve, 0.3)

  def empty?(nil), do: true
  def empty?(<<>>), do: true
  def empty?([]), do: true
  def empty?(x) when map_size(x) == 0, do: true
  def empty?(_), do: false

  def to_atom(nil), do: nil
  def to_atom(text), do: String.to_atom(text)

  def cast_boolean("true"), do: true
  def cast_boolean("TRUE"), do: true
  def cast_boolean("1"), do: true
  def cast_boolean(1), do: true
  def cast_boolean(true), do: true
  def cast_boolean(_), do: false

  def check_integer(x, default) do
    case Regex.match?(~r/^[0-9]*$/, x) do
      true -> x
      _ -> default
    end
  end

  def sqlite_in(values) do
    Enum.reduce(values, "(", fn
      x, acc when is_binary(x) -> IO.iodata_to_binary([acc, x, ","])
      x, acc -> IO.iodata_to_binary([acc, to_string(x), ","])
    end)
    |> String.replace_trailing(",", ")")
  end

  @spec rows_to_columns(map() | Keyword.t()) :: {list(), list()}
  def rows_to_columns(map_or_kw) do
    result =
      cond do
        is_map(map_or_kw) ->
          Map.to_list(map_or_kw)

        is_list(map_or_kw) ->
          Keyword.to_list(map_or_kw)
      end
      |> Enum.reduce(%{keys: [], values: []}, fn {key, value}, acc ->
        %{keys: [key | acc.keys], values: [value | acc.values]}
      end)

    keys = Enum.reverse(result.keys)
    values = Enum.reverse(result.values)

    {keys, values}
  end

  @spec calc_fees(a :: integer(), b :: integer(), size :: pos_integer()) :: integer()
  def calc_fees(0, 0, _size), do: 1
  def calc_fees(a, b, size), do: a * size + b

  @spec calc_reserve(integer()) :: integer()
  def calc_reserve(fees), do: trunc(fees * @reserve)

  def get_name_from_node(node_name) do
    node_name |> to_string() |> String.split("@") |> hd
  end

  def my_ip do
    node() |> to_string() |> String.split("@") |> List.last()
  end

  def get_random_node_verifier do
    Node.list() |> Enum.random() |> to_string() |> String.split("@") |> hd
  end

  def delete_oldest_file(dir) do
    dir
    |> Path.expand()
    |> File.ls!()
    |> Enum.sort_by(&File.stat!(&1).mtime)
    |> List.first()
    |> File.rm!()
  end

  def delete_files(dir, timestamp) do
    dir
    |> Path.expand()
    |> File.ls!()
    |> Enum.filter(&(File.stat!(&1).mtime < timestamp))
    |> Enum.each(fn path ->
      File.rm(path)
    end)
  end

  def getaddr(hostname) do
    cond do
      Match.host_or_ipv4?(hostname) ->
        :inet_udp.getaddr(String.to_charlist(hostname))

      true ->
        :inet6_udp.getaddr(String.to_charlist(hostname))
    end
  end

  defmacro json(data) do
    quote bind_quoted: [data: data] do
      var!(conn)
      |> put_resp_content_type("application/json")
      |> send_resp(200, Jason.encode!(data))
    end
  end

  defmacro send_json(data) do
    quote bind_quoted: [data: data] do
      case data do
        nil ->
          send_resp(var!(conn), 204, "")

        [] ->
          send_resp(var!(conn), 204, "")

        _ ->
          var!(conn)
          |> put_resp_content_type("application/json")
          |> send_resp(200, Jason.encode!(data))
      end
    end
  end

  def fetch_query(%{query_string: query_string}) do
    Plug.Conn.Query.decode(
      query_string,
      %{},
      Plug.Conn.InvalidQueryError,
      true
    )
  end

  @spec time_threshold(non_neg_integer(), non_neg_integer()) :: boolean()
  def time_threshold(time, interval) do
    now = :erlang.system_time(:millisecond)

    time in Range.new(now - interval, now + interval)
  end

  def date_start_to_time(date, unit_time \\ :millisecond) do
    d = DateTime.from_iso8601(date)

    case d do
      {:error, _} -> 0
      {:ok, dt, _} -> DateTime.to_unix(dt, unit_time)
    end
  end

  def date_end_to_time(date, unit_time \\ :millisecond) do
    d = DateTime.from_iso8601(date)

    case d do
      {:error, _} -> 0
      {:ok, dt, _} -> DateTime.to_unix(dt, unit_time)
    end
  end

  def encode16(nil), do: nil
  def encode16(x), do: Base.encode16(x)

  def encode64(nil), do: nil
  def encode64(x), do: Fast64.encode64(x)
end
