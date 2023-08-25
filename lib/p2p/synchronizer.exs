defmodule Synchronizer do
  alias Ippan.Block
  alias Ippan.P2P
  use GenServer

  def init(my_state, %{round: my_round} = last_block) do
    {:ok, validators} = ValidatorStore.all()

    node = Application.get_env(:ipncore, :sync_host)
    |> Enum.random()



    nodes =
      validators
      |> Enum.take_random(10)

    block =
      BlockStore.last()
      |> Block.to_map()

    states =
      for node <- nodes do
        case P2P.request(node.id, %{"method" => "get_state"}, 10_000, 1) do
          {:ok, response} ->
            response

          _ ->
            :error
        end
      end
      |> Enum.filter(fn
        %{"status" => "ok"} = _msg -> true
        _ -> false
      end)
      |> Enum.map(fn %{"body" => body} -> body end)

    {fkey, _} =
      states
      |> Enum.filter(fn x -> x["round"] > my_round)
      |> Enum.frequencies_by(fn x -> {x["round"], x["hash"]} end)
      |> Enum.max_by(&elem(&1, 1))

    filter_states =
      filter_states
      |> Enum.filter(&({&1["round"], &1["hash"]} == fkey))
      |> List.first()

    build(filter_states, block)
  end

  def build(%{"round" => round} = state, last_block) do
    last_round = last_block.round

    case P2P.request(node_id, %{"method" => "get_bft"}, 10_000, 1) do
      {:ok, %{"body" => response} = _reqeuest} ->
        for msg <- response do
          send(VoteCounter, {"new_recv", msg})
        end

        _ -> :ok
    end
  end
end
