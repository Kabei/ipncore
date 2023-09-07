defmodule Ippan.NetworkSup do
  use DynamicSupervisor
  alias IO.ANSI
  @module __MODULE__
  def start_link(children) do
    opts = Application.get_env(:ipncore, :network)

    result =
      case Process.whereis(@module) do
        nil ->
          DynamicSupervisor.start_link(@module, [{ThousandIsland, opts}] ++ children,
            name: @module
          )

        pid ->
          {:ok, pid}
      end

    IO.puts(
      "Running #{ANSI.red()}IPNCORE#{ANSI.reset()} P2P Network with port #{ANSI.yellow()}#{opts[:port]}#{ANSI.reset()}"
    )

    result
  end

  def start_child(args) do
    DynamicSupervisor.start_child(@module, {Ippan.NetworkClient, args})
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one, extra_arguments: [init_arg])
  end
end