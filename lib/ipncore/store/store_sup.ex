defmodule Ippan.StoreSupervisor do
  use Supervisor

  def start_link(data_dir) do
    Supervisor.start_link(__MODULE__, data_dir, name: __MODULE__)
  end

  @impl true
  def init(data_dir) do
    children =
      [
        {MessageStore, Path.join(data_dir, "store/priv/messages.db")},
        {WalletStore, Path.join(data_dir, "store/wallet.db")},
        {EnvStore, Path.join(data_dir, "store/env.db")},
        {ValidatorStore, Path.join(data_dir, "store/validator.db")},
        {TokenStore, Path.join(data_dir, "store/token.db")},
        {BalanceStore, Path.join(data_dir, "store/balance.db")},
        {RefundStore, Path.join(data_dir, "store/refund.db")},
        {DomainStore, Path.join(data_dir, "store/domain.db")},
        {DnsStore, Path.join(data_dir, "store/dns.db")},
        {BlockStore, Path.join(data_dir, "store/block.db")},
        {RoundStore, Path.join(data_dir, "store/round.db")}
      ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
