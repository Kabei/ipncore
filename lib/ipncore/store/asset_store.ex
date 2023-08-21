# defmodule MsgStore do
#   use GenServer

#   def start_link(name) do
#     GenServer.start_link(__MODULE__, name, name: __MODULE__)
#   end

#   @impl true
#   def init(args) do
#     {:ok, args}
#   end

#   @impl true
#   def handle_call(:sync, _from, state) do
#     {:reply, :ok, state}
#   end
# end

# defmodule AssetStore do
#   require SqliteStore

#   @creation %{
#     balance: SQL.readFile!("lib/sql/balance.sql"),
#     block: SQL.readFile!("lib/sql/block.sql"),
#     domain: SQL.readFile!("lib/sql/domain.sql"),
#     dns: SQL.readFile!("lib/sql/dns.sql"),
#     env: SQL.readFile!("lib/sql/env.sql"),
#     token: SQL.readFile!("lib/sql/token.sql"),
#     wallet: SQL.readFile!("lib/sql/wallet.sql")
#   }

#   @statments %{
#     balance: SQL.readFileStmt!("lib/sql/balance.stmt.sql"),
#     block: SQL.readFileStmt!("lib/sql/block.stmt.sql"),
#     domain: SQL.readFileStmt!("lib/sql/domain.stmt.sql"),
#     dns: SQL.readFileStmt!("lib/sql/dns.stmt.sql"),
#     env: SQL.readFileStmt!("lib/sql/token.stmt.sql"),
#     # payments: SQL.readFileStmt!("lib/sql/payments.stmt.sql"),
#     token: SQL.readFileStmt!("lib/sql/token.stmt.sql"),
#     wallet: SQL.readFileStmt!("lib/sql/wallet.stmt.sql")
#   }

#   @attaches %{
#     "balance" => "balance.db",
#     "block" => "block.db",
#     "domain" => "domain.db",
#     "dns" => "dns.db",
#     "env" => "env.db",
#     # "payments" => "payments.db",
#     "token" => "token.db",
#     "wallet" => "wallet.db"
#   }

#   def init do
#     SqliteStore.check_version()
#   end
# end

# defmodule GenStore do
#   use GenServer

#   def start_link(name) do
#     GenServer.start_link(__MODULE__, name, name: __MODULE__)
#   end

#   @impl true
#   def init(args) do
#     {:ok, args}
#   end

#   @impl true
#   def handle_call(:sync, _from, state) do

#     {:reply, :ok, satte}
#   end
# end
