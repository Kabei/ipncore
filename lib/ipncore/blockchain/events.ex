defmodule Ippan.Events do
  alias Ippan.Event

  alias Ippan.Func.{
    Env,
    Balance,
    Tx,
    Validator,
    Token,
    Domain,
    Dns,
    Wallet
  }

  @spec lookup(event_id :: non_neg_integer()) :: Event.t() | :undefined
  def lookup(n = 0),
    do: %Event{
      id: n,
      name: "wallet.sub",
      base: :wallet,
      mod: Wallet,
      fun: :subscribe,
      before: :pre_sub,
      deferred: true,
      validator: false
    }

  def lookup(n = 1),
    do: %Event{
      id: n,
      name: "wallet.unsub",
      base: :wallet,
      mod: Wallet,
      fun: :unsubscribe,
      deferred: false
    }

  def lookup(n = 50),
    do: %Event{
      id: n,
      name: "env.set",
      base: :env,
      mod: Env,
      fun: :set,
      before: :pre_set,
      deferred: true
    }

  def lookup(n = 51),
    do: %Event{
      id: n,
      name: "env.delete",
      base: :env,
      mod: Env,
      before: :pre_delete,
      fun: :delete,
      deferred: true
    }

  def lookup(n = 100),
    do: %Event{
      id: n,
      name: "validator.new",
      base: :validator,
      mod: Validator,
      fun: :new,
      before: :pre_new,
      deferred: true
    }

  def lookup(n = 101),
    do: %Event{
      id: n,
      name: "validator.update",
      base: :validator,
      mod: Validator,
      fun: :update,
      before: :pre_update,
      deferred: true
    }

  def lookup(n = 102),
    do: %Event{
      id: n,
      name: "validator.delete",
      base: :validator,
      mod: Validator,
      fun: :delete,
      before: :pre_delete,
      deferred: true
    }

  def lookup(n = 200),
    do: %Event{
      id: n,
      name: "token.new",
      base: :token,
      mod: Token,
      fun: :new,
      before: :pre_new,
      deferred: true
    }

  def lookup(n = 201),
    do: %Event{
      id: n,
      name: "token.update",
      base: :token,
      mod: Token,
      fun: :update
    }

  def lookup(n = 202),
    do: %Event{
      id: n,
      name: "token.delete",
      base: :token,
      mod: Token,
      fun: :delete
    }

  def lookup(n = 250),
    do: %Event{
      id: n,
      name: "balance.lock",
      base: :balance,
      mod: Balance,
      fun: :lock
    }

  def lookup(n = 251),
    do: %Event{
      id: n,
      name: "balance.unlock",
      base: :balance,
      mod: Balance,
      fun: :unlock
    }

  def lookup(n = 300),
    do: %Event{
      id: n,
      name: "tx.coinbase",
      base: :tx,
      mod: Tx,
      fun: :coinbase
    }

  def lookup(n = 301),
    do: %Event{
      id: n,
      name: "tx.send",
      base: :tx,
      mod: Tx,
      fun: :send
    }

  def lookup(n = 302),
    do: %Event{
      id: n,
      name: "tx.burn",
      base: :tx,
      mod: Tx,
      fun: :burn
    }

  def lookup(n = 303),
    do: %Event{
      id: n,
      name: "tx.refund",
      mod: Tx,
      base: :tx,
      fun: :refund
    }

  def lookup(n = 304),
    do: %Event{
      id: n,
      name: "tx.refundable",
      mod: Tx,
      base: :tx,
      fun: :send_refundable
    }

  def lookup(n = 400),
    do: %Event{
      id: n,
      name: "domain.new",
      mod: Domain,
      base: :domain,
      fun: :new,
      before: :pre_new,
      deferred: true
    }

  def lookup(n = 401),
    do: %Event{
      id: n,
      name: "domain.update",
      base: :domain,
      mod: Domain,
      fun: :update
    }

  def lookup(n = 402),
    do: %Event{
      id: n,
      name: "domain.delete",
      base: :domain,
      mod: Domain,
      fun: :delete
    }

  def lookup(n = 403),
    do: %Event{
      id: n,
      name: "domain.renew",
      base: :domain,
      mod: Domain,
      fun: :renew
    }

  def lookup(n = 500),
    do: %Event{
      id: n,
      name: "dns.new",
      base: :dns,
      mod: Dns,
      fun: :new
    }

  def lookup(n = 501),
    do: %Event{
      id: n,
      name: "dns.update",
      base: :dns,
      mod: Dns,
      fun: :update
    }

  def lookup(n = 502),
    do: %Event{
      id: n,
      name: "dns.delete",
      base: :dns,
      mod: Dns,
      fun: :delete
    }

  def lookup(_), do: :undefined
end
