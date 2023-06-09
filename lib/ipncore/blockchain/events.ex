defmodule Ippan.Events do
  alias Ippan.Event

  alias Ippan.Func.{
    Env,
    Account,
    Balance,
    Tx,
    Block,
    Validator,
    Token,
    Domain,
    DNS,
    Round,
    Wallet
  }

  @spec lookup(event_id :: non_neg_integer()) :: Event.t() | :undefined
  def lookup(n = 0),
    do: %Event{
      id: n,
      name: "wallet.new",
      base: :wallet,
      mod: Wallet,
      fun: :new,
      deferred: true,
      validator: false,
      auth: false
    }

  def lookup(n = 1),
    do: %Event{
      id: n,
      name: "wallet.subscribe",
      base: :wallet,
      mod: Wallet,
      fun: :subscribe,
      deferred: true,
      validator: false,
      auth: true
    }

  def lookup(n = 50),
    do: %Event{
      id: n,
      name: "env.set",
      base: :env,
      mod: Env,
      fun: :set,
      deferred: true,
      auth: true
    }

  def lookup(n = 51),
    do: %Event{
      id: n,
      name: "env.delete",
      base: :env,
      mod: Env,
      fun: :delete,
      deferred: true,
      auth: true
    }

  def lookup(n = 100),
    do: %Event{
      id: n,
      name: "validator.new",
      base: :validator,
      mod: Validator,
      fun: :new,
      deferred: true,
      auth: true
    }

  def lookup(n = 101),
    do: %Event{
      id: n,
      name: "validator.update",
      base: :validator,
      mod: Validator,
      fun: :update,
      auth: true
    }

  def lookup(n = 102),
    do: %Event{
      id: n,
      name: "validator.delete",
      base: :validator,
      mod: Validator,
      fun: :delete,
      auth: true
    }

  def lookup(n = 200),
    do: %Event{
      id: n,
      name: "token.new",
      base: :token,
      mod: Token,
      fun: :new,
      deferred: true,
      auth: true
    }

  def lookup(n = 201),
    do: %Event{
      id: n,
      name: "token.update",
      base: :token,
      mod: Token,
      fun: :update,
      auth: true
    }

  def lookup(n = 202),
    do: %Event{
      id: n,
      name: "token.delete",
      base: :token,
      mod: Token,
      fun: :delete,
      auth: true
    }

  def lookup(n = 250),
    do: %Event{
      id: n,
      name: "balance.lock",
      base: :balance,
      mod: Balance,
      fun: :lock,
      auth: true
    }

  def lookup(n = 251),
    do: %Event{
      id: n,
      name: "balance.unlock",
      base: :balance,
      mod: Balance,
      fun: :unlock,
      auth: true
    }

  def lookup(n = 300),
    do: %Event{
      id: n,
      name: "tx.coinbase",
      base: :tx,
      mod: Tx,
      fun: :coinbase,
      auth: true
    }

  def lookup(n = 301),
    do: %Event{
      id: n,
      name: "tx.send",
      base: :tx,
      mod: Tx,
      fun: :send,
      auth: true
    }

  def lookup(n = 302),
    do: %Event{
      id: n,
      name: "tx.burn",
      base: :tx,
      mod: Tx,
      fun: :burn,
      auth: true
    }

  def lookup(n = 303),
    do: %Event{
      id: n,
      name: "tx.refund",
      mod: Tx,
      base: :tx,
      fun: :refund,
      auth: true
    }

  def lookup(n = 400),
    do: %Event{
      id: n,
      name: "domain.new",
      mod: Domain,
      base: :domain,
      fun: :new,
      deferred: true,
      auth: true
    }

  def lookup(n = 401),
    do: %Event{
      id: n,
      name: "domain.update",
      base: :domain,
      mod: Domain,
      fun: :update,
      auth: true
    }

  def lookup(n = 402),
    do: %Event{
      id: n,
      name: "domain.delete",
      base: :domain,
      mod: Domain,
      fun: :delete,
      auth: true
    }

  def lookup(n = 403),
    do: %Event{
      id: n,
      name: "domain.renew",
      base: :domain,
      mod: Domain,
      fun: :renew,
      auth: true
    }

  # def lookup(n = 404),
  #   do: %Event{
  #     id: n,
  #     name: "domain.expiry",
  #     base: :domain,
  #     mod: Domain,
  #     fun: :expiry,
  #
  #     system: true,
  #     auth: false
  #   }

  def lookup(n = 500),
    do: %Event{
      id: n,
      name: "dns.new",
      base: :dns,
      mod: DNS,
      fun: :new,
      auth: true
    }

  def lookup(n = 501),
    do: %Event{
      id: n,
      name: "dns.update",
      base: :dns,
      mod: DNS,
      fun: :update,
      auth: true
    }

  def lookup(n = 502),
    do: %Event{
      id: n,
      name: "dns.delete",
      base: :dns,
      mod: DNS,
      fun: :delete,
      auth: true
    }

  # def lookup(n = 900),
  #   do: %Event{
  #     id: n,
  #     name: "block.new",
  #     base: :block,
  #     mod: Block,
  #     fun: :new,
  #
  #     system: true,
  #     auth: false
  #   }

  # def lookup(n = 901),
  #   do: %Event{
  #     id: n,
  #     name: "block.received",
  #     base: :block,
  #     mod: Block,
  #     fun: :received,
  #
  #     system: true,
  #     auth: false
  #   }

  # def lookup(n = 990),
  #   do: %Event{
  #     id: n,
  #     name: "round.start",
  #     base: :round,
  #     mod: Round,
  #     fun: :start,
  #
  #     system: true,
  #     auth: false
  #   }

  # def lookup(n = 999),
  #   do: %Event{
  #     id: n,
  #     name: "round.end",
  #     base: :round,
  #     mod: Round,
  #     fun: :end,
  #
  #     system: true,
  #     auth: false
  #   }

  def lookup(_), do: :undefined
end
