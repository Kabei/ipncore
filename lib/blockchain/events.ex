defmodule Ippan.Events do
  alias Ippan.Event
  alias Ippan.Func.{Env, Account, Balance, Block, Validator, Token, Domain, DNS, Round}

  @spec lookup(event_id :: non_neg_integer()) :: Event.t() | :undefined
  def lookup(n = 0),
    do: %Event{id: n, name: "account.new", mod: Account, fun: :new, parallel: false, auth_type: 0}

  def lookup(n = 1),
    do: %Event{
      id: n,
      name: "account.validator",
      mod: Account,
      parallel: false,
      fun: :validator,
      auth_type: 2
    }

  def lookup(n = 2),
    do: %Event{
      id: n,
      name: "account.recover",
      mod: Account,
      fun: :recover,
      parallel: true,
      auth_type: 1
    }

  def lookup(n = 3),
    do: %Event{
      id: n,
      name: "account.update",
      mod: Account,
      fun: :update,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 50),
    do: %Event{id: n, name: "env.set", mod: Env, fun: :set, parallel: false, auth_type: 2}

  def lookup(n = 51),
    do: %Event{id: n, name: "env.delete", mod: Env, fun: :delete, parallel: false, auth_type: 2}

  def lookup(n = 100),
    do: %Event{
      id: n,
      name: "validator.new",
      mod: Validator,
      fun: :new,
      parallel: false,
      auth_type: 2
    }

  def lookup(n = 101),
    do: %Event{
      id: n,
      name: "validator.update",
      mod: Validator,
      fun: :update,
      parallel: false,
      auth_type: 2
    }

  def lookup(n = 102),
    do: %Event{
      id: n,
      name: "validator.delete",
      mod: Validator,
      fun: :delete,
      parallel: false,
      auth_type: 2
    }

  def lookup(n = 200),
    do: %Event{id: n, name: "token.new", mod: Token, fun: :new, parallel: false, auth_type: 2}

  def lookup(n = 201),
    do: %Event{
      id: n,
      name: "token.update",
      mod: Token,
      fun: :update,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 202),
    do: %Event{
      id: n,
      name: "token.delete",
      mod: Token,
      fun: :delete,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 250),
    do: %Event{
      id: n,
      name: "balance.lock",
      mod: Balance,
      fun: :lock,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 251),
    do: %Event{
      id: n,
      name: "balance.unlock",
      mod: Balance,
      fun: :unlock,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 300),
    do: %Event{id: n, name: "tx.coinbase", mod: Tx, fun: :coinbase, parallel: true, auth_type: 2}

  def lookup(n = 301),
    do: %Event{id: n, name: "tx.send", mod: Tx, fun: :send, parallel: true, auth_type: 2}

  def lookup(n = 302),
    do: %Event{id: n, name: "tx.burn", mod: Tx, fun: :burn, parallel: true, auth_type: 2}

  def lookup(n = 303),
    do: %Event{id: n, name: "tx.refund", mod: Tx, fun: :refund, parallel: true, auth_type: 2}

  def lookup(n = 400),
    do: %Event{id: n, name: "domain.new", mod: Domain, fun: :new, parallel: false, auth_type: 2}

  def lookup(n = 401),
    do: %Event{
      id: n,
      name: "domain.update",
      mod: Domain,
      fun: :update,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 402),
    do: %Event{
      id: n,
      name: "domain.delete",
      mod: Domain,
      fun: :delete,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 403),
    do: %Event{
      id: n,
      name: "domain.renew",
      mod: Domain,
      fun: :renew,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 404),
    do: %Event{
      id: n,
      name: "domain.expiry",
      mod: Domain,
      fun: :expiry,
      parallel: true,
      system: true,
      auth_type: 0
    }

  def lookup(n = 500),
    do: %Event{
      id: n,
      name: "dns.new",
      mod: DNS,
      fun: :new,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 501),
    do: %Event{
      id: n,
      name: "dns.update",
      mod: DNS,
      fun: :update,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 502),
    do: %Event{
      id: n,
      name: "dns.delete",
      mod: DNS,
      fun: :delete,
      parallel: true,
      auth_type: 2
    }

  def lookup(n = 900),
    do: %Event{
      id: n,
      name: "block.new",
      mod: Block,
      fun: :new,
      parallel: true,
      system: true,
      auth_type: 0
    }

  def lookup(n = 901),
    do: %Event{
      id: n,
      name: "block.received",
      mod: Block,
      fun: :received,
      parallel: true,
      system: true,
      auth_type: 0
    }

  def lookup(n = 990),
    do: %Event{
      id: n,
      name: "round.start",
      mod: Round,
      fun: :start,
      parallel: true,
      system: true,
      auth_type: 0
    }

  def lookup(n = 999),
    do: %Event{
      id: n,
      name: "round.end",
      mod: Round,
      fun: :end,
      parallel: true,
      system: true,
      auth_type: 0
    }

  def lookup(_), do: :undefined
end
