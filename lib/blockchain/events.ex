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

  @spec lookup(event_id :: non_neg_integer()) :: map() | :undefined
  def lookup(0) do
    %Event{
      id: 0,
      name: "wallet.sub",
      base: :wallet,
      mod: Wallet,
      fun: :subscribe,
      before: :pre_sub,
      deferred: true,
      validator: false
    }
  end

  def lookup(1) do
    %Event{
      id: 1,
      name: "wallet.unsub",
      base: :wallet,
      mod: Wallet,
      fun: :unsubscribe,
      deferred: false
    }
  end

  def lookup(50) do
    %Event{
      id: 50,
      name: "env.set",
      base: :env,
      mod: Env,
      fun: :set,
      before: :pre_set,
      deferred: true
    }
  end

  def lookup(51) do
    %Event{
      id: 51,
      name: "env.delete",
      base: :env,
      mod: Env,
      before: :pre_delete,
      fun: :delete,
      deferred: true
    }
  end

  def lookup(100) do
    %Event{
      id: 100,
      name: "validator.new",
      base: :validator,
      mod: Validator,
      fun: :new,
      before: :pre_new,
      deferred: true
    }
  end

  def lookup(101) do
    %Event{
      id: 101,
      name: "validator.update",
      base: :validator,
      mod: Validator,
      fun: :update,
      before: :pre_update,
      deferred: true
    }
  end

  def lookup(102) do
    %Event{
      id: 102,
      name: "validator.delete",
      base: :validator,
      mod: Validator,
      fun: :delete,
      before: :pre_delete,
      deferred: true
    }
  end

  def lookup(200) do
    %Event{
      id: 200,
      name: "token.new",
      base: :token,
      mod: Token,
      fun: :new,
      before: :pre_new,
      deferred: true
    }
  end

  def lookup(201) do
    %Event{
      id: 201,
      name: "token.update",
      base: :token,
      mod: Token,
      fun: :update
    }
  end

  def lookup(202) do
    %Event{
      id: 202,
      name: "token.delete",
      base: :token,
      mod: Token,
      fun: :delete
    }
  end

  def lookup(250) do
    %Event{
      id: 250,
      name: "balance.lock",
      base: :balance,
      mod: Balance,
      fun: :lock
    }
  end

  def lookup(251) do
    %Event{
      id: 251,
      name: "balance.unlock",
      base: :balance,
      mod: Balance,
      fun: :unlock
    }
  end

  def lookup(300) do
    %Event{
      id: 300,
      name: "tx.coinbase",
      base: :tx,
      mod: Tx,
      fun: :coinbase
    }
  end

  def lookup(301) do
    %Event{
      id: 301,
      name: "tx.send",
      base: :tx,
      mod: Tx,
      fun: :send
    }
  end

  def lookup(302) do
    %Event{
      id: 304,
      name: "tx.refundable",
      mod: Tx,
      base: :tx,
      fun: :send_refundable
    }
  end

  def lookup(303) do
    %Event{
      id: 303,
      name: "tx.refund",
      mod: Tx,
      base: :tx,
      fun: :refund
    }
  end

  def lookup(304) do
    %Event{
      id: 304,
      name: "tx.burn",
      base: :tx,
      mod: Tx,
      fun: :burn
    }
  end

  def lookup(400) do
    %Event{
      id: 400,
      name: "domain.new",
      mod: Domain,
      base: :domain,
      fun: :new,
      before: :pre_new,
      deferred: true
    }
  end

  def lookup(401) do
    %Event{
      id: 401,
      name: "domain.update",
      base: :domain,
      mod: Domain,
      fun: :update
    }
  end

  def lookup(402) do
    %Event{
      id: 402,
      name: "domain.delete",
      base: :domain,
      mod: Domain,
      fun: :delete
    }
  end

  def lookup(403) do
    %Event{
      id: 403,
      name: "domain.renew",
      base: :domain,
      mod: Domain,
      fun: :renew
    }
  end

  def lookup(500) do
    %Event{
      id: 500,
      name: "dns.new",
      base: :dns,
      mod: Dns,
      fun: :new
    }
  end

  def lookup(501) do
    %Event{
      id: 501,
      name: "dns.update",
      base: :dns,
      mod: Dns,
      fun: :update
    }
  end

  def lookup(502) do
    %Event{
      id: 502,
      name: "dns.delete",
      base: :dns,
      mod: Dns,
      fun: :delete
    }
  end

  def lookup(_), do: :undefined
end
