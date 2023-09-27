defmodule Ippan.Funcs do
  alias Ippan.Func
  alias Ippan.Funx

  @spec lookup(event_id :: non_neg_integer()) :: Func.t() | :undefined
  def lookup(0) do
    %Func{
      id: 0,
      name: "wallet.sub",
      mod: Func.Wallet,
      modx: Funx.Wallet,
      fun: :subscribe,
      deferred: true,
      check: 1
    }
  end

  def lookup(1) do
    %Func{
      id: 1,
      name: "wallet.unsub",
      mod: Func.Wallet,
      modx: Funx.Wallet,
      fun: :unsubscribe,
      deferred: false
    }
  end

  def lookup(50) do
    %Func{
      id: 50,
      name: "env.set",
      mod: Func.Env,
      modx: Funx.Env,
      fun: :set,
      deferred: true
    }
  end

  def lookup(51) do
    %Func{
      id: 51,
      name: "env.delete",
      mod: Func.Env,
      modx: Funx.Env,
      fun: :delete,
      deferred: true
    }
  end

  def lookup(100) do
    %Func{
      id: 100,
      name: "validator.new",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :new,
      deferred: true
    }
  end

  def lookup(101) do
    %Func{
      id: 101,
      name: "validator.update",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :update,
      deferred: true
    }
  end

  def lookup(102) do
    %Func{
      id: 102,
      name: "validator.delete",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :delete,
      deferred: true
    }
  end

  def lookup(200) do
    %Func{
      id: 200,
      name: "token.new",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :new,
      deferred: true
    }
  end

  def lookup(201) do
    %Func{
      id: 201,
      name: "token.update",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :update
    }
  end

  def lookup(202) do
    %Func{
      id: 202,
      name: "token.delete",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :delete
    }
  end

  def lookup(250) do
    %Func{
      id: 250,
      name: "balance.lock",
      mod: Func.Balance,
      modx: Funx.Balance,
      fun: :lock,
      check: 2
    }
  end

  def lookup(251) do
    %Func{
      id: 251,
      name: "balance.unlock",
      mod: Func.Balance,
      modx: Funx.Balance,
      fun: :unlock,
      check: 2
    }
  end

  def lookup(300) do
    %Func{
      id: 300,
      name: "tx.coinbase",
      mod: Func.Tx,
      modx: Funx.Tx,
      fun: :coinbase
    }
  end

  def lookup(301) do
    %Func{
      id: 301,
      name: "tx.send",
      mod: Func.Tx,
      modx: Funx.Tx,
      fun: :send
    }
  end

  def lookup(302) do
    %Func{
      id: 302,
      name: "tx.refundable",
      mod: Func.Tx,
      modx: Funx.Tx,
      fun: :send_refundable
    }
  end

  def lookup(303) do
    %Func{
      id: 303,
      name: "tx.refund",
      mod: Func.Tx,
      modx: Funx.Tx,
      fun: :refund
    }
  end

  def lookup(304) do
    %Func{
      id: 304,
      name: "tx.burn",
      mod: Func.Tx,
      modx: Funx.Tx,
      fun: :burn
    }
  end

  def lookup(400) do
    %Func{
      id: 400,
      name: "domain.new",
      mod: Func.Domain,
      modx: Funx.Domain,
      fun: :new,
      deferred: true
    }
  end

  def lookup(401) do
    %Func{
      id: 401,
      name: "domain.update",
      mod: Func.Domain,
      modx: Funx.Domain,
      fun: :update
    }
  end

  def lookup(402) do
    %Func{
      id: 402,
      name: "domain.delete",
      mod: Func.Domain,
      modx: Funx.Domain,
      fun: :delete
    }
  end

  def lookup(403) do
    %Func{
      id: 403,
      name: "domain.renew",
      mod: Func.Domain,
      modx: Funx.Domain,
      fun: :renew
    }
  end

  def lookup(500) do
    %Func{
      id: 500,
      name: "dns.new",
      mod: Func.Dns,
      modx: Funx.Dns,
      fun: :new
    }
  end

  def lookup(501) do
    %Func{
      id: 501,
      name: "dns.update",
      mod: Func.Dns,
      modx: Funx.Dns,
      fun: :update
    }
  end

  def lookup(502) do
    %Func{
      id: 502,
      name: "dns.delete",
      mod: Func.Dns,
      modx: Funx.Dns,
      fun: :delete
    }
  end

  def lookup(_), do: :undefined
end
