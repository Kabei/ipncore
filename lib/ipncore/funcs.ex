defmodule Ippan.Funcs do
  alias Ippan.{Func, Funx}

  @spec lookup(fun_id :: non_neg_integer()) :: Func.t() | :undefined
  def lookup(0) do
    %Func{
      id: 0,
      name: "wallet.new",
      mod: Func.Wallet,
      modx: Funx.Wallet,
      fun: :new,
      deferred: true,
      check: 1
    }
  end

  def lookup(1) do
    %Func{
      id: 1,
      name: "wallet.sub",
      mod: Func.Wallet,
      modx: Funx.Wallet,
      fun: :subscribe,
      deferred: true
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
      name: "validator.join",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :join,
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
      name: "validator.leave",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :leave,
      deferred: true
    }
  end

  def lookup(103) do
    %Func{
      id: 103,
      name: "validator.env_set",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :env_set,
      deferred: true
    }
  end

  def lookup(104) do
    %Func{
      id: 104,
      name: "validator.env_delete",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :env_delete,
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

  def lookup(203) do
    %Func{
      id: 203,
      name: "token.prop_add",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :prop_add,
      deferred: true
    }
  end

  def lookup(204) do
    %Func{
      id: 204,
      name: "token.prop_drop",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :prop_drop,
      deferred: true
    }
  end

  def lookup(205) do
    %Func{
      id: 205,
      name: "token.env_set",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :env_set,
      deferred: true
    }
  end

  def lookup(206) do
    %Func{
      id: 206,
      name: "token.env_delete",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :env_delete,
      deferred: true
    }
  end

  def lookup(300) do
    %Func{
      id: 300,
      name: "coin.new",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :coinbase
    }
  end

  def lookup(301) do
    %Func{
      id: 301,
      name: "coin.send",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :send
    }
  end

  def lookup(302) do
    %Func{
      id: 302,
      name: "coin.refund",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :refund
    }
  end

  def lookup(303) do
    %Func{
      id: 303,
      name: "coin.lock",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :lock,
      deferred: true
    }
  end

  def lookup(304) do
    %Func{
      id: 304,
      name: "coin.unlock",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :unlock,
      deferred: true
    }
  end

  def lookup(305) do
    %Func{
      id: 305,
      name: "coin.burn",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :burn,
      deferred: true
    }
  end

  def lookup(306) do
    %Func{
      id: 306,
      name: "coin.multisend",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :multisend
    }
  end

  def lookup(307) do
    %Func{
      id: 307,
      name: "coin.reload",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :reload,
      deferred: true
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
