defmodule Ippan.Funcs do
  alias Ippan.{Func, Funx}

  @spec lookup(fun_id :: non_neg_integer()) :: Func.t() | :undefined
  def lookup(0) do
    %Func{
      id: 0,
      name: "account.new",
      mod: Func.Account,
      modx: Funx.Account,
      fun: :new,
      origin: {:pk, :first},
      index: {:arg, :first},
      priority: 3
    }
  end

  def lookup(1) do
    %Func{
      id: 1,
      name: "account.subscribe",
      mod: Func.Account,
      modx: Funx.Account,
      fun: :subscribe,
      origin: 2,
      priority: 4,
      index: 1
    }
  end

  def lookup(2) do
    %Func{
      id: 2,
      name: "account.editKey",
      mod: Func.Account,
      modx: Funx.Account,
      fun: :edit_key,
      index: 1,
      priority: 4
    }
  end

  def lookup(50) do
    %Func{
      id: 50,
      name: "env.set",
      mod: Func.Env,
      modx: Funx.Env,
      fun: :set,
      priority: "d",
      index: 1
    }
  end

  def lookup(51) do
    %Func{
      id: 51,
      name: "env.delete",
      mod: Func.Env,
      modx: Funx.Env,
      fun: :delete,
      priority: "d",
      index: 1
    }
  end

  def lookup(100) do
    %Func{
      id: 100,
      name: "validator.join",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :join,
      index: {:arg, :first},
      priority: 2
    }
  end

  def lookup(101) do
    %Func{
      id: 101,
      name: "validator.update",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :update,
      index: 1,
      priority: 3
    }
  end

  def lookup(102) do
    %Func{
      id: 102,
      name: "validator.leave",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :leave,
      index: 1,
      priority: 4
    }
  end

  def lookup(103) do
    %Func{
      id: 103,
      name: "validator.envPut",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :env_put,
      index: 1,
      priority: 3
    }
  end

  def lookup(104) do
    %Func{
      id: 104,
      name: "validator.envDelete",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :env_delete,
      index: 1,
      priority: 4
    }
  end

  def lookup(105) do
    %Func{
      id: 105,
      name: "validator.active",
      mod: Func.Validator,
      modx: Funx.Validator,
      fun: :active,
      index: 1,
      priority: 3
    }
  end

  def lookup(200) do
    %Func{
      id: 200,
      name: "token.new",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :new,
      index: {:arg, :first},
      priority: 3
    }
  end

  def lookup(201) do
    %Func{
      id: 201,
      name: "token.update",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :update,
      index: {:type, :first},
      priority: 3
    }
  end

  def lookup(202) do
    %Func{
      id: 202,
      name: "token.delete",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :delete,
      index: {:type, :first},
      priority: 4
    }
  end

  def lookup(203) do
    %Func{
      id: 203,
      name: "token.propAdd",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :prop_add,
      index: {:type, :first},
      priority: 3
    }
  end

  def lookup(204) do
    %Func{
      id: 204,
      name: "token.propDrop",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :prop_drop,
      index: {:type, :first},
      priority: 4
    }
  end

  def lookup(205) do
    %Func{
      id: 205,
      name: "token.envPut",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :env_put,
      index: {:type, :first},
      priority: 3
    }
  end

  def lookup(206) do
    %Func{
      id: 206,
      name: "token.envDelete",
      mod: Func.Token,
      modx: Funx.Token,
      fun: :env_delete,
      index: {:type, :first},
      priority: 4
    }
  end

  def lookup(300) do
    %Func{
      id: 300,
      name: "coin.new",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :coinbase,
      index: {:type, :first},
      priority: 2
    }
  end

  def lookup(301) do
    %Func{
      id: 301,
      name: "coin.send",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :send,
      priority: 1
    }
  end

  def lookup(302) do
    %Func{
      id: 302,
      name: "coin.refund",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :refund,
      priority: 2
    }
  end

  def lookup(303) do
    %Func{
      id: 303,
      name: "coin.lock",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :lock,
      origin: {:check, 0},
      priority: 0,
      flag: {:arg, 0}
    }
  end

  def lookup(304) do
    %Func{
      id: 304,
      name: "coin.unlock",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :unlock,
      origin: {:check, 0},
      priority: 0,
      flag: {:arg, 0}
    }
  end

  def lookup(305) do
    %Func{
      id: 305,
      name: "coin.drop",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :drop,
      index: 1,
      priority: 3
    }
  end

  def lookup(306) do
    %Func{
      id: 306,
      name: "coin.burn",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :burn,
      origin: {:check, 0},
      index: 1,
      priority: 2,
      flag: {:arg, 0}
    }
  end

  def lookup(307) do
    %Func{
      id: 307,
      name: "coin.multisend",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :multisend,
      priority: 1
    }
  end

  def lookup(308) do
    %Func{
      id: 308,
      name: "coin.reload",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :reload,
      index: 1,
      priority: 2
    }
  end

  def lookup(309) do
    %Func{
      id: 309,
      name: "coin.auth",
      mod: Func.Coin,
      modx: Funx.Coin,
      fun: :auth,
      index: {:type, :first, 2},
      priority: 3
    }
  end

  def lookup(600) do
    %Func{
      id: 600,
      name: "service.new",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :new,
      index: {:arg, :first},
      priority: 3
    }
  end

  def lookup(601) do
    %Func{
      id: 601,
      name: "service.update",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :update,
      index: 1,
      priority: 3
    }
  end

  def lookup(602) do
    %Func{
      id: 602,
      name: "service.delete",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :delete,
      index: 1,
      priority: 4
    }
  end

  def lookup(603) do
    %Func{
      id: 603,
      name: "service.pay",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :pay,
      priority: 2
    }
  end

  def lookup(604) do
    %Func{
      id: 604,
      name: "service.stream",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :stream,
      origin: {:check, 1},
      index: {:arg, :fisrt, 2},
      priority: 2,
      flag: {:arg, 1}
    }
  end

  def lookup(605) do
    %Func{
      id: 605,
      name: "service.withdraw",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :withdraw,
      index: 1,
      priority: 2
    }
  end

  def lookup(610) do
    %Func{
      id: 610,
      name: "service.subscribe",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :subscribe,
      index: {:from, :first}
    }
  end

  def lookup(611) do
    %Func{
      id: 611,
      name: "service.unsubscribe",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :unsubscribe,
      index: {:from, :first},
      priority: 4
    }
  end

  def lookup(612) do
    %Func{
      id: 612,
      name: "service.kick",
      mod: Func.Service,
      modx: Funx.Service,
      fun: :kick,
      priority: 4
    }
  end

  def lookup(900) do
    %Func{
      id: 900,
      name: "sys.upgrade",
      mod: Func.Sys,
      modx: Funx.Sys,
      fun: :upgrade,
      index: 3,
      priority: "d"
    }
  end

  def lookup(type) do
    IO.puts("type undefined #{inspect(type)}")
    :undefined
  end

  # def lookup(_), do: :undefined
end
