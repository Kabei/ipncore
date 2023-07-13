#!/usr/bin/bash

export MIX_ENV=prod
export PATH="/root/.cargo/bin:${PATH}"

apt update -y
apt install erlang elixir curl git cmake -y
curl https://sh.rustup.rs -sSf | sh -s -- -y

git clone --branch v0.4 https://kabei@github.com/kabei/ipncore.git

cd ipncore

mix local.hex --force
mix deps.get
mix local.rebar --force
mix compile

mkdir -p priv
cp ../kem.key priv/
cp ../falcon.key priv/
cp ../secret.key priv/

chmod +x run.sh
chmod +x update.sh
