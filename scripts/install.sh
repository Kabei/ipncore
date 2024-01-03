#!/usr/bin/bash

export MIX_ENV=prod
export PATH="/root/.cargo/bin:${PATH}"

apt update -y
apt install erlang elixir curl git cmake zip unzip -y
curl https://sh.rustup.rs -sSf | sh -s -- -y

git clone --branch v0.5 https://kabei@github.com/kabei/ipncore.git

cd ipncore

mix local.hex --force
mix deps.get
mix local.rebar --force
mix compile

cp ../env_file ./

