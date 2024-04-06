#!/usr/bin/bash

export MIX_ENV=prod
export PATH="/root/.cargo/bin:${PATH}"

apt update -y

# On debian
apt install ca-certificates libyaml-dev erlang-dev elixir curl git cmake -y
# On fedora / Red-Hat like
# sudo dnf install ca-certificates libyaml-devel erlang elixir curl git cmake -y

curl https://sh.rustup.rs -sSf | sh -s -- -y

git clone --branch v0.5 https://kabei@github.com/kabei/ipncore.git

cd ipncore

mix local.hex --force
mix deps.get
mix local.rebar --force
mix compile

cp ../env_file ./

