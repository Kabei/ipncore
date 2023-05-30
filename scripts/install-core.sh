#!/usr/bin/bash
# debian 11

apt-get update
apt-get install erlang elixir git build-essential -y

git clone --branch beta2 https://kabei@github.com/kabei/ipncore.git
git clone https://kabei@github.com/kabei/falcon.git
git clone https://kabei@github.com/kabei/ntrukem.git
git clone https://kabei@github.com/kabei/exqlite.git
git clone https://kabei@github.com/kabei/fast64.git

cd ipncore
mix local.hex --force
mix deps.get
mix local.rebar --force