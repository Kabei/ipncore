#!/usr/bin/bash
# debian 11

wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
dpkg -i erlang-solutions_2.0_all.deb
wget https://packages.erlang-solutions.com/debian/erlang_solutions.asc
apt-key add erlang_solutions.asc
apt-get update
apt-get install erlang elixir git build-essential cargo -y

git clone --branch beta2 https://kabei@github.com/kabei/ipncore.git
git clone https://kabei@github.com/kabei/falcon.git
git clone https://kabei@github.com/kabei/ntrukem.git
git clone https://kabei@github.com/kabei/exqlite.git
git clone https://kabei@github.com/kabei/fast64.git

cd ipncore
mix local.hex --force
mix deps.get
mix local.rebar --force