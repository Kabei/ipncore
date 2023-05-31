#!/usr/bin/bash
# debian 11

apt-get update
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
dpkg -i erlang-solutions_2.0_all.deb
wget https://packages.erlang-solutions.com/debian/erlang_solutions.asc
apt-key add erlang_solutions.asc
apt-get install erlang elixir git build-essential -y
curl https://sh.rustup.rs -sSf | sh

git clone --branch beta2 https://kabei@github.com/kabei/ipncore.git
git clone https://kabei@github.com/kabei/falcon.git
git clone https://kabei@github.com/kabei/ntrukem.git
git clone https://kabei@github.com/kabei/exqlite.git
git clone https://kabei@github.com/kabei/fast64_elixir.git

cd ipncore
mix local.hex --force
mix deps.get
mix local.rebar --force

# redhat 9
dnf update -y
curl https://sh.rustup.rs -sSf | sh
dnf install make wget git -y

wget https://packages.erlang-solutions.com/erlang-solutions-2.0-1.noarch.rpm
sudo rpm -Uvh erlang-solutions-2.0-1.noarch.rpm
sudo yum install erlang


sudo docker exec -it 6bdb0d46f110 /bin/bash
