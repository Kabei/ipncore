#!/usr/bin/bash
# debian 11

apt-get update
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
dpkg -i erlang-solutions_2.0_all.deb
wget https://packages.erlang-solutions.com/debian/erlang_solutions.asc
apt-key add erlang_solutions.asc
apt-get install erlang elixir git build-essential -y

add-apt-repository ppa:certbot/certbot -y
apt-get update
apt install certbot -y

git clone --branch beta https://kabei@github.com/kabei/ipncore.git
git clone https://kabei@github.com/kabei/ipnutils.git
git clone https://kabei@github.com/kabei/falcon.git

certbot certonly --standalone -d ippan.uk --non-interactive --agree-tos --email contact@ippan.com

cd ipncore
mix local.hex --force
mix deps.get
mix local.rebar --force

echo "
[Unit]
Description=IPPAN Core Service
After=network.target
StartLimitIntervalSec=0

[Service]
User=root
Group=root
Type=simple
TimeoutStopSec=0
Environment=MIX_ENV=prod
WorkingDirectory=$PWD
ExecStart=elixir --erl \"+P 2000000 +A 10\" -S mix run --no-halt --no-compile
ExecStop=/bin/kill -s TERM \$MAINPID
Restart=always
RestartSec=1
PIDFile=/run/ipncore/ipncore.pid
LimitNOFILE=65535
MemoryDenyWriteExecute=true
ProtectKernelModules=true
ProtectKernelTunables=true
ProtectControlGroups=true
RestrictRealtime=true
RestrictNamespaces=true

[Install]
WantedBy=multi-user.target
Alias=ipncore.service
" > /etc/systemd/system/ipncore.service

systemctl daemon-reload
./script/install-db.sh
./script/ufw.sh

mkdir -p priv/cert
cp /etc/letsencrypt/live/ippan.uk/privkey.pem priv/cert/key.pem
cp /etc/letsencrypt/live/ippan.uk/fullchain.pem priv/cert/cacert.pem
cp /etc/letsencrypt/live/ippan.uk/cert.pem priv/cert/cert.pem
# systemctl start ipncore.service