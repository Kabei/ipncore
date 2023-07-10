#!/usr/bin/bash

export PATH="/root/.cargo/bin:${PATH}"
export MIX_ENV=prod
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

# Environment
Environment=MIX_ENV=prod
Environment=ROLE=${ROLE}
Environment=NODE=${NODE}
Environment=DATA_DIR=${DATA_DIR}
Environment=VID=${VID}

WorkingDirectory=/usr/src/ipncore
ExecStart=./run.sh
ExecStop=/bin/kill -s TERM $MAINPID
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
systemctl start ipncore.service