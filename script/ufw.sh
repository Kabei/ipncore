#!/usr/bin/bash
apt-get install ufw
ufw default deny incoming
ufw default allow outgoing
ufw allow ssh
ufw allow http
ufw allow https
ufw allow 53/udp
ufw allow 853/tcp
ufw enable