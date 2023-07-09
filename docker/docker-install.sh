#!/bin/bash

apt update -y
apt -y install curl apt-transport-https ca-certificates curl gnupg2 software-properties-common

curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -cs) stable" -y
apt update -y

apt-cache policy docker-ce
apt -y install docker-ce
docker --version