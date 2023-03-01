#!/usr/bin/bash
apt-get install postgresql
sudo -u postgres psql
create database ippan;
create user kambei with encrypted password 'NdgPPUWiSXF1EQbC5Pqm';
grant all privileges on database ippan to kambei;
exit