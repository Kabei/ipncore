## IPNCORE

#### Version 0.4
#
#### IPNcore es un nodo de verificacion de transacciones de IPPAN blockchain.
#### IPPAN blockchain realiza mineria paralela, permitiendo

## Requirements

* CPU: +4 cores
* Memory: +4 GB
* Storage: +20 GB
* Bandwitch: +50 Mbps

## Dependencies

* Erlang 25+
* Elixir 1.14+
* cargo 1.70+
* cmake 3.26+

## 

## Installation
### Generate keys
#### Each validator has 2 keys, one for signing blocks and another for signing connections. In addition, a third key (ntru-kem) is used for the handshake together with the falcon-512 key.

#### It is necessary to create the key files of the validator certificate that has the seeds in base64 format.
#### (!) The following sizes are binary and must be written in base64 text in the file
* secret.key (32 bytes fixed)
* falcon.key (+48 bytes)
* kem.key (+48 bytes)

```bash
curl https://github.com/kabei/release/0.4/ipncore-install.sh \
&& chmod +x ipncore-install.sh \
&& ./ipncore-install.sh
```
##### (!) Only tested on Debian 11
#### The installer will move the key files to the project's private folder.
#
## Run

#### There are two roles: the verifier performs preliminary verification and the miner writes transactions to the blockchain.
### Miner role
```bash
MIX_ENV=prod ROLE=miner NODE=miner@172.17.0.1 COOKIE=supersecret \
VID=1 DATA_DIR=/usr/src/data ./run.sh
```
### Verifier role
```bash
MIX_ENV=prod ROLE=verifier NODE=v1@172.17.0.2 COOKIE=supersecret \
MINER=miner@172.17.0.1 VID=1 DATA_DIR=/usr/src/data ./run.sh
```

#
## Blockchain Default settings

| | |
|-|-|
|Block Time|5 seconds|
|Time out of refund transactions|72 hours|
|Event Max Size|8.192 bytes|
|Tx note Max Size|255 bytes|
|Native Token|IPN|
|Block Max size|10 MB|
