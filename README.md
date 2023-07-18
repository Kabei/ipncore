## IPNCORE

#### Version 0.4
#
#### IPNcore is an IPPAN blockchain transaction verification node.

## Requirements

* Processor: 4 CPUs
* Memory: 4 GB
* HDD Space: +20 GB
* Bandwitch: 50 Mbps
* Public IPv4 / IPv6

## Dependencies

* Erlang 25+
* Elixir 1.14+
* cargo 1.70+
* cmake 3.26+

## 
## Installation
### Generate keys
Each validator has 2 keys, one for signing blocks and another for signing connections. In addition, a third key (ntru-kem) is used for the handshake together with the falcon-512 key.

It is necessary to create the key files of the validator certificate that has the seeds in base64 format.
#### (!) The following sizes are binary and must be written in base64 text in the file
* secret.key (32 bytes fixed)
* falcon.key (+48 bytes)
* kem.key (+48 bytes)

```bash
curl https://github.com/kabei/releases/download/0.4/ipncore-install.sh \
&& chmod +x ipncore-install.sh \
&& ./ipncore-install.sh
```
##### (!) Only tested on Debian 11
#### The installer will move the key files to the project's private folder.
#
## Run
There are two roles: the verifier performs preliminary verification and the miner writes transactions to the blockchain.

### Verifier role
```bash
echo "NODE=v1@127.0.0.1
COOKIE=supersecret
VID=0
ROLE=miner
DATA_DIR=/usr/src/data
MINER=miner@127.0.0.1" > env_file

./run.sh
```
### Miner role
```bash
echo "NODE=miner@127.0.0.1
COOKIE=supersecret
VID=0
ROLE=miner
DATA_DIR=/usr/src/data" > env_file

./run.sh
```

#
## Default settings

| | |
|-|-|
|Block Time|5 seconds|
|Native Token|IPN|
|Block file Max size|10 MB|
|Request Max size|8192 bytes|
|Max tranfer amount|Thousand billion units|
|P2P port|5815|
|HTTP port|8080|
|Refund transaction timeout|72 hours|
<!-- |Tx note Max size|255 bytes| -->
