## IPNCORE
IPPAN blockchain transaction verification node.

## Requirements
* Processor: 8 CPUs
* Memory: 4 GB RAM
* Storage: 50 GB SSD NVME
* Bandwitch: 1 Gbps
* Public IPv4 / IPv6

## Dependencies
* Erlang 25
* Elixir 1.14
* cargo 1.70
* cmake 3.26
* git 2.41.0

## Installation 
### Generate keys
Makes 2 random seeds of 32 bytes in base64
```
openssl rand -base64 32
```
#### Generate env_file
```bash
echo "
NAME=miner
VID=<number>
SECRET_KEY=<base64-seed-32-bytes>
CLUSTER_KEY=<base64-seed-32-bytes>
DATA_DIR=/usr/src/data
NODES=worker1@192.168.0.2" > env_file
```

#### Download and execute script
```bash
curl https://github.com/kabei/releases/download/0.5/ipncore-install.sh \
&& chmod +x ipncore-install.sh \
&& ./ipncore-install.sh
```

## Run

```bash
./run.sh
```
## Docker
See docker/README.md

## Settings
|||
|-|-|
|Blockchain|IPPAN|
|Block Time|5 seconds|
|Native Token|IPN|
|Block file Max size|10 MB|
|Transaction Max size|8192 bytes|
|Tx note Max size|255 bytes|
|Refund transaction timeout|72 hours|
|Max tranfer amount|Thousand billion units|
|P2P port|5815|
|Cluster port|4848|
|HTTP port|8080|
