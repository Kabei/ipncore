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

#### Download and execute script
```bash
curl https://github.com/kabei/releases/download/0.5/ipncore-install.sh \
&& chmod +x ipncore-install.sh \
&& ./ipncore-install.sh
```
### Generate keys
```
mix run gen_keys.exs only
```

If you already have a registered account you can add base64 to that account
```
mix run gen_keys.exs <secret-base-64> only
```
#### Generate env_file
```bash
echo "
NAME=miner
VID=<number-of-validator-register>
SECRET_KEY=<secret-key-base-64>
CLUSTER_KEY=<cluster-key-base-64>
DATA_DIR=<custom-directory-path>
NODES=<name@hostname>" > env_file
```

## Run

```bash
cp scripts/run.sh ./run.sh
chmod +x run.sh
./run.sh
```
## Docker
See docker/README.md

## Config
|Name|Default|
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
