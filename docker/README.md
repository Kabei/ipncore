### Build image
```bash
docker build -t ipncore:0.4 .
```

### Run a miner container
```bash
docker run -e ROLE=miner -e NODE=miner@127.0.0.1 -e COOKIE=CfBa3fdR1AZuefnx \
-p 4369:4369 -p 5815:5815 -p 8080:8080 --volume data:/var/data \
--restart=on-failure:5 -d --name miner ipncore:0.4
```

### Run a verifier container
```bash
docker run -e ROLE=verifier -e NODE=verifier@127.0.0.1 -e COOKIE=CfBa3fdR1AZuefnx -e MINER=miner@127.0.0.1 \
-p 4369:4369 -p 8080:8080 --volume data:/var/data --restart=on-failure:5 \
-d --name verifier ipncore:0.4
```

### Arguments to docker run:
* Role: `-e ROLE=verifier|miner` (default: verifier)
* Node name: `-e NODE=name@address`
* Cookie secret: `-e COOKIE=value`
* Role: `-e MINER=miner@adddress` (only verifier role)
* IEX mode: `-e MODE=iex` (optional)
* Print log: `-e LOG=ippan.log` (optional)

### docker interactive mode
```bash
docker exec -it miner /bin/bash
```
