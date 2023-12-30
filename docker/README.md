### Build image
```bash
docker build -t ipncore:0.5 .
```

### Run a container
```bash
docker run -e \
-p 4848:4848 -p 5815:5815 -p 8080:8080 --volume data:/var/data \
--restart=on-failure:5 -d --name miner ipncore:0.5
```

### Arguments to docker run:
* Node name: `-e NAME=<string>`
* Validator ID: `-e VID=<number>`
* secret key: `-e SECRET_KEY=<base64-string>`
* cluster secret key: `-e CLUSTER_KEY=<base64-string>`
* Data folder path: `-e DATA_DIR=<path>` (optional)
* Print loggerfile: `-e LOG=<filepath>` (optional)

### Docker interactive mode
```bash
docker exec -it miner /bin/bash
```
