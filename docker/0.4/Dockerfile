FROM elixir:1.15.1-slim

LABEL HASH_COMMIT=${HASH_COMMIT}

ENV PATH="/root/.cargo/bin:${PATH}" \
    MIX_ENV=prod \
    DATA_DIR=/var/data \
    KEM_DIR=priv/kem.key \
    FALCON_DIR=priv/falcon.key \
    KEY_DIR=priv/secret.key

WORKDIR /usr/src

RUN apt-get update && apt-get install -y git curl cmake
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

RUN git clone --branch v0.4 https://kabei@github.com/kabei/ipncore.git \
    && git clone https://kabei@github.com/kabei/falcon.git \
    && git clone https://kabei@github.com/kabei/ntrukem.git \
    && git clone https://kabei@github.com/kabei/exqlite.git \
    && git clone https://kabei@github.com/kabei/fast64_elixir.git \
    && git clone https://github.com/Kabei/blake3.git

WORKDIR /usr/src/ipncore

RUN mix local.hex --force && mix deps.get && mix local.rebar --force
RUN mix compile

RUN chmod +x run \ 
    && mkdir -p priv
COPY kem.key priv/
COPY falcon.key priv/
COPY secret.key priv/

EXPOSE 5815 8080

VOLUME ["/var/data"]

ENTRYPOINT ["./run"]

# docker build -t ipncore:0.4 .
# docker run --rm -e ROLE=miner -e NODE=miner@localhost -e COOKIE=CfBa3fdR1AZuefnx -p 5815:5815 -p 8080:80 --volume data:/var/data -d --name miner ipncore:0.4
# docker run --rm -e ROLE=miner -e NODE=miner@localhost -e COOKIE=CfBa3fdR1AZuefnx -p 5815:5815 -p 8080:80 --volume data:/var/data -it --name miner ipncore:0.4
# docker exec -it miner /bin/bash
