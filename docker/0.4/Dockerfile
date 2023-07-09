FROM elixir:1.15.1-slim

ENV PATH="/root/.cargo/bin:${PATH}" \
    MIX_ENV=prod \
    DATA_DIR=/var/data \
    KEM_DIR=priv/kem.key \
    FALCON_DIR=priv/falcon.key \
    KEY_DIR=priv/secret.key

WORKDIR /usr/src

RUN apt-get update && apt-get install -y git curl cmake
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

RUN git clone --branch v0.4 https://kabei@github.com/kabei/ipncore.git

WORKDIR /usr/src/ipncore

RUN mix local.hex --force && mix deps.get && mix local.rebar --force
RUN mix compile

RUN chmod +x run.sh \
    && chmod +x update.sh \
    && mkdir -p priv
COPY kem.key priv/
COPY falcon.key priv/
COPY secret.key priv/

EXPOSE 4369 5815 8080

VOLUME ["/var/data"]

ENTRYPOINT ["./run.sh"]
