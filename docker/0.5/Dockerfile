FROM elixir:1.16.0-slim

ENV PATH="/root/.cargo/bin:${PATH}" \
    MIX_ENV=prod

VOLUME ["/src", "/data"]

RUN apt-get update && apt-get install -y git curl cmake
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

WORKDIR /src

RUN git clone --branch v0.5 https://kabei@github.com/kabei/ipncore.git

COPY env_file ./env_file
COPY masterlist ./masterlist
COPY whitelist ./whitelist
COPY scripts/run.sh run.sh

RUN mix local.hex --force && mix deps.get && mix local.rebar --force
RUN mix compile
RUN chmod +x run.sh

EXPOSE 4848 5815 8080

ENTRYPOINT ["./run.sh"]
