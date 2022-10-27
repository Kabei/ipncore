defmodule Ipncore.Migration.Blockchain do
  use Ipnutils.MigrationHelper

  def up(%{"channel" => channel, "version" => 0} = params) do
    tablespace = Map.get(params, "ts", "pg_default")

    [
      "CREATE SCHEMA IF NOT EXISTS \"#{channel}\"",
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".block(
        index bigint NOT NULL,
        height bigint NOT NULL,
        hash bytea NOT NULL,
        prev bytea,
        mk bytea NOT NULL,
        type smallint,
        "time" bigint,
        vsn integer NOT NULL,
        tx_count integer,
        amount bigint,
        txvol numeric,
        CONSTRAINT block_pkey PRIMARY KEY (index),
        CONSTRAINT block_height UNIQUE (height)
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".event(
        id bytea NOT NULL,
        hash bytea NOT NULL,
        type smallint NOT NULL,
        block_index bigint,
        payload TEXT,
        size integer DEFAULT 0,
        status smallint NOT NULL,
        sigs bytea[],
        "time" bigint NOT NULL,
        version integer NOT NULL
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".txs(
        index bytea NOT NULL,
        hash bytea NOT NULL,
        block_index bigint,
        type smallint NOT NULL,
        status smallint NOT NULL,
        memo boolean DEFAULT FALSE,
        sigs bytea[],
        amount bigint NOT NULL,
        total_input bigint NOT NULL,
        fees bigint NOT NULL,
        size integer NOT NULL,
        vsn integer NOT NULL,
        in_count integer NOT NULL,
        out_count integer NOT NULL,
        "time" bigint,
        CONSTRAINT txs_pkey PRIMARY KEY (index)
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".txo(
        id bytea NOT NULL,
        address bytea,
        tid VARCHAR(64) NOT NULL,
        type VARCHAR,
        value bigint,
        avail boolean DEFAULT FALSE,
        CONSTRAINT txo_pkey PRIMARY KEY (id)
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".txi(
        oid bytea NOT NULL,
        txid bytea NOT NULL,
        key bytea,
        CONSTRAINT txi_pkey PRIMARY KEY (oid),
        CONSTRAINT fk_txi_txs FOREIGN KEY (txid)
        REFERENCES "#{channel}".txs (index) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE CASCADE
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".txd(
        txid bytea NOT NULL,
        data bytea NOT NULL,
        mime character varying(10) NOT NULL,
        CONSTRAINT txd_pkey PRIMARY KEY (txid),
        CONSTRAINT fk_txd_txs FOREIGN KEY (txid)
        REFERENCES "#{channel}".txs (index) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE CASCADE
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".token(
        id VARCHAR(64) NOT NULL,
        name VARCHAR,
        type smallint,
        enabled boolean DEFAULT TRUE,
        decimals integer DEFAULT 0,
        symbol VARCHAR(5),
        creator bytea NOT NULL,
        owner bytea NOT NULL,
        supply bigint DEFAULT 0,
        destroyed bigint DEFAULT 0,
        props jsonb,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT token_pkey PRIMARY KEY (id)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".balances(
        address bytea NOT NULL,
        tid VARCHAR(64) NOT NULL,
        amount numeric(36,0) DEFAULT 0 NOT NULL,
        out_count numeric(36,0) DEFAULT 0,
        in_count numeric(36,0) DEFAULT 0,
        tx_count numeric(36,0) DEFAULT 0,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT balances_pkey PRIMARY KEY (address, tid)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".pools(
          name VARCHAR NOT NULL,
          hostname VARCHAR NOT NULL,
          address bytea NOT NULL,
          enabled boolean DEFAULT TRUE,
          fee double precision NOT NULL,
          percent boolean NOT NULL,
          created_at bigint NOT NULL,
          updated_at bigint NOT NULL,
          CONSTRAINT pools_pkey PRIMARY KEY (hostname)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".domain(
          name VARCHAR NOT NULL,
          address bytea NOT NULL,
          email VARCHAR(64),
          avatar VARCHAR,
          enabled boolean DEFAULT TRUE,
          records integer DEFAULT 0,
          created_at bigint NOT NULL,
          renewed_at bigint NOT NULL,
          updated_at bigint NOT NULL,
          CONSTRAINT pools_pkey PRIMARY KEY (name)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".dns_record(
        domain VARCHAR NOT NULL,
        name VARCHAR(64) NOT NULL,
        type VARCHAR(5) NOT NULL,
        value VARCHAR() NOT NULL,
        ttl integer NOT NULL,
        CONSTRAINT fk_dns_record FOREIGN KEY (domain)
          REFERENCES "#{channel}".domain (name) MATCH SIMPLE
          ON UPDATE NO ACTION
          ON DELETE CASCADE
      )
      """
      # """
      # CREATE TABLE IF NOT EXISTS "#{channel}".account(
      #     username TEXT NOT NULL,
      #     address bytea NOT NULL,
      #     name TEXT,
      #     avatar TEXT,
      #     email TEXT,
      #     phone TEXT,
      #     enabled boolean DEFAULT TRUE,
      #     props jsonb,
      #     created_at bigint NOT NULL,
      #     updated_at bigint NOT NULL,
      #     CONSTRAINT pools_pkey PRIMARY KEY (username)
      # )
      # """
      # """
      # CREATE TABLE IF NOT EXISTS "#{channel}".tx_votes(
      #   id bytea NOT NULL,
      #   index bytea NOT NULL,
      #   address bytea,
      #   sigs bytea,
      #   "time" bigint NOT NULL,
      #   vote boolean,
      #   CONSTRAINT votes_txs_pkey PRIMARY KEY (id),
      #   CONSTRAINT fk_votes_index_txs FOREIGN KEY (index)
      #       REFERENCES "#{channel}".txs (index) MATCH SIMPLE
      #       ON UPDATE NO ACTION
      #       ON DELETE CASCADE
      # )
      # TABLESPACE #{tablespace};
      # """,
      # """
      # CREATE TABLE IF NOT EXISTS "#{channel}".block_votes(
      #   id bytea NOT NULL,
      #   index integer NOT NULL,
      #   address bytea,
      #   sigs bytea,
      #   "time" bigint NOT NULL,
      #   vote boolean ,
      #   CONSTRAINT votes_block__pkey PRIMARY KEY (id),
      #   CONSTRAINT fk_votes_index_block FOREIGN KEY (index)
      #     REFERENCES "#{channel}".block (index) MATCH SIMPLE
      #     ON UPDATE NO ACTION
      #     ON DELETE CASCADE
      #     NOT VALID
      # )
      # TABLESPACE #{tablespace};
      # """
    ] ++
      create_functions()

    # ++ create_triggers(channel)
  end

  def down(%{"channel" => channel}) do
    "DROP SCHEMA \"#{channel}\" CASCADE"
  end

  # defp create_triggers(channel) do
  #   [
  #     """
  #     CREATE TRIGGER "tg_cancel_approve_txs"
  #     BEFORE UPDATE
  #     ON "#{channel}".txs
  #     FOR EACH ROW
  #     EXECUTE PROCEDURE sys.cancel_approve_txs('#{channel}');
  #     """
  #   ]
  # end

  defp create_functions do
    []
  end
end
