defmodule Ipncore.Migration.Blockchain do
  use Ipnutils.MigrationHelper

  def up(%{"channel" => channel, "version" => 0} = params) do
    tablespace = Map.get(params, "ts", "pg_default")

    [
      "CREATE SCHEMA IF NOT EXISTS \"#{channel}\"",
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".block(
        height bigint NOT NULL,
        time bigint,
        type smallint,
        hash bytea NOT NULL,
        prev bytea,
        mk bytea NOT NULL,
        vsn integer NOT NULL,
        ev_count integer,
        txvol numeric,
        CONSTRAINT block_pk PRIMARY KEY (height)
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".event(
        time bigint NOT NULL,
        hash bytea NOT NULL,
        type smallint NOT NULL,
        block_index bigint,
        sig_count smallint DEFAULT 0,
        size integer DEFAULT 0,
        vsn integer NOT NULL
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".tx(
        id bytea NOT NULL,
        fee bigint NOT NULL,
        out_count integer NOT NULL,
        token_value jsonb,
        memo varchar(100)
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".txo(
        txid bytea NOT NULL,
        ix integer NOT NULL,
        token varchar(64) NOT NULL,
        "from" bytea,
        "to" bytea,
        value bigint,
        reason char(1)
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".token(
        id varchar(64) NOT NULL,
        name varchar,
        enabled bool DEFAULT TRUE,
        decimals integer DEFAULT 0,
        symbol varchar(5),
        owner bytea NOT NULL,
        avatar varchar,
        supply bigint DEFAULT 0,
        burned bigint DEFAULT 0,
        props jsonb,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT token_pk PRIMARY KEY (id)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".balances(
        address bytea NOT NULL,
        token varchar(64) NOT NULL,
        amount numeric DEFAULT 0 NOT NULL,
        locked bool DEFAULT FALSE NOT NULL,
        out_count numeric DEFAULT 0,
        in_count numeric DEFAULT 0,
        tx_count numeric DEFAULT 0,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT balances_pk PRIMARY KEY (address, token)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".validator(
          host varchar NOT NULL,
          name varchar NOT NULL,
          owner bytea NOT NULL,
          avatar varchar,
          enabled bool DEFAULT TRUE,
          fee double precision NOT NULL,
          fee_type smallint NOT NULL,
          created_at bigint NOT NULL,
          updated_at bigint NOT NULL,
          CONSTRAINT validator_pk PRIMARY KEY (host)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".domain(
          name varchar(25) NOT NULL,
          owner bytea NOT NULL,
          email varchar(64),
          avatar varchar,
          enabled bool DEFAULT TRUE,
          forsale bool DEFAULT FALSE,
          records integer DEFAULT 0,
          created_at bigint NOT NULL,
          renewed_at bigint NOT NULL,
          updated_at bigint NOT NULL,
          CONSTRAINT domain_pk PRIMARY KEY (name)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".dns_record(
        domain varchar NOT NULL,
        type varchar(5) NOT NULL,
        value varchar(64) NOT NULL,
        ttl integer NOT NULL,
        root varchar(25) NOT NULL
      )
      """
      # """
      # CREATE TABLE IF NOT EXISTS "#{channel}".tx_votes(
      #   id bytea NOT NULL,
      #   index bytea NOT NULL,
      #   address bytea,
      #   sigs bytea,
      #   time bigint NOT NULL,
      #   vote bool,
      #   CONSTRAINT votes_txs_pk PRIMARY KEY (id),
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
      #   time bigint NOT NULL,
      #   vote bool,
      #   CONSTRAINT votes_block__pk PRIMARY KEY (id),
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
