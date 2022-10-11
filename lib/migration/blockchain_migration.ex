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
        prev bytea,
        hash bytea NOT NULL,
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
      CREATE TABLE IF NOT EXISTS "#{channel}".txs(
        index bytea NOT NULL,
        hash bytea NOT NULL,
        block_index bigint,
        type smallint NOT NULL,
        status smallint NOT NULL,
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
        tid bytea NOT NULL,
        value bigint,
        avail bool NOT NULL DEFAULT FALSE,
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
        id bytea NOT NULL,
        name character varying,
        type smallint,
        group bytea,
        enabled boolean DEFAULT TRUE,
        decimals integer DEFAULT 0,
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
        tid bytea NOT NULL,
        amount numeric(36,0) DEFAULT 0 NOT NULL,
        out_count numeric(36,0) DEFAULT 0,
        in_count numeric(36,0) DEFAULT 0,
        tx_count numeric(36,0) DEFAULT 0,
        created_at bigint NOT NULL,
        update bigint,
        CONSTRAINT balances_pkey PRIMARY KEY (address, tid)
      )
      """,
      # """
      # CREATE TABLE IF NOT EXISTS "#{channel}".account(
      #   id bytea NOT NULL,
      #   name character varying(50),
      #   address bytea,
      #   props jsonb,
      #   created_at bigint NOT NULL,
      #   update bigint,
      #   CONSTRAINT account_pkey PRIMARY KEY (id),
      #   CONSTRAINT block_height UNIQUE (name)
      # )
      # """,
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
      create_functions() ++
      create_triggers(channel)
  end

  def down(%{"channel" => channel}) do
    "DROP SCHEMA \"#{channel}\" CASCADE"
  end

  defp create_triggers(channel) do
    [
      """
      CREATE TRIGGER "tg_cancel_approve_txs"
      BEFORE UPDATE
      ON "#{channel}".txs
      FOR EACH ROW
      EXECUTE PROCEDURE sys.cancel_approve_txs('#{channel}');
      """
    ]
  end

  defp create_functions do
    [
      """
      CREATE OR REPLACE FUNCTION sys.utxo(
      _indexes bytea[],
      _schema character varying)
      RETURNS TABLE(id sys.index, tid sys.tid, value bigint, address sys.address) 
      LANGUAGE 'plpgsql'
      COST 100
      VOLATILE PARALLEL UNSAFE
      ROWS 1000

      AS $BODY$
      BEGIN

      execute ('SET search_path to ' || _schema);
      return query SELECT a.id, a.tid, a.value, a.address
      FROM txo as a
      WHERE a.id = ANY (_indexes::bytea[]) and a.avail=true and c.used=false ORDER BY a.id ASC;

      END
      $BODY$;
      """,
      """
      CREATE OR REPLACE FUNCTION sys.uoutputs(
      _address bytea[],
      _tid bytea,
      _total bigint,
      _schema character varying)
      RETURNS TABLE(id sys.index, address sys.address, tid sys.tid, val bigint, bal bigint) 
      LANGUAGE 'plpgsql'
      COST 100
      VOLATILE PARALLEL UNSAFE
      ROWS 1000

      AS $BODY$
      BEGIN
      bal := 0;

      execute ('SET search_path to ' || _schema);
      FOR id, address, tid, val, bal IN
      (SELECT t0."id" AS "id", t0."address" AS "address", t0."tid" AS "tid", t0."value" AS "value",
      SUM(t0."value") OVER (
      ORDER BY t0."id" ASC rows between unbounded preceding and current row) AS "bal"
      FROM txo AS t0
      WHERE (t0.avail = true) AND (t0."tid" = _tid) AND (t0."address" = ANY(_address::bytea[])) AND (t0."used" = false) AND bal < _total
      GROUP BY t0.id, t0.tid, t0.value, t0.address
      ORDER BY t0."id" ASC)
      LOOP
      RETURN NEXT;
      EXIT WHEN bal >= _total;
      END LOOP;
      END
      $BODY$;
      """
      # ,
      # """
      # CREATE OR REPLACE FUNCTION sys.uoutputs_multi(
      #   _address bytea[],
      #   _tid bytea,
      #   _total bigint,
      #   _prefix character varying)
      #   RETURNS TABLE(id bytea, address bytea, tid bytea, val bigint, channel character varying, bal bigint)
      #   LANGUAGE 'plpgsql'
      #   COST 100
      #   VOLATILE PARALLEL UNSAFE
      #   ROWS 1000

      # AS $BODY$
      # declare
      # _schema character varying;

      # BEGIN
      # bal := 0;

      # FOR _schema IN (
      # SELECT a.table_schema
      # FROM information_schema.tables as a
      # WHERE a.table_type='BASE TABLE' and a.table_name='txo' and a.table_schema LIKE _prefix||'%'
      # )
      # LOOP

      # execute ('SET search_path to ' ||'"'|| _schema ||'"');
      # FOR id, address, tid, val, channel, bal IN
      # (SELECT t0."id" AS "id", t0."address" AS "address", t0."tid" AS "tid", t0."value" AS "value",
      # _schema as "channel",SUM(t0."value") OVER (
      # ORDER BY t0."id" ASC rows between unbounded preceding and current row) AS "bal"
      # FROM txo AS t0
      # LEFT OUTER JOIN txi AS t1 ON (t1."oid" = t0."id")

      # WHERE (t0.avail = true) AND (t0."tid" = _tid) AND (t0."address" = ANY(_address::bytea[])) AND (t1."oid" IS NULL) AND bal < _total
      # GROUP BY t0.id, t0.tid, t0.value, t0.address
      # ORDER BY t0."id" ASC)
      # LOOP
      # RETURN NEXT;
      # EXIT WHEN bal >= _total;
      # END LOOP;

      # END LOOP;
      # END
      # $BODY$;
      # """
    ]
  end
end
