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
        ifees bytea[],
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
        mime character varying(5) NOT NULL,
        CONSTRAINT txd_pkey PRIMARY KEY (txid),
        CONSTRAINT fk_txd_txs FOREIGN KEY (txid)
        REFERENCES "#{channel}".txs (index) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE CASCADE
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".tx_votes(
        id bytea NOT NULL,
        index bytea NOT NULL,
        address bytea,
        sigs bytea,
        "time" bigint NOT NULL,
        vote boolean,
        CONSTRAINT votes_txs_pkey PRIMARY KEY (id),
        CONSTRAINT fk_votes_index_txs FOREIGN KEY (index)
            REFERENCES "#{channel}".txs (index) MATCH SIMPLE
            ON UPDATE NO ACTION
            ON DELETE CASCADE
      )
      TABLESPACE #{tablespace};
      """,
      """
      CREATE TABLE IF NOT EXISTS "#{channel}".block_votes(
        id bytea NOT NULL,
        index integer NOT NULL,
        address bytea,
        sigs bytea,
        "time" bigint NOT NULL,
        vote boolean ,
        CONSTRAINT votes_block__pkey PRIMARY KEY (id),
        CONSTRAINT fk_votes_index_block FOREIGN KEY (index)
          REFERENCES "#{channel}".block (index) MATCH SIMPLE
          ON UPDATE NO ACTION
          ON DELETE CASCADE
          NOT VALID
      )
      TABLESPACE #{tablespace};
      """
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
          RETURNS TABLE(id bytea, tid bytea, value bigint, address bytea)
          LANGUAGE 'plpgsql'
          COST 100
          VOLATILE PARALLEL UNSAFE
          ROWS 1000

      AS $BODY$
      BEGIN

      execute ('SET search_path to ' || _schema);
      return query SELECT a.id, a.tid, a.value, a.address
          FROM txo as a
          left outer join txi as c on a.id=c.oid
          WHERE a.id = ANY (_indexes::bytea[]) and a.avail=true ORDER BY a.id ASC;

      END
      $BODY$;
      """,
      """
      CREATE OR REPLACE FUNCTION sys.uoutputs(
        _address bytea[],
        _tid bytea,
        _total bigint,
        _schema character varying)
        RETURNS TABLE(id bytea, address bytea, tid bytea, val bigint, bal bigint)
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
      LEFT OUTER JOIN txi AS t1 ON (t1."oid" = t0."id")

      WHERE (t0.avail = true) AND (t0."tid" = _tid) AND (t0."address" = ANY(_address::bytea[])) AND (t1."oid" IS NULL) AND bal < _total
      GROUP BY t0.id, t0.tid, t0.value, t0.address
      ORDER BY t0."id" ASC)
      LOOP
      RETURN NEXT;
      EXIT WHEN bal >= _total;
      END LOOP;
      END
      $BODY$;
      """,
      """
      CREATE OR REPLACE FUNCTION sys.uoutputs_multi(
        _address bytea[],
        _tid bytea,
        _total bigint,
        _prefix character varying)
        RETURNS TABLE(id bytea, address bytea, tid bytea, val bigint, channel character varying, bal bigint)
        LANGUAGE 'plpgsql'
        COST 100
        VOLATILE PARALLEL UNSAFE
        ROWS 1000

      AS $BODY$
      declare
      _schema character varying;

      BEGIN
      bal := 0;

      FOR _schema IN (
      SELECT a.table_schema
      FROM information_schema.tables as a
      WHERE a.table_type='BASE TABLE' and a.table_name='txo' and a.table_schema LIKE _prefix||'%'
      )
      LOOP

      execute ('SET search_path to ' ||'"'|| _schema ||'"');
      FOR id, address, tid, val, channel, bal IN
      (SELECT t0."id" AS "id", t0."address" AS "address", t0."tid" AS "tid", t0."value" AS "value",
      _schema as "channel",SUM(t0."value") OVER (
      ORDER BY t0."id" ASC rows between unbounded preceding and current row) AS "bal"
      FROM txo AS t0
      LEFT OUTER JOIN txi AS t1 ON (t1."oid" = t0."id")

      WHERE (t0.avail = true) AND (t0."tid" = _tid) AND (t0."address" = ANY(_address::bytea[])) AND (t1."oid" IS NULL) AND bal < _total
      GROUP BY t0.id, t0.tid, t0.value, t0.address
      ORDER BY t0."id" ASC)
      LOOP
      RETURN NEXT;
      EXIT WHEN bal >= _total;
      END LOOP;

      END LOOP;
      END
      $BODY$;
      """
    ]
  end
end
