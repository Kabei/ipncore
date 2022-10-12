defmodule Ipncore.Migration.System do
  use Ipnutils.MigrationHelper

  # def up(%{"version" => 1}) do
  #   base_function()
  # end

  def up(%{"version" => version}) do
    time = :erlang.system_time(:millisecond)

    [
      "CREATE SCHEMA IF NOT EXISTS sys",
      """
      CREATE TABLE IF NOT EXISTS sys.env(
        key character varying NOT NULL,
        value character varying,
        added bigint NOT NULL,
        update bigint,
        CONSTRAINT env_pkey PRIMARY KEY (key)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS sys.channel(
        id character varying NOT NULL,
        pubkey bytea NOT NULL,
        enabled boolean DEFAULT TRUE,
        genesis_time bigint NOT NULL,
        last_height bigint,
        last_hash bytea,
        block_count bigint NOT NULL,
        coins numeric,
        tx_count bigint NOT NULL,
        vsn integer NOT NULL,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT chain_pkey PRIMARY KEY (id)
      )
      """,
      "INSERT INTO sys.env(key, value, added, update) VALUES('version', #{version}, #{time}, #{time})"
    ] ++
      trigger_approved() ++
      create_functions()
  end

  def down(_) do
    "DROP SCHEMA sys CASCADE"
  end

  def trigger_approved do
    [
      """
      CREATE OR REPLACE FUNCTION sys.cancel_approve_txs()
      RETURNS trigger
      LANGUAGE 'plpgsql'
      VOLATILE
      COST 100
      AS $BODY$
      declare _schema character varying;
      BEGIN
      _schema := TG_ARGV[0];
      execute ('SET search_path to ' || '"' ||_schema ||'"' );
      if TG_OP='UPDATE' THEN ---CHECK IF IT IS AN UPDATE
      ----Verify that Status is not updated to NULL
      IF NEW.status IS NULL THEN
      RAISE EXCEPTION 'ERR_STAT_001';
      END IF;
      ----Verify that a transaction with status 201 cannot be canceled
      IF OLD.status=201 OR (OLD.status>=400 AND OLD.status<500) THEN
      RAISE EXCEPTION 'ERR_STAT_002';
      END IF;
      ---Perform the cancel operation
      IF (NEW.status>=400 AND NEW.status<500) THEN
      delete from txi where txid=NEW.index;
      delete from txo where OLD.index=substring(id::bytea from 1 for length (NEW.index));
      END IF;
      ----Carry out the approval process
      IF (NEW.status=200 OR NEW.status=201) THEN
      update txo set avail=TRUE where OLD.index=substring(id::bytea from 1 for length (NEW.index));
      END IF;
      END IF;
      RETURN NEW;
      END;
      $BODY$;
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
