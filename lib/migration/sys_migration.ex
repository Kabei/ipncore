defmodule Ipncore.Migration.System do
  use Ipnutils.MigrationHelper

  # def up(%{"version" => 1}) do
  #   base_function()
  # end

  def up(%{"version" => version}) do
    time = :erlang.system_time(:millisecond)

    [
      """
      CREATE TABLE IF NOT EXISTS sys.env(
        key character varying NOT NULL,
        value character varying,
        time bigint NOT NULL,
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
        last_index bigint NOT NULL,
        block_count bigint NOT NULL,
        coins numeric,
        tx_count bigint NOT NULL,
        vsn integer NOT NULL,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT chain_pkey PRIMARY KEY (id)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS sys.token(
        id bytea NOT NULL,
        name character varying,
        type smallint,
        group bytea,
        decimals integer DEFAULT 0,
        enabled boolean DEFAULT TRUE,
        creator bytea NOT NULL,
        supply bigint DEFAULT 0,
        destroyed bigint DEFAULT 0,
        props jsonb,
        created_at bigint NOT NULL,
        updated_at bigint,
        CONSTRAINT token_pkey PRIMARY KEY (id)
      )
      """,
      """
      CREATE TABLE IF NOT EXISTS sys.balances(
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
      """
      CREATE TABLE IF NOT EXISTS sys.account(
        id bytea NOT NULL,
        name character varying(50),
        address bytea,
        props jsonb,
        created_at bigint NOT NULL,
        update bigint,
        CONSTRAINT account_pkey PRIMARY KEY (id),
        CONSTRAINT block_height UNIQUE (name)
      )
      """,
      "INSERT INTO sys.env(key, value, added, update) VALUES('version', #{version}, #{time}, #{time})",
      trigger_approved()
    ]

    # base_function()
  end

  def down(_) do
    "DROP SCHEMA sys CASCADE"
  end

  def trigger_approved do
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
  end
end
