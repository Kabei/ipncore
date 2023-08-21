CREATE TABLE IF NOT EXISTS $table(
  hash BLOB,
  timestamp BIGINT,
  type INTEGER,
  account_id BLOB,
  validator_id BIGINT,
  node_id BIGINT,
  args BLOB,
  message BLOB,
  signature BLOB,
  size INTEGER DEFAULT 0,
  PRIMARY KEY(timestamp, hash)
);

CREATE TABLE IF NOT EXISTS $table_df(
  hash BLOB,
  timestamp BIGINT,
  key BLOB,
  type INTEGER,
  account_id BLOB,
  validator_id BIGINT,
  node_id BIGINT,
  args BLOB,
  message BLOB,
  signature BLOB,
  size INTEGER DEFAULT 0,
  round BIGINT,
  PRIMARY KEY(timestamp, hash),
  UNIQUE(key, type)
);

CREATE TABLE IF NOT EXISTS msgd_approved(
  hash BLOB,
  timestamp BIGINT,
  key BLOB,
  type INTEGER,
  account_id BLOB,
  validator_id BIGINT,
  node_id BIGINT,
  args BLOB,
  message BLOB,
  signature BLOB,
  size INTEGER DEFAULT 0,
  round BIGINT,
  PRIMARY KEY(timestamp, hash),
  UNIQUE(key, type)
);

CREATE TABLE IF NOT EXISTS $table_hash(
  hash BLOB NOT NULL,
  validator_id BIGINT NOT NULL,
  timestamp BIGINT,
  PRIMARY KEY(hash, validator_id)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS $table_hashd(
  hash BLOB PRIMARY KEY NOT NULL,
  timestamp BIGINT
) WITHOUT ROWID;

CREATE TRIGGER IF NOT EXISTS tg_insert_hash_msg BEFORE INSERT ON $table
BEGIN
INSERT INTO $table_hash VALUES(NEW.hash, NEW.node_id, NEW.timestamp);
END;

CREATE TRIGGER IF NOT EXISTS tg_insert_hash_msgd BEFORE INSERT ON $table_df
BEGIN
INSERT INTO $table_hashd VALUES(NEW.hash, NEW.timestamp);
END;