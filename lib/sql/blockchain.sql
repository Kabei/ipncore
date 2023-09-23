CREATE TABLE IF NOT EXISTS validator(
  id BIGINT PRIMARY KEY NOT NULL,
  hostname VARCHAR(50) UNIQUE NOT NULL,
  port INTEGER NOT NULL,
  name VARCHAR(30) NOT NULL,
  owner BLOB NOT NULL,
  pubkey BLOB NOT NULL,
  net_pubkey BLOB NOT NULL,
  avatar TEXT,
  fee_type TINYINT NOT NULL,
  fee DOUBLE NOT NULL,
  stake BIGINT,
  failures INTEGER,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS block(
  id BIGINT PRIMARY KEY,
  creator BIGINT NOT NULL,
  height BIGINT NOT NULL,
  hash BLOB NOT NULL,
  prev BLOB,
  hashfile BLOB,
  signature BLOB NOT NULL,
  round BIGINT NOT NULL,
  timestamp BIGINT NOT NULL,
  count INTEGER DEFAULT 0,
  rejected INTEGER,
  size BIGINT DEFAULT 0,
  vsn INTEGER,
  UNIQUE(creator, height)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS round(
  id BIGINT PRIMARY KEY NOT NULL,
  hash BLOB NOT NULL,
  prev BLOB,
  creator BIGINT NOT NULL,
  signature BLOB NOT NULL,
  coinbase BIGINT,
  count BIGINT,
  tx_count BIGINT,
  size BIGINT,
  blocks BLOB,
  extra BLOB
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS jackpot(
  round_id BIGINT NOT NULL,
  winner_id BLOB,
  amount BIGINT DEFAULT 0,
  PRIMARY KEY(round_id, winner_id)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS snapshot(
  round_id BIGINT PRIMARY KEY NOT NULL,
  hash BLOB NOT NULL,
  size BIGINT NOT NULL
) WITHOUT ROWID;