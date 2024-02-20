CREATE TABLE IF NOT EXISTS validator(
  id BIGINT PRIMARY KEY NOT NULL,
  hostname VARCHAR(50) UNIQUE NOT NULL,
  port INTEGER NOT NULL,
  name VARCHAR(30) NOT NULL,
  owner BLOB NOT NULL,
  class TEXT,
  pubkey BLOB NOT NULL,
  net_pubkey BLOB NOT NULL,
  avatar TEXT,
  fa INTEGER NOT NULL,
  fb INTEGER NOT NULL,
  active BOOLEAN,
  failures INTEGER,
  env BLOB,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS idx_val_class ON validator(class);

CREATE TABLE IF NOT EXISTS token(
  id VARCHAR(20) PRIMARY KEY NOT NULL,
  owner BLOB NOT NULL,
  name TEXT NOT NULL,
  avatar TEXT,
  decimal TINYINT DEFAULT 0,
  symbol VARCHAR(5) NOT NULL,  
  max_supply BLOB,
  props BLOB,
  env BLOB,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS refund(
  hash BLOB PRIMARY KEY NOT NULL,
  sender BLOB NOT NULL,
  `to` BLOB NOT NULL,
  token TEXT NOT NULL,
  amount BIGINT NOT NULL,
  expiry_in BIGINT NOT NULL
) WITHOUT ROWID;
