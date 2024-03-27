CREATE TABLE IF NOT EXISTS validator(
  id TEXT PRIMARY KEY NOT NULL,
  hostname VARCHAR(50) UNIQUE NOT NULL,
  port INTEGER NOT NULL,
  name VARCHAR(30) NOT NULL,
  owner TEXT NOT NULL,
  class TEXT,
  pubkey BLOB NOT NULL,
  net_pubkey BLOB NOT NULL,
  image TEXT,
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
  id TEXT PRIMARY KEY NOT NULL,
  owner TEXT NOT NULL,
  name TEXT NOT NULL,
  image TEXT,
  decimal TINYINT DEFAULT 0,
  symbol VARCHAR(5) NOT NULL,  
  max_supply BLOB,
  props BLOB,
  env BLOB,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS refund(
  sender TEXT NOT NULL,
  nonce BIGINT NOT NULL,
  `to` TEXT NOT NULL,
  token TEXT NOT NULL,
  amount BIGINT NOT NULL,
  expiry_in BIGINT NOT NULL,
  PRIMARY KEY (sender, nonce)
) WITHOUT ROWID;
