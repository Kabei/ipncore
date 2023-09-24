CREATE TABLE IF NOT EXISTS token(
  id VARCHAR(20) PRIMARY KEY NOT NULL,
  owner BLOB NOT NULL,
  name TEXT NOT NULL,
  avatar TEXT,
  decimal TINYINT DEFAULT 0,
  symbol VARCHAR(5) NOT NULL,  
  max_supply BLOB,
  props BLOB,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS domain(
  name TEXT PRIMARY KEY NOT NULL,
  owner BLOB NOT NULL,
  email TEXT,
  avatar TEXT,
  records BIGINT DEFAULT 0,
  enabled BOOLEAN DEFAULT TRUE,
  created_at BIGINT NOT NULL,
  renewed_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;
    
CREATE INDEX IF NOT EXISTS idx_domain_renew ON domain(renewed_at);

CREATE TABLE IF NOT EXISTS refund(
  hash BLOB PRIMARY KEY NOT NULL,
  sender BLOB NOT NULL,
  `to` BLOB NOT NULL,
  token TEXT NOT NULL,
  amount BIGINT NOT NULL,
  expiry_in BIGINT NOT NULL
) WITHOUT ROWID;
