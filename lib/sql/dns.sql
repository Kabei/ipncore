CREATE TABLE IF NOT EXISTS domain(
  name TEXT PRIMARY KEY NOT NULL,
  owner BLOB NOT NULL,
  email TEXT,
  image TEXT,
  records BIGINT DEFAULT 0,
  created_at BIGINT NOT NULL,
  renewed_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;
    
CREATE INDEX IF NOT EXISTS idx_domain_renew ON domain(renewed_at);

CREATE TABLE IF NOT EXISTS dns(
  domain TEXT NOT NULL,
  name TEXT NOT NULL,
  type TINYINT NOT NULL,
  data TEXT,
  ttl INTEGER DEFAULT 0,
  hash BLOB NOT NULL,
  PRIMARY KEY(domain, hash)
) WITHOUT ROWID;