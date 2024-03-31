CREATE TABLE IF NOT EXISTS env(
    name TEXT PRIMARY KEY NOT NULL,
    value BLOB
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS acl(
  asset TEXT,
  account TEXT,
  level BLOB,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL,
  PRIMARY KEY(asset, account)
) WITHOUT ROWID;