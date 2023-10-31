CREATE TABLE IF NOT EXISTS nodes(
  id TEXT PRIMARY KEY NOT NULL,
  hostname TEXT NOT NULL,
  port INTEGER,
  role TEXT,
  pubkey BLOB,
  net_pubkey BLOB,
  avatar TEXT,
  created_at BIGINT,
  updated_at BIGINT
) WITHOUT ROWID;