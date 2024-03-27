CREATE TABLE IF NOT EXISTS nodes(
  id TEXT PRIMARY KEY NOT NULL,
  hostname TEXT NOT NULL,
  port INTEGER,
  class TEXT,
  pubkey BLOB,
  net_pubkey BLOB,
  image TEXT,
  created_at BIGINT,
  updated_at BIGINT
) WITHOUT ROWID;

CREATE INDEX IF NOT EXISTS idx_nodes_class ON nodes(class);