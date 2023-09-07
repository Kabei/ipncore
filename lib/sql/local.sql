CREATE TABLE IF NOT EXISTS cluster(
  id BIGINT PRIMARY KEY NOT NULL,
  hostname TEXT NOT NULL,
  port INTEGER DEFAULT 4848,
  name TEXT,
  role TEXT,
  pubkey BLOB,
  net_pubkey BLOB,
  avatar TEXT,
  created_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;