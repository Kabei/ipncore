CREATE TABLE IF NOT EXISTS $block(
  height BIGINT NOT NULL,
  creator BIGINT NOT NULL,
  hash BLOB NOT NULL,
  prev BLOB,
  hashfile BLOB,
  signature BLOB NOT NULL,
  round BIGINT NOT NULL,
  timestamp BIGINT NOT NULL,
  count INTEGER DEFAULT 0,
  size BIGINT DEFAULT 0,
  error BOOLEAN DEFAULT FALSE,
  vsn integer,
  PRIMARY KEY(height, creator)
) WITHOUT ROWID;