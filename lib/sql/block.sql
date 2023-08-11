CREATE TABLE IF NOT EXISTS $block(
  height BIGINT NOT NULL,
  creator BIGINT NOT NULL,
  hash BLOB NOT NULL,
  prev BLOB,
  hashfile BLOB,
  signature BLOB NOT NULL,
  round BIGINT NOT NULL,
  timestamp BIGINT NOT NULL,
  ev_count INTEGER DEFAULT 0,
  size BIGINT DEFAULT 0,
  error BOOLEAN DEFAULT FALSE,
  PRIMARY KEY(height, creator)
) WITHOUT ROWID;
      
CREATE TABLE IF NOT EXISTS $msg_block(
  validator_id BIGINT,
  round BIGINT,
  creator BIGINT,
  height BIGINT,
  data BLOB,
  timestamp BIGINT,
  PRIMARY KEY(validator_id, creator, height)
) WITHOUT ROWID;