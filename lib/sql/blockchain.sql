CREATE TABLE IF NOT EXISTS block(
  id BIGINT PRIMARY KEY,
  creator BIGINT NOT NULL,
  height BIGINT NOT NULL,
  hash BLOB NOT NULL,
  prev BLOB,
  filehash BLOB,
  signature BLOB NOT NULL,
  round BIGINT NOT NULL,
  timestamp BIGINT NOT NULL,
  count INTEGER DEFAULT 0,
  rejected INTEGER,
  size BIGINT DEFAULT 0,
  status INTEGER,
  vsn INTEGER,
  UNIQUE(creator, height)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS round(
  id BIGINT PRIMARY KEY NOT NULL,
  hash BLOB,
  prev BLOB,
  creator BIGINT NOT NULL,
  signature BLOB,
  reward BIGINT,
  count BIGINT,
  tx_count BIGINT,
  size BIGINT,
  status INTEGER,
  timestamp BIGINT NOT NULL,
  blocks BLOB,
  extra BLOB
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS jackpot(
  round_id BIGINT NOT NULL,
  winner BLOB,
  amount BIGINT DEFAULT 0,
  PRIMARY KEY(round_id, winner)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS snapshot(
  round_id BIGINT PRIMARY KEY NOT NULL,
  hash BLOB NOT NULL,
  size BIGINT NOT NULL
) WITHOUT ROWID;