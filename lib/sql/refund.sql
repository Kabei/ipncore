CREATE TABLE IF NOT EXISTS $table(
    hash BLOB PRIMARY KEY NOT NULL,
    sender BLOB NOT NULL,
    `to` BLOB NOT NULL,
    token TEXT NOT NULL,
    amount BIGINT NOT NULL,
    expiry_in BIGINT NOT NULL
  ) WITHOUT ROWID;