CREATE TABLE IF NOT EXISTS serv(
  id TEXT NOT NULL PRIMARY KEY,
  name TEXT NOT NULL,
  extra BLOB,
  created_at BIGINT,
  updated_at BIGINT
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS subpay(
  id TEXT NOT NULL,
  payer TEXT NOT NULL,
  token TEXT NOT NULL,
  created_at BIGINT,
  last_round BIGINT,
  extra BLOB,
  PRIMARY KEY(id, payer, token)
) WITHOUT ROWID;