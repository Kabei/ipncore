CREATE TABLE IF NOT EXISTS serv(
  id TEXT NOT NULL PRIMARY KEY,
  name TEXT,
  owner TEXT,
  image TEXT,
  descrip TEXT,
  extra BLOB,
  subs BIGINT,
  status INTEGER,
  created_at BIGINT,
  updated_at BIGINT
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS subpay(
  id TEXT NOT NULL,
  payer TEXT NOT NULL,
  token TEXT NOT NULL,
  lastPay BIGINT,
  div BIGINT,
  every BIGINT,
  spent BIGINT,
  extra BLOB,
  status INTEGER,
  created_at BIGINT,
  PRIMARY KEY(id, payer, token)
) WITHOUT ROWID;