CREATE TABLE IF NOT EXISTS $table(
  name TEXT PRIMARY KEY NOT NULL,
  owner BLOB NOT NULL,
  email TEXT,
  avatar TEXT,
  records BIGINT DEFAULT 0,
  enabled BOOLEAN DEFAULT TRUE,
  created_at BIGINT NOT NULL,
  renewed_at BIGINT NOT NULL,
  updated_at BIGINT NOT NULL
) WITHOUT ROWID;
    
CREATE UNIQUE INDEX idx_domain_renew ON $table(renewed_at);
  