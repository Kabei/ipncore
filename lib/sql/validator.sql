CREATE TABLE IF NOT EXISTS $table(
    id BIGINT PRIMARY KEY NOT NULL,
    hostname VARCHAR(50) UNIQUE NOT NULL,
    name VARCHAR(30) NOT NULL,
    owner BLOB NOT NULL,
    pubkey BLOB NOT NULL,
    net_pubkey BLOB NOT NULL,
    avatar TEXT,
    fee_type TINYINT NOT NULL,
    fee DOUBLE NOT NULL,
    stake BIGINT NOT NULL DEFAULT 0,
    created_at BIGINT NOT NULL,
    updated_at BIGINT NOT NULL
);