CREATE TABLE IF NOT EXISTS $table(
    id TEXT NOT NULL,
    token VARCHAR(20) NOT NULL,
    amount BIGINT DEFAULT 0,
    locked BIGINT DEFAULT 0,
    created_at BIGINT NOT NULL,
    updated_at BIGINT NOT NULL,
    PRIMARY KEY (id, token)
) WITHOUT ROWID;

CREATE TRIGGER IF NOT EXISTS tg_balance_insert_max_amount
BEFORE INSERT ON $table
BEGIN
SELECT CASE WHEN NEW.amount > $maximum_amount
THEN RAISE(ABORT, 'Max value of amount exceeded')
WHEN NEW.locked > $maximum_amount
THEN RAISE(ABORT, 'Max value of locked exceeded')
END;
END;

CREATE TRIGGER IF NOT EXISTS tg_balance_update_max_amount
BEFORE UPDATE ON $table
BEGIN
SELECT CASE WHEN NEW.amount > $maximum_amount
THEN RAISE(ABORT, 'Max value of amount exceeded')
WHEN NEW.locked > $maximum_amount
THEN RAISE(ABORT, 'Max value of locked exceeded')
END;
END;