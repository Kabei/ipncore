--name: insert
INSERT INTO $table VALUES(?1,?2,?3,?4);

--name: validator
SELECT pubkey, validator FROM $table WHERE id=?1 AND validator=?2;

--name: lookup
SELECT * FROM $table WHERE id=?;

--name: exists
SELECT 1 FROM $table WHERE id=?;

--name: delete
DELETE FROM $table WHERE id=?;

--name: jackpot
SELECT pos, id FROM (SELECT ROW_NUMBER() OVER () AS pos, id FROM $table ORDER BY created_at ASC) WHERE pos = ?;

--name: total
SELECT count(1) FROM $table;  