--name: delete_expiry
DELETE FROM $table WHERE renewed_at < ?;

--name: owner
SELECT 1 FROM $table WHERE name = ?1 AND owner = ?2;

--name: insert
INSERT INTO $table VALUES(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9);

--name: lookup
SELECT * FROM $table WHERE name = ?1;

--name: exists
SELECT 1 FROM $table WHERE name = ?1;

--name: renew
UPDATE $table SET renewed_at = renewed_at + ?3, updated_at = ?4 WHERE name=?1 AND owner=?2 LIMIT 1;

--name: delete
DELETE FROM $table WHERE name = ?1 AND owner =?2;