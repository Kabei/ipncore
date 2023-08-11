--name: delete_hash
DELETE FROM $table WHERE domain = ?1 AND name=?2 AND hash=?3;

--name: delete_type
DELETE FROM $table WHERE domain = ?1 AND name=?2 AND type=?3;

--name: delete_name
DELETE FROM $table WHERE domain = ?1 AND name=?2;

--name: delete
DELETE FROM $table WHERE domain = ?1;

--name: insert
INSERT OR REPLACE INTO $table VALUES(?1, ?2, ?3, ?4, ?5, ?6);

--name: lookup
SELECT * FROM $table WHERE domain=?1 AND hash=?2;

--name: exists
SELECT 1 FROM $table WHERE domain=?1 AND hash=?2;