--name: delete_expiry
DELETE FROM $table WHERE expiry_in < ?1;

--name: insert
REPLACE INTO $table VALUES(?1,?2,?3,?4,?5,?6);

--name: lookup
SELECT sender, token, amount FROM $table WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3;

--name: exists
SELECT 1 FROM $table WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3;

--name: delete
DELETE FROM $table WHERE hash = ?;