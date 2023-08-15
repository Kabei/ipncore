--name: insert
INSERT INTO $table VALUES(?1,?2,?3,?4,?5,?6);

--name: balance
SELECT 1 FROM $table WHERE id = ?1 AND token = ?2 AND amount >= ?3 LIMIT 1;

--name: lookup
SELECT * FROM $table WHERE id = ?1 AND token = ?2 LIMIT 1;

--name: exists
SELECT 1 FROM $table WHERE id = ?1 AND token = ?2 LIMIT 1;

--name: delete
DELETE FROM $table WHERE id = ?1 AND token = ?2 LIMIT 1;

--name: send
UPDATE $table SET amount = amount - ?3, updated_at = ?4 WHERE id = ?1 AND token = ?2 AND amount >= ?3 LIMIT 1;

--name: income
INSERT INTO $table (id,token,amount,created_at,updated_at)
VALUES(?1, ?2, ?3, ?4, ?4) ON CONFLICT (id, token)
DO UPDATE SET amount = amount + ?3, updated_at = ?4
WHERE id = ?1 AND token = ?2 LIMIT 1;

--name: lock
UPDATE $table SET amount = amount - ?3, locked = locked + ?3 WHERE id = ?1 AND token = ?2 AND amount >= ?3 LIMIT 1;

--name: unlock
UPDATE $table SET amount = amount + ?3, locked = locked - ?3 WHERE id = ?1 AND token = ?2 AND locked >= ?3 LIMIT 1;