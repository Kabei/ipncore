--name: insert
REPLACE INTO env values(?1, ?2, ?3);

--name: delete
DELETE FROM env WHERE name=?1;

--name: lookup
SELECT value FROM env WHERE name=?1;  