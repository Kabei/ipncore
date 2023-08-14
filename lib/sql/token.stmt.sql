--name: sum_supply
UPDATE $table SET supply = supply + ?2 WHERE id = ?1 AND (max_supply = 0 OR max_supply >= supply + ?2) LIMIT 1;

--name: sum_burned
UPDATE $table SET burned = burned + ?2, supply = supply - ?2 WHERE id = ?1 LIMIT 1;

--name: owner_props
SELECT 1 FROM $table WHERE id = ?1 AND owner = ?2 AND props LIKE ?3;

--name: props
SELECT 1 FROM $table WHERE id = ?1 AND props LIKE ?2;

--name: insert
INSERT INTO $table VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13);

--name: lookup
SELECT * FROM $table WHERE id = ?;

--name: exists
SELECT 1 FROM $table WHERE id = ?;

--name: delete
DELETE FROM $table WHERE id = ?1 AND owner = ?2 AND supply = 0 AND burned = 0;

--name: owner
SELECT 1 FROM $table WHERE id = ?1 AND owner = ?2;

--name: total
SELECT COUNT(1) FROM $table;