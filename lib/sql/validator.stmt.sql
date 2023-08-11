--name: all
SELECT * FROM $table;

--name: owner
SELECT 1 FROM $table WHERE id = ?1 AND owner = ?2;

--name: total
SELECT COUNT(1) FROM $table;

--name: insert
INSERT INTO $table values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12);

--name: lookup
SELECT * FROM $table WHERE id = ?1;

--name: exists
SELECT 1 FROM $table WHERE id = ?1;

--name: delete
DELETE FROM $table WHERE id = ?1;
  