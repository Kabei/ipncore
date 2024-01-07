--name: insert_node
REPLACE INTO nodes VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9);

--name: get_node
SELECT * FROM nodes WHERE id=? LIMIT 1;

--name: exists_node
SELECT 1 FROM nodes WHERE id=? LIMIT 1;

--name: last_mod
SELECT updated_at FROM nodes ORDER BY updated_at DESC LIMIT 1;

--name: total_nodes
SELECT COUNT(1) FROM nodes;

--name: all_nodes
SELECT * FROM nodes;

--name: delete_nodes
DELETE FROM nodes;