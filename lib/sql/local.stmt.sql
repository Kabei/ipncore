--name: insert_node
INSERT INTO nodes VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9);

--name: get_node
SELECT * FROM nodes WHERE id=? LIMIT 1;

--name: exists_node
SELECT 1 FROM nodes WHERE id=? LIMIT 1;

--name: total_nodes
SELECT COUNT(1) FROM nodes;

--name: all_nodes
SELECT * FROM nodes;

--name: delete_nodes
DELETE FROM nodes;