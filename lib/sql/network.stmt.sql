--name: insert_node
INSERT INTO nodes VALUES(?1,?2,?3,?4,?5,?6,?7);

--name: get_node
SELECT * FROM nodes WHERE id=? LIMIT 1;

--name: all_nodes
SELECT * FROM nodes;

--name: delete_nodes
DELETE FROM nodes;