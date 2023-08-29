--name: insert_msg_round
INSERT INTO msg_round VALUES(?1,?2,?3,?4,?5);

--name: insert_msg_block
INSERT INTO msg_block VALUES(?1, ?2, ?3);

--name: exists_msg_block
SELECT 1 FROM msg_block WHERE creator=?1 AND height=?2 LIMIT 1;

--name: delete_msg_block
DELETE FROM msg_block WHERE creator = ?1 AND height = ?2;