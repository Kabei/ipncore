
--name: avg_round_time
SELECT TRUNC(AVG(timestamp)) FROM $block WHERE round = ?;

--name: last_block_by_creator
SELECT * FROM $block WHERE creator = ? ORDER BY height DESC;

--name: fetch_uniques
SELECT creator, height FROM $block WHERE round = ?1 ORDER BY creator ASC;

--name: fetch_hash_round
SELECT hash FROM $block WHERE round = ?1 ORDER BY creator ASC;

--name: count_by_round
SELECT count(1) FROM $block WHERE round = ?;

--name: fetch_bft
SELECT validator_id, data FROM $msg_block WHERE round >= ? ORDER BY round ASC, creator ASC;

--name: insert_bft
INSERT INTO $msg_block values(?1,?2,?3,?4,?5,?6);

--name: delete_bft
DELETE FROM $msg_block WHERE round <= ?;

--name: fetch_hash
SELECT hash FROM $block WHERE creator = ?1 AND height BETWEEN ?2 AND ?3 ORDER BY height ASC;

--name: insert
INSERT INTO $block values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11);

--name: lookup
SELECT * FROM $block WHERE height = ?;

--name: delete
DELETE FROM $block WHERE height = ?;

--name: count
SELECT count(1) FROM $block WHERE creator=?;

--name: last
SELECT * FROM $block WHERE creator=? ORDER BY height DESC;  