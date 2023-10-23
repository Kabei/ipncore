--name: insert_env
REPLACE INTO main.env values(?1, ?2);

--name: get_env
SELECT value FROM main.env WHERE name=?1 LIMIT 1;

--name: all_env
SELECT name, value FROM main.env;

--name: delete_env
DELETE FROM main.env WHERE name=?1;


--name: insert_token
INSERT INTO assets.token VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10);

--name: get_token
SELECT * FROM assets.token WHERE id = ? LIMIT 1;

--name: exists_token
SELECT 1 FROM assets.token WHERE id = ?;

--name: owner_token
SELECT 1 FROM assets.token WHERE id = ?1 AND owner = ?2;

--name: total_tokens
SELECT COUNT(1) FROM assets.token;

--name: delete_token
DELETE FROM assets.token WHERE id = ?1 AND owner = ?2;


--name: insert_domain
INSERT INTO assets.domain VALUES(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9);

--name: get_domain
SELECT * FROM assets.domain WHERE name = ?1;

--name: exists_domain
SELECT 1 FROM assets.domain WHERE name = ?1;

--name: owner_domain
SELECT 1 FROM assets.domain WHERE name = ?1 AND owner = ?2;

--name: delete_domain
DELETE FROM assets.domain WHERE name = ?1 AND owner =?2;

--name: renew_domain
UPDATE assets.domain SET renewed_at = renewed_at + ?3, updated_at = ?4 WHERE name=?1 AND owner=?2;

--name: delete_expiry_domain
DELETE FROM assets.domain WHERE renewed_at < ?;


--name: insert_dns
INSERT OR REPLACE INTO dns.dns VALUES(?1, ?2, ?3, ?4, ?5, ?6);

--name: get_dns
SELECT * FROM dns.dns WHERE domain=?1 AND hash=?2;

--name: exists_dns
SELECT 1 FROM dns.dns WHERE domain=?1 AND hash=?2;

--name: delete_hash_dns
DELETE FROM dns.dns WHERE domain = ?1 AND name=?2 AND hash=?3;

--name: delete_type_dns
DELETE FROM dns.dns WHERE domain = ?1 AND name=?2 AND type=?3;

--name: delete_name_dns
DELETE FROM dns.dns WHERE domain = ?1 AND name=?2;

--name: delete_dns
DELETE FROM dns.dns WHERE domain = ?1;


--name: next_block_id
SELECT COALESCE((SELECT id FROM blockchain.block ORDER BY id DESC LIMIT 1) + 1, 0);

--name: insert_block
INSERT INTO blockchain.block values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14);

--name: exists_block
SELECT 1 FROM blockchain.block WHERE id=? LIMIT 1;

--name: exists_local_block
SELECT 1 FROM blockchain.block WHERE creator=? AND height=? LIMIT 1;

--name: get_block
SELECT * FROM blockchain.block WHERE id=? LIMIT 1;

--name: get_pending_block
SELECT * FROM blockchain.block WHERE id IS NULL AND creator=? AND height=? LIMIT 1;

--name: total_pending_blocks
SELECT count(1) FROM blockchain.block WHERE id IS NULL;

--name: total_blocks_created
SELECT count(1) FROM blockchain.block WHERE creator=?;

--name: last_block_created
SELECT height, hash FROM blockchain.block WHERE creator=? ORDER BY height DESC LIMIT 1;

--name: last_block_id
SELECT id FROM blockchain.block ORDER BY id DESC LIMIT 1;

--name: last_block_height_created
SELECT height FROM blockchain.block WHERE creator=? ORDER BY height DESC LIMIT 1;

--name: get_round_blocks
SELECT id FROM blockchain.block WHERE round = ? ORDER BY id ASC;

--name: creator_blocks_hash
SELECT hash FROM blockchain.block WHERE round = ?1 ORDER BY id ASC;

--name: total_round_blocks
SELECT count(1) FROM blockchain.block WHERE round = ?;


--name: insert_round
INSERT INTO blockchain.round VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13);

--name: exists_round
SELECT 1 FROM blockchain.round WHERE id=? LIMIT 1;

--name: get_round
SELECT * FROM blockchain.round WHERE id = ? LIMIT 1;

--name: get_rounds
SELECT * FROM blockchain.round WHERE id >= ?1 LIMIT ?2 OFFSET ?3;

--name: last_round
SELECT id, hash FROM blockchain.round ORDER BY id DESC LIMIT 1;


--name: insert_validator
INSERT INTO blockchain.validator values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13,?14);

--name: get_players
SELECT * FROM blockchain.validator WHERE failures < 6;

--name: get_validator
SELECT * FROM blockchain.validator WHERE id = ? LIMIT 1;

--name: exists_validator
SELECT 1 FROM blockchain.validator WHERE id = ? LIMIT 1;

--name: exists_host_validator
SELECT 1 FROM blockchain.validator WHERE hostname = ?1;

--name: owner_validator
SELECT 1 FROM blockchain.validator WHERE id = ?1 AND owner = ?2;

--name: total_validators
SELECT COUNT(1) FROM blockchain.validator;

--name: next_id_validator
SELECT COALESCE((SELECT id FROM blockchain.validator ORDER BY id DESC LIMIT 1) + 1, 0);

--name: delete_validator
DELETE FROM blockchain.validator WHERE id = ?1;


--name: insert_refund
REPLACE INTO assets.refund VALUES(?1,?2,?3,?4,?5,?6);

--name: exists_refund
SELECT 1 FROM assets.refund WHERE hash = ?1 AND `to` = ?2;

--name: delete_get_refund
DELETE FROM assets.refund WHERE hash = ? RETURNING sender, token, amount;

--name: delete_expiry_refund
DELETE FROM assets.refund WHERE expiry_in < ?1;


--name: insert_jackpot
INSERT INTO blockchain.jackpot VALUES(?1,?2,?3);

--name: get_jackpot
SELECT winner, amount FROM blockchain.jackpot WHERE round_id=? LIMIT 1;