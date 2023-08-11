--name: has_winner
SELECT 1 FROM $jackpot WHERE round_id = ?;

--name: insert_winner
INSERT INTO $jackpot values(?, ?, ?);

--name: insert_snap
INSERT INTO $snapshot values(?, ?, ?);

--name: insert
INSERT INTO $round values(?1,?2,?3,?4,?5,?6);

--name: lookup
SELECT * FROM $round WHERE id = ?;

--name: exists
SELECT 1 FROM $round WHERE id = ?;

--name: last
SELECT * FROM $round ORDER BY id DESC LIMIT 1;

--name: delete
DELETE FROM $round WHERE id = ?;

--name: total
SELECT COUNT(1) FROM $round;
