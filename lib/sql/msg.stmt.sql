--name: all_df
SELECT * FROM $table_df;

--name: all_hash
SELECT * FROM $table_hash;

--name: select
SELECT hash, timestamp, type, account_id, validator_id, node_id, args, message, signature, size, ROWID
      FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, ROWID, *  FROM $table)
      WHERE total <= ?1 AND node_id = ?2 ORDER BY hash;

--name: select_df
SELECT hash, timestamp, key, type, account_id, validator_id, node_id, args, message, signature, size, ROWID
      FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, ROWID, * FROM $table_df WHERE round IS NULL)
      WHERE total <= ?1 AND node_id = ?2 ORDER BY hash;

--name: select_all_df_approved
SELECT hash, timestamp, key, type, account_id, validator_id, node_id, args, message, signature, size FROM $table_df WHERE round = ?1 ORDER BY timestamp, hash;

--name: delete_all
DELETE FROM $table WHERE ROWID <= ?1;

--name: delete_all_df
DELETE FROM $table_df WHERE ROWID <= ?1 AND round IS NULL;

--name: delete_all_df_approved
DELETE FROM $table_df WHERE round = ?1;

--name: delete_hash
DELETE FROM $table WHERE hash = ?;

--name: delete_hash
DELETE FROM $table WHERE hash = ?;

--name: delete_hash_df
DELETE FROM $table_df WHERE ROWID <= ?1 AND round IS NULL;

--name: delete_df
DELETE FROM $table_df WHERE timestamp = ? AND hash = ?;

--name: approve_df
UPDATE $table_df SET round=?1 WHERE timestamp = ?2 AND hash = ?3 LIMIT 1;

--name: insert_df
INSERT INTO $table_df (hash,timestamp,key,type,account_id,validator_id,node_id,args,message,signature,size) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)
    ON CONFLICT (key,type) DO UPDATE SET timestamp=?3, hash=?4, account_id=?5, validator_id=?6, node_id=?7, args=?8, message=?9, signature=?10, size=?11
    WHERE timestamp > EXCLUDED.timestamp OR timestamp = EXCLUDED.timestamp AND hash > EXCLUDED.hash LIMIT 1;

--name: delete_expiry
DELETE FROM $table_hash WHERE timestamp < (? - $expiry_time);

--name: insert
INSERT INTO $table VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10);
