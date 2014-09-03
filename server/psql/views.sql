CREATE OR REPLACE VIEW servers_to_reconnect AS
  SELECT *
    FROM network_servers
   WHERE id IN (   SELECT min(s.id)
                     FROM network_servers as s, networks as n
                    WHERE s.network = n.id
                      AND n.reconnect = TRUE
                 GROUP BY s.network
               );



