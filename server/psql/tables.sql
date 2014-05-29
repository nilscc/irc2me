-- DROP TABLE IF EXISTS backlog;
-- DROP TABLE IF EXISTS network_channels;
-- DROP TABLE IF EXISTS network_servers;
-- DROP TABLE IF EXISTS networks;
-- DROP TABLE IF EXISTS accounts;

CREATE TABLE IF NOT EXISTS accounts
(
  id        serial      PRIMARY KEY,

  login     text        NOT NULL UNIQUE,
  password  bytea       NOT NULL,

  name      text
);

CREATE TABLE IF NOT EXISTS networks
(
  id        serial      PRIMARY KEY,
  account   integer     NOT NULL REFERENCES accounts (id) ON DELETE CASCADE,
  name      text        NOT NULL
);

CREATE TABLE IF NOT EXISTS network_servers
(
  id        serial      PRIMARY KEY,
  network   integer     NOT NULL REFERENCES networks (id) ON DELETE CASCADE,

  address   text        NOT NULL,
  port      integer     NOT NULL,
  use_ssl   boolean     DEFAULT true
);

CREATE TABLE IF NOT EXISTS network_channels
(
  id        serial      PRIMARY KEY,
  network   integer     NOT NULL REFERENCES networks (id) ON DELETE CASCADE,

  name      text        NOT NULL
);

CREATE TABLE IF NOT EXISTS backlog
(
  id        serial      PRIMARY KEY,
  channel   integer     NOT NULL REFERENCES network_channels (id) ON DELETE CASCADE
);
