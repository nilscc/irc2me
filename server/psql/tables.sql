-- DROP TABLE IF EXISTS network_backlog;
-- DROP TABLE IF EXISTS network_channels;
-- DROP TABLE IF EXISTS network_servers;
-- DROP TABLE IF EXISTS networks;
-- 
-- DROP TABLE IF EXISTS account_identities;
-- DROP TABLE IF EXISTS accounts;

--------------------------------------------------------------------------------
-- accounts

CREATE TABLE IF NOT EXISTS accounts
(
  id        serial      PRIMARY KEY,

  login     text        NOT NULL UNIQUE,
  password  bytea       NOT NULL
);

CREATE TABLE IF NOT EXISTS account_identities
(
  id        serial      PRIMARY KEY,
  account   integer     NOT NULL REFERENCES accounts (id) ON DELETE CASCADE,

  username  text        NOT NULL,
  realname  text        NOT NULL,
  nick      text        NOT NULL,
  nick_alt  text[]       
);

--------------------------------------------------------------------------------
-- networks

CREATE TABLE IF NOT EXISTS networks
(
  id        serial      PRIMARY KEY,
  account   integer     NOT NULL REFERENCES accounts (id) ON DELETE CASCADE,
  name      text        NOT NULL,

  identity  integer     REFERENCES account_identities (id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS network_servers
(
  id        serial      PRIMARY KEY,
  network   integer     NOT NULL REFERENCES networks (id) ON DELETE CASCADE,

  address   text        NOT NULL,
  port      integer     NOT NULL,
  use_ssl   boolean     NOT NULL DEFAULT true
);

CREATE TABLE IF NOT EXISTS network_channels
(
  id        serial      PRIMARY KEY,
  network   integer     NOT NULL REFERENCES networks (id) ON DELETE CASCADE,

  name      text        NOT NULL
);

CREATE TABLE IF NOT EXISTS network_backlog
(
  id        serial      PRIMARY KEY,
  channel   integer     NOT NULL REFERENCES network_channels (id) ON DELETE CASCADE
);
