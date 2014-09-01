module IRC.Types where

data TLSSettings
  = TLS         -- ^ Use TLS
  | STARTTLS    -- ^ Use TLS via STARTTLS
  | OptionalTLS -- ^ Use CAP command to find out whether or not IRC server
                --   supports TLS
  | Plaintext   -- ^ No TLS at all
