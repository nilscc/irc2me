{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans #-} -- FIXME

module IRC.ProtoBuf.Instances where

import Data.Foldable
import Data.Monoid

-- FIXME: https://github.com/alphaHeavy/protobuf/issues/3
deriving instance Foldable Last
