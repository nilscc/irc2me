module ProtoBuf.Helper where

import           Data.ProtocolBuffers
import           Data.ByteString (ByteString)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as EE
import           Data.Monoid

import qualified Network.IRC.ByteString.Parser as I

--
-- helper
--

splitFrom
  :: Either I.UserInfo ByteString
  -> (Optional a (Value Text), Optional b (Value Text))
splitFrom from =
  case from of
    Left ui -> (putField $ Just $ decodeUtf8 (I.userNick ui), mempty)
    Right s -> (mempty, putField $ Just $ decodeUtf8 s)

putBS :: ByteString -> Optional a (Value Text)
putBS = putField . Just . decodeUtf8

putBSs :: [ByteString] -> Repeated a (Value Text)
putBSs = putField . map decodeUtf8

putBSMaybe :: Maybe ByteString -> Optional a (Value Text)
putBSMaybe = putField . fmap decodeUtf8

decodeUtf8 :: ByteString -> Text
decodeUtf8 = E.decodeUtf8With EE.lenientDecode
