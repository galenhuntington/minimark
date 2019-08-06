--+
import BasePrelude

import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Builder

import Text.XmlHtml
import Text.Minimark (minimark)

main = do
   val <- either (error . show) id <$> parseHTML "" <$> B.getContents
   print val
   BL.putStr $ toLazyByteString $ render $ minimark $ val

