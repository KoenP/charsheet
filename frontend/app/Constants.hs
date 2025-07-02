module Constants
  (charName)
where

--------------------------------------------------------------------------------
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import Data.Text (Text, pack)
import qualified Data.Text as Text
--------------------------------------------------------------------------------

foreign import javascript "getCharName"
  js_getCharName :: JSString

charName :: Text
charName = pack $ JSString.unpack js_getCharName
