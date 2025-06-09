module Main where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Functor
import Data.Map (Map)
import GHC.Generics
import Debug.Trace

import GHC.JS.Prim
import GHCJS.Marshal.Internal (pFromJSVal)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.JSString as JSString
import JavaScript.Web.XMLHttpRequest
import Miso
import Miso.Effect
import Miso.String
import Data.Text (Text)
import Data.String.Conversions (ConvertibleStrings(convertString), cs)

import SF
import Types
import qualified Page.EditChar
--------------------------------------------------------------------------------

sf :: Cmd ~> View Action
sf = proc cmd -> do
  let pageSFEvent = case cmd of
        Goto page -> Just (selectPageSF page)
        _         -> Nothing
  page <- installEventSF (constant loadingPage) -< (pageSFEvent, cmd)

  -- TODO this doesn't seem to work everywhere (clicking "outside" of the main
  -- div doesn't trigger clickout).
  returnA -< div_ [onClick (Cmd ClickOut)] [page]

selectPageSF :: Page -> (Cmd ~> View Action)
selectPageSF LoadingPage =
  constant loadingPage
selectPageSF (EditCharPage options) =
  Page.EditChar.pageSF options

loadingPage :: View action
loadingPage = div_ [] [text "Loading..."]

--------------------------------------------------------------------------------

main :: IO ()
main = traceShow (eitherDecodeStrict example :: Either String Spec) $
  startApp App {..}
  where
    initialAction = SendRequest
    model         = Model loadingPage sf
    update        = \action -> traceShow action (updateModel action)
    view          = \(Model v _) -> v
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

foreign import javascript "getCharName"
  getCharNameJSVal :: JSVal

charName :: JSString
charName = pFromJSVal getCharNameJSVal

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = pure m
updateModel SendRequest m =
  m <# fmap GotResponse getEditCharacterPage
updateModel (GotResponse options) m =
  Main.updateSF (Goto (EditCharPage options)) m
updateModel (SendChoiceSubmission (OptionId origin id) choice) m =
  m <# fmap GotResponse (postSubmitChoice (fromMisoString origin) (fromMisoString id) (submitChoiceToJSString choice))
updateModel (Cmd cmd) m =
  Main.updateSF cmd m

updateSF :: Cmd -> Model -> Effect Action Model
updateSF cmd (Model v (SF sf)) =
  noEff $ uncurry Model $ sf (0, cmd)

--------------------------------------------------------------------------------

getEditCharacterPage :: IO CharacterOptions
getEditCharacterPage = do
  let req = Request { reqMethod = GET
                    , reqURI = JSString.concat
                      [ "/api/character/", charName, "/edit_character_page" ]
                    , reqLogin = Nothing
                    , reqHeaders = []
                    , reqWithCredentials = False
                    , reqData = NoData
                    }
  Just encodedOptions <- fmap contents (xhrByteString req)
  case eitherDecodeStrict encodedOptions of
    Left err   -> error
      $ Prelude.concat [err, "\nin:\n", Prelude.map BS.w2c (BS.unpack encodedOptions)]
    Right resp -> return resp

submitChoiceToJSString :: SubmitChoice -> JSString
submitChoiceToJSString (SubmitListChoice choices) = cs (encode choices)
submitChoiceToJSString (SubmitSingletonChoice choice) = fromMisoString choice

postSubmitChoice :: JSString -> JSString -> JSString -> IO CharacterOptions
postSubmitChoice origin id choice = do
  let req = Request { reqMethod = POST
                    , reqURI = JSString.concat
                      ["/api/character/", charName, "/choice"]
                    , reqLogin = Nothing
                    , reqHeaders = []
                    , reqWithCredentials = False
                    , reqData = FormData [ ("origin", StringVal origin)
                                         , ("id", StringVal id)
                                         , ("choice", StringVal choice)
                                         ]
                    }
  Just encodedOptions <- fmap contents (xhrByteString req)
  case eitherDecodeStrict encodedOptions of
    Left err -> error
      $ Prelude.concat [err, "\nin:\n", Prelude.map BS.w2c (BS.unpack encodedOptions)]
    Right resp -> return resp

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing    = Left err
maybeToEither _   (Just res) = Right res

example = "{\"list\": [{\"desc\": \"\", \"opt\": \"dragonborn\"}, {\"desc\": \"\", \"opt\": \"dwarf\"}, {\"desc\": \"\", \"opt\": \"elf\"}, {\"desc\": \"\", \"opt\": \"gnome\"}, {\"desc\": \"\", \"opt\": \"'half-elf'\"}, {\"desc\": \"\", \"opt\": \"'half-orc'\"}, {\"desc\": \"\", \"opt\": \"halfling\"}, {\"desc\": \"\", \"opt\": \"human\"}, {\"desc\": \"\", \"opt\": \"tiefling\"}, {\"desc\": \"\", \"opt\": \"bugbear\"}, {\"desc\": \"\", \"opt\": \"tabaxi\"}, {\"desc\": \"\", \"opt\": \"firbolg\"}], \"spectype\": \"list\"}"

instance ConvertibleStrings LBS.ByteString JSString where
  convertString = JSString.pack . LBS8.unpack
