module Main where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
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

import SF
import Types
import qualified Page.EditChar
--------------------------------------------------------------------------------

sf :: Cmd ~> View Action
sf = proc cmd -> do
  let pageSFEvent = case cmd of
        Goto page -> Just (selectPageSF page)
        _         -> Nothing
  installEventSF (constant loadingPage) -< (pageSFEvent, cmd)

selectPageSF :: Page -> (Cmd ~> View Action)
selectPageSF LoadingPage =
  constant loadingPage
selectPageSF (EditCharPage options) =
  Page.EditChar.pageSF options

loadingPage :: View action
loadingPage = div_ [] [text "Loading..."]

--------------------------------------------------------------------------------

main :: IO ()
main = startApp App {..}
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
  Just resp <- fmap ((decodeStrict =<<) . contents) (xhrByteString req)
  return resp
