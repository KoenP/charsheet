module Widget where

--------------------------------------------------------------------------------
import Control.Lens
import Data.Aeson
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, intercalate, pack)
import Reflex.Class
import Reflex.Dom

import Types (ReactiveM, ReactiveIOM)
import Util ((|->), errorOnLeft)
--------------------------------------------------------------------------------

dynAttrButton :: ReactiveM t m => Dynamic t (Map Text Text) -> Text -> m (Event t ())
dynAttrButton attrsDyn label = mdo
  (buttonWidget, _) <- elDynAttr' "button" attrsDyn (text label)
  return $ domEvent Click buttonWidget

dynClassButton :: ReactiveM t m => Dynamic t [Text] -> Text -> m (Event t ())
dynClassButton classDyn label = dynAttrButton (("class" |->) . intercalate " " <$> classDyn) label

uncondCondClasses :: Reflex t => [Text] -> [Text] -> Dynamic t Bool -> Dynamic t [Text]
uncondCondClasses unconditional conditional dyn = dyn <&> (\b -> unconditional <> if b then conditional else [])

loadWidget :: (ReactiveIOM t m, IsXhrPayload x, FromJSON a)
           => b -> XhrRequest x -> (a -> m b) -> m (Dynamic t b)
loadWidget nullVal req k = do
  postBuildE <- getPostBuild
  resE <- performRequestAsync (req <$ postBuildE)
  let loadedWidgetE
         =  k
         .  errorOnLeft . eitherDecodeStrictText
         .  fromJust . view xhrResponse_responseText
        <$> resE
  widgetHold (text "Loading..." >> pure nullVal) loadedWidgetE

domShow :: (DomBuilder t m, Show a) => a -> m ()
domShow = text . pack . show

divcl :: DomBuilder t m => Text -> m a -> m a
divcl = elClass "div"
