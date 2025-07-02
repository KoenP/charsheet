module Widget where

--------------------------------------------------------------------------------
import Control.Monad.Fix
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text (Text, intercalate)
import Reflex.Class
import Reflex.Dom

import Types (ReactiveM)
import Util ((|->))
--------------------------------------------------------------------------------

dynAttrButton :: ReactiveM t m => Dynamic t (Map Text Text) -> Text -> m (Event t ())
dynAttrButton attrsDyn label = mdo
  (buttonWidget, _) <- elDynAttr' "button" attrsDyn (text label)
  return $ domEvent Click buttonWidget

dynClassButton :: ReactiveM t m => Dynamic t [Text] -> Text -> m (Event t ())
dynClassButton classDyn label = dynAttrButton (("class" |->) . intercalate " " <$> classDyn) label

uncondCondClasses :: Reflex t => [Text] -> [Text] -> Dynamic t Bool -> Dynamic t [Text]
uncondCondClasses unconditional conditional dyn = dyn <&> (\b -> unconditional <> if b then conditional else [])
