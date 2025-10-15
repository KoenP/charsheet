module Widget where

--------------------------------------------------------------------------------
import Data.Aeson
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text, intercalate, pack)
import Reflex.Class
import Reflex.Dom

import Types
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

loadWidget :: forall t m x a b. (ReactiveIOM t m, IsXhrPayload x, FromJSON a)
           => XhrRequest x -> (a -> m (Event t b)) -> m (Event t a, Event t b)
loadWidget req k = loadWidget' (performReq req) k

performReq :: (ReactiveIOM t m, IsXhrPayload x, FromJSON a) => XhrRequest x -> m (Event t a)
performReq req = do
  postBuildE <- getPostBuild
  resE <- performRequestAsync (req <$ postBuildE)
  return $ errorOnLeft . eitherDecodeStrictText . fromJust <$> fmap _xhrResponse_responseText resE
      
  -- postBuildE <- getPostBuild
  -- resE <- performRequestAsync (req <$ postBuildE)
  -- let responseTextE = fmap _xhrResponse_responseText resE
  --     responseValE = errorOnLeft . eitherDecodeStrictText . fromJust <$> responseTextE

  -- ev <- switchDyn <$> widgetHold (text "Loading..." >> return never) (fmap k responseValE)

  -- return (responseValE, ev)

loadWidget' :: ReactiveIOM t m => m (Event t a) -> (a -> m (Event t b)) -> m (Event t a, Event t b)
loadWidget' performLoad k = do
  resE <- performLoad
  widgetE <- switchDyn <$> widgetHold (text "Loading..." >> return never) (fmap k resE)
  return (resE, widgetE)



domShow :: (DomBuilder t m, Show a) => a -> m ()
domShow = text . pack . show

divcl :: DomBuilder t m => [Text] -> m a -> m a
divcl classes = elClass "div" (intercalate " " classes)

{-
x :: XhrRequest x
page :: CharacterOptions -> m (Event t (Cache -> Cache))

loadWidget :: Event t (Cache -> Cache)
           -> x
           -> (CharacterOptions -> m (Event t (Cache -> Cache)))
           -> m (Event t CharacterOptions, Dynamic t (Event t (Cache -> Cache)))

responseValE :: Event t CharacterOptions

fmap k responseValE :: Event t (m (Event t (Cache -> Cache)))

widgetHold :: m (Event t (Cache -> Cache))
           -> Event t (m (Event t (Cache -> Cache)))
           -> m (Dynamic t (Event t (Cache -> Cache)))

widgetHold (pure never) (fmap k responseValE)


  postBuildE <- getPostBuild
  resE <- performRequestAsync (req <$ postBuildE)

  let responseTextE = traceEvent "responseTextE" $ fmap _xhrResponse_responseText resE
      responseValE = errorOnLeft . eitherDecodeStrictText . fromJust <$> responseTextE
  dyn <- widgetHold (text "Loading..." >> pure nullVal) (fmap k responseValE)
  return (responseValE, traceDynWith (const "loadWidget dyn updated") dyn)
-}
