module Page.EditChar where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Function hiding ((.))
import Data.Functor
import GHC.Generics

import Miso hiding (on)
import Miso.String (MisoString, ms)
import qualified Miso.String

import SF
import Types
import Util
--------------------------------------------------------------------------------

pageSF :: CharacterOptions -> (Cmd ~> View Action)
pageSF charOpts = proc cmd -> do
  selectedLevel <- setter 1 -< case cmd of (SelectLevel lvl) -> Just lvl; _ -> Nothing
  let sideNavView = viewSideNav selectedLevel charOpts
  mainView    <- mainSF (options charOpts) -< (cmd, selectedLevel)
  returnA -< div_ [class_ "edit-page"] [sideNavView, mainView]

viewSideNav :: Level -> CharacterOptions -> View Action
viewSideNav selectedLevel (CharacterOptions { char_level, options }) = div_
  [ class_ "side-nav" ]
  [ table_ []
    $ map (viewSideNavLevelButton selectedLevel)
    $ reverse $ filter (<= char_level) $ Map.keys $ options
  ]

viewSideNavLevelButton :: Level -> Level -> View Action
viewSideNavLevelButton selectedLevel level =
  tr_ []
  [ td_ [] [] -- TODO add retract button
  , td_ []
    [ button_
      (onClick (Cmd (SelectLevel level))
       : [style_ (Map.singleton "color" "#ffffff") | selectedLevel == level]
      )
      [ text (ms $ "Level " ++ show level)]
    ]
  ]

mainSF :: Map Level [Option] -> ((Cmd, Level) ~> View Action)
mainSF optsPerLevel0 = proc (cmd, selectedLevel) -> do
  let selectLevelE = case cmd of SelectLevel lvl -> Just lvl; _ -> Nothing
      newOptionsE = Nothing -- TODO

  optsPerLevel <- setter optsPerLevel0 -< newOptionsE

  let opts = fromMaybe [] $ Map.lookup selectedLevel optsPerLevel
      nextE = optionListSF opts <$ (void selectLevelE <|> void newOptionsE)

  installEventSF (optionListSF (fromMaybe [] $ Map.lookup 1 optsPerLevel0))
    -< (nextE, cmd)


optionListSF :: [Option] -> (Cmd ~> View Action)
optionListSF options =
  let
    optionsSortField Option{display_origin_category, origin_category_index} =
      (origin_category_index, display_origin_category)

    sfs
      = map (uncurry originCategoryOptionsListSF)
      $ map (\((_,k),v) -> (k,v)) $ Map.assocs
      $ multiMapFromList
      $ zip (map optionsSortField options) options

  in
    div_ [class_ "main-section"]
    . (h1_ [] [text "You have the following options:"] :)
    <$> col sfs

-- TODO ability table
-- mainSF :: CharacterOptions -> ((Cmd, Level) ~> View Action)
-- mainSF CharacterOptions{ options } = proc (cmd, selectedLevel) -> do
--   let
--     optionsForLevel = fromMaybe [] $ Map.lookup selectedLevel $ options
--     optionsSortField Option{display_origin_category, origin_category_index} =
--       (origin_category_index, display_origin_category)
-- 
-- 
--   optsView <- col optsSFs -< cmd
-- 
--   returnA -< div_
--     [class_ "main-section"]
--     ( h1_ [] [text "You have the following options:"]
--     : optsView
--     )
-- 
--   where
--     optsSFs :: [Cmd ~> View Action]
--     optsSFs
--       = map (uncurry originCategoryOptionsListSF)
--       $ map (\((_,k),v) -> (k,v)) $ Map.assocs
--       $ multiMapFromList
--       $ zip (map optionsSortField optionsForLevel) optionsForLevel


originCategoryOptionsListSF :: MisoString -> List Option -> (Cmd ~> View Action)
originCategoryOptionsListSF category options = proc cmd -> do
  let header = viewOriginCategoryHeader category
  optionViews <- col (map optionSF options) -< cmd
  returnA -< div_ [class_ "origin-category"] (header : optionViews)

optionSF :: Option -> (Cmd ~> View Action)
optionSF Option{spec, choice} = undefined

specSF :: Spec -> Maybe Choice -> (Cmd ~> View Action)
specSF (ListSpec entries) choice =
  let listChoice = case choice of Just (ListChoice xs) -> Just xs; _ -> Nothing
  in dropdownSF entries listChoice

dropdownSF :: MisoString -> MisoString -> Maybe MisoString -> (Cmd ~> View Action)
dropdownSF id entries currentlySelected = proc cmd -> do
  open <- setter False
       -< case cmd of Dropdown id' isOpen | id == id' -> Just isOpen; _ -> Nothing

  returnA -<
    div_
    [class_ "dropdown"] -- TODO dropdown style
    [ button_ [onClick (Dropdown id True)] []
    , div_
      [style_ $ Map.singleton "visibility" (if open then "visible" else "hidden")]
      []
    ]

viewOriginCategoryHeader :: MisoString -> View Action
viewOriginCategoryHeader category = h2_ [] [text headerMsg] 
  where
    headerMsg = case category of
      "init"     -> "Choose your background, class, and race:"
      "level up" -> "Level up:"
      _          -> "From " <> category <> ":"
