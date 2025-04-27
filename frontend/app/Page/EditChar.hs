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
pageSF options = proc cmd -> do
  selectedLevel <- setter 1 -< case cmd of (SelectLevel lvl) -> Just lvl; _ -> Nothing
  let sideNavView = viewSideNav selectedLevel options
  mainView    <- mainSF options -< (cmd, selectedLevel)
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

-- TODO ability table
mainSF :: CharacterOptions -> ((Cmd, Level) ~> View Action)
mainSF CharacterOptions{ options } = proc (cmd, selectedLevel) -> do
  let
    optsView
      = map (uncurry viewOriginCategoryOptionsList)
      $ map (\((_,k),v) -> (k,v)) $ Map.assocs
      $ multiMapFromList
      $ zip (map optionsSortField optionsForLevel) optionsForLevel

    optionsForLevel = fromMaybe [] $ Map.lookup selectedLevel $ options

    optionsSortField Option{display_origin_category, origin_category_index} =
      (origin_category_index, display_origin_category)

  returnA -< div_
    [class_ "main-section"]
    ( h1_ [] [text "You have the following options:"]
    : optsView
    )

viewOriginCategoryOptionsList :: MisoString -> List Option -> View Action
viewOriginCategoryOptionsList category options = div_
  [ class_ "origin-category" ]
  ( h2_ [] [text headerMsg] : [] )

  where
    headerMsg = case category of
      "init"     -> "Choose your background, class, and race:"
      "level up" -> "Level up:"
      _          -> "From " <> category <> ":"
