module Page.EditChar where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Comonad
import Control.Monad
import Data.List hiding (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Function hiding ((.))
import Data.Functor
import GHC.Generics

import Miso hiding (on)
import Miso.String (MisoString, ms, intercalate)
import qualified Miso.String

import Data.Zipper (Zipper(Zipper))
import qualified Data.Zipper as Zipper
import SF
import Types
import Util

import Debug.Trace
--------------------------------------------------------------------------------

pageSF :: CharacterOptions -> (Cmd ~> View Action)
pageSF charOpts0@(CharacterOptions opts0 selectedLevel0) = trace "pageSF" $ proc cmd -> do
  let selectLevelE = case cmd of SelectLevel lvl -> Just lvl; _ -> Nothing
  selectedLevel <- setter selectedLevel0 -< selectLevelE

  let newCharOptsE = case cmd of ReceivedCharacterOptions new -> Just new
                                 _                            -> Nothing
  charOpts <- setter charOpts0 -< newCharOptsE

  let sideNavView = viewSideNav selectedLevel charOpts

  let refreshMainE = mainSF (fromJust $ Map.lookup selectedLevel $ options charOpts)
        <$ (void selectLevelE <|> void newCharOptsE)
  mainView <- installEventSF (mainSF $ fromJust $ Map.lookup selectedLevel0 $ options charOpts0)
    -< (refreshMainE, cmd)

  returnA -< div_ [class_ "edit-page"] [sideNavView, mainView]

viewSideNav :: Level -> CharacterOptions -> View Action
viewSideNav selectedLevel (CharacterOptions options char_level) = div_
  [ class_ "side-nav" ]
  [ table_ []
    $ map (viewSideNavLevelButton char_level selectedLevel)
    $ reverse $ Map.keys $ options
  ]

viewSideNavLevelButton :: Level -> Level -> Level -> View Action
viewSideNavLevelButton charLevel selectedLevel level =
  tr_ []
  [ td_ [] [] -- TODO add retract button
  , td_ []
    [ button_
      (onClick (Cmd (SelectLevel level))
       : [style_ (Map.singleton "color" "#ffffff") | selectedLevel == level]
      )
      [text buttonText]
    ]
  ]
  where
    buttonText | level > charLevel = "+"
               | otherwise         = "Level " <> ms (show level)

-- mainSF :: Map Level [Option] -> ((Cmd, Level, ) ~> View Action)
-- mainSF optsPerLevel0 = proc (cmd, selectedLevel) -> do
  -- let selectLevelE = case cmd of SelectLevel lvl -> Just lvl; _ -> Nothing
  --     newOptionsE = Nothing -- TODO

  -- optsPerLevel <- setter optsPerLevel0 -< newOptionsE

  -- let opts = fromMaybe [] $ Map.lookup selectedLevel optsPerLevel
  --     nextE = optionListSF opts <$ (void selectLevelE <|> void newOptionsE)

  -- installEventSF (optionListSF (fromMaybe [] $ Map.lookup 1 optsPerLevel0))
  --   -< (nextE, cmd)


mainSF :: [Option] -> (Cmd ~> View Action)
mainSF options =
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
optionSF Option{origin_category, origin, id, display_id, spec, choice} = proc cmd -> do
  specView <- specSF (OptionId origin id) spec choice -< cmd
  returnA -< div_
    [class_ "options-section-style"]
    [h3_ [] [text display_id], specView]

optionIdToId :: OptionId -> MisoString
optionIdToId (OptionId origin id) = origin </> id

specSF :: OptionId -> Spec -> Maybe Choice -> (Cmd ~> View Action)

specSF optionId (ListSpec entries) choice =
  let atomicChoice = fmap atomic_choice choice -- deliberate irrefutable record access
      mkAction = SendChoiceSubmission optionId . SubmitSingletonChoice . fromJust -- TODO also support clear choice command
      ddEntries = [DropdownEntry opt (intercalate "\n\n" desc) | ListSpecEntry desc opt <- entries]
  in dropdownSF ddEntries (optionIdToId optionId) mkAction atomicChoice

specSF optionId (FromSpec unique num subspec) choice =
  let sub = join $ maybeToList $ fmap subchoices choice -- deliberate irrefutable record access
  in fromSpecSF optionId unique num subspec sub

specSF optionId (OrSpec leftname left rightname right) choice =
  let orChoice = liftA2 (,) side subchoice <$> choice
  in orSpecSF optionId leftname left rightname right orChoice

fromSpecSF :: OptionId -> Unique -> Maybe Int -> Spec -> [Choice] -> (Cmd ~> View Action)
fromSpecSF optionId unique numOrUnlimited subspec subchoices =
  fmap viewDropdowns . sequenceA
  $ zipWith3 (dropdownSF entries) ids editFns dropdownChoices
  where
    -- For now, we only support lists of dropdowns in the interface, so anything
    -- but list subspec / atomic choices will error.
    atomicChoices = map atomic_choice subchoices
    editFns = choiceEditFunctions optionId atomicChoices

    entries = [DropdownEntry opt (intercalate "\n\n" desc) | ListSpecEntry desc opt <- list subspec]

    ids = [prefix </> ms (show i) | let prefix = optionIdToId optionId, i :: Int <- [0..]]
    dropdownChoices = map Just atomicChoices <> [Nothing]

    limit = case numOrUnlimited of Nothing -> Data.Function.id
                                   Just n -> take n
    disabledDropdowns = case numOrUnlimited of Nothing -> []
                                               Just _  -> repeat disabledDropdown

    disabledDropdown = div_
      [class_ "dropdown dropdown-disabled"]
      [button_ [] [text "..."]]

    viewDropdowns dropdowns = div_ []
      $ map (div_ [style_ (Map.singleton "marginTop" "2px")] . singleton)
      $ limit
      $ dropdowns <> disabledDropdowns

choiceEditFunctions :: OptionId -> [MisoString] -> [Maybe MisoString -> Action]
choiceEditFunctions optionId choices = case choices of
  []     -> [mkChoice . singleton . fromMaybe ""]
  c : cs -> Zipper.toList (extend overwriteOrDeleteFocused (Zipper [] c cs))
    <> [mkChoice . ((c:cs) <>) . singleton . fromMaybe ""]

  where
    mkChoice = SendChoiceSubmission optionId . SubmitListChoice

    overwriteOrDeleteFocused :: Zipper MisoString -> (Maybe MisoString -> Action)
    overwriteOrDeleteFocused (Zipper ls _ rs) newChoice =
      mkChoice $ case newChoice of
                   Just x  -> Zipper.toList (Zipper ls x rs)
                   Nothing -> reverse ls <> rs


orSpecSF :: OptionId
         -> MisoString -> Spec -> MisoString -> Spec
         -> Maybe (Dir, Choice)
         -> (Cmd ~> View Action)
orSpecSF optionId leftname left rightname right orChoice = proc cmd -> do
  dir <- potentiallyUninitializedSetter (fmap fst orChoice) -< case cmd of
    SelectOrChoiceDir id' dir' | id' == idPrefix -> Just dir'
    _                                            -> Nothing
  returnA -< div_ []
    [ input_ [ type_ "radio"
             , checked_ (dir == Just L)
             , id_ leftId
             , onInput $ const $ Cmd $ SelectOrChoiceDir idPrefix L
             ]
    , label_ [for_ leftId] [text leftname]
    , input_ [ type_ "radio"
             , checked_ (dir == Just R)
             , id_ rightId
             , onInput $ const $ Cmd $ SelectOrChoiceDir idPrefix R
             ]
    , label_ [for_ rightId] [text rightname]
    ]

  where
    idPrefix = optionIdToId optionId
    leftId   = idPrefix <> "/" <> leftname
    rightId  = idPrefix <> "/" <> rightname

data DropdownEntry = DropdownEntry
  { ddeName   :: MisoString
  , ddeDesc   :: MisoString
  }
dropdownSF :: [DropdownEntry] -> MisoString -> (Maybe MisoString -> Action) -> Maybe MisoString
           -> (Cmd ~> View Action)
dropdownSF entries id mkAction initiallySelected = proc cmd -> do
  let dropdownCmd = case cmd of DropdownCmd id' cmd' | id == id' -> Just cmd' ; _ -> Nothing

  rec
    dOpen <- delay False -< open
    open <- setter False -< case dropdownCmd of
      Just OpenDropdown -> Just (not dOpen)
      Just (SelectDropdownOption _) -> Just False
      _ -> case cmd of ClickOut -> Just False
                       DropdownCmd id' _ | id /= id' -> Just False
                       _ -> Nothing

  currentlySelected
    <- setter initiallySelected
    -< case dropdownCmd of Just (SelectDropdownOption new) -> Just new
                           _                               -> Nothing

  let mkDropdownEntry maybeName = button_
        [ class_ "dropdown-entry"
        , onClick (mkAction maybeName)
        ]
        [text (fromMaybe "-- clear selection --" maybeName)]

  returnA -<
    div_
    [ class_ "dropdown dropdown-enabled"
    , onWithOptions
        (Options { preventDefault = False, stopPropagation = True })
        "click"
        emptyDecoder
        (const NoOp)
    ]
    [ button_
      [ onClick (Cmd $ DropdownCmd id OpenDropdown)
      , class_ (if open then "dropdown-button-open" else "dropdown-button-closed")
      ]
      [case currentlySelected of Nothing -> text "..."; Just x -> text x]
    , div_
      [ style_ $ Map.singleton "visibility" (if open then "visible" else "hidden")
      , class_ "dropdown-content"
      ]
      ([mkDropdownEntry Nothing | isJust currentlySelected]
       <> map (mkDropdownEntry . Just . ddeName) entries)
    ]

buttonColor :: Bool -> Bool -> Bool -> MisoString
buttonColor isDisabled isOptionSelected isOpen =
  case (isDisabled, isOptionSelected, isOpen ) of
    (True, _    , _    ) -> "rgb(150,150,150)"
    (_   , True , False) -> "rgb(0,180,0)"
    (_   , True , True ) -> "rgb(0,150,0)"
    (_   , False, True ) -> "#2989b9"
    (_   , False, False) -> "#3498db"

viewOriginCategoryHeader :: MisoString -> View Action
viewOriginCategoryHeader category = h2_ [] [text headerMsg]
  where
    headerMsg = case category of
      "init"     -> "Choose your background, class, and race:"
      "level up" -> "Level up:"
      _          -> "From " <> category <> ":"
