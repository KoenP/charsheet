module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Css exposing (Color)
import Dict exposing (Dict)
import Http
import Platform.Cmd
import Set exposing (Set)
import Time exposing (Posix)

import Types.Ability exposing (..)

----------------------------------------------------------------------
-- CHARACTER SHEET

type CharId = CharId String

type alias CharacterSheet =
  { name : String
  , summary : CharacterSummary
  , ac_formulas : List AcFormula
  , hit_dice : List HitDice
  , ability_table : AbilityTable
  , skill_table : SkillTable
  , languages : List String
  , weapons : List String
  , armor : List String
  , tools : List Tool
  , resistances : List Resistance
  , notable_traits : List NotableTraitCategory
  , attacks : List Attack
  , pact_magic: Maybe PactMagic
  , spellcasting_sections : List SpellcastingSection
  , spell_slots : List Int
  , resources : List Resource
  }

type alias CharacterSummary =
  { ac : Int
  , class : String
  , hd : String
  , initiative : Int
  , level : Int
  , maxhp : Int
  , pp : Int
  , prof_bon : Int
  , race : String
  , speed : List Speed
  }

type alias Speed = { mode: String, speed: Int }

type alias Tool = { tool: String, expertise: Bool }

type alias Resistance = { damage_type: String, resistance: String }

type alias NotableTraitCategory = { category: String, traits: List Trait }
type alias Trait =
  { name        : String
  , desc        : Maybe (List String)
  , ref         : Maybe String
  , seminotable : Bool
  }

type alias AcFormula =
  { name : String
  , ac : Int
  , shield : Maybe Int
  }

type alias HitDice =
  { n : Int
  , d : Int
  }

type alias Attack =
  { name : String
  , range : String
  , to_hit_or_dc : String
  , damage : String
  , notes : String
  }

type alias SpellcastingSection =
  { max_prepared_spells      : Maybe Int
  , origin                   : Origin
  , origin_shorthand         : String
  , spell_attack_mod         : Int
  , spell_save_dc            : Int
  , spellcasting_ability     : String
  , spellcasting_ability_mod : Int
  , spells                   : List Spell
  }
type alias Origin = String

type alias Spell =
  { aoe           : Maybe String
  , casting_time  : String
  , components    : List Component
  , concentration : Bool
  , description   : String
  , higher_level  : Maybe String
  , duration      : String
  , level         : Int
  , name          : SpellName
  , prepared      : AlwaysPrepared
  , range         : String
  , resources     : List PrologTerm
  , ref           : Maybe String
  , ritual        : Ritual
  , school        : String
  , shortdesc     : Maybe String
  , summary       : String
  , to_hit        : Maybe Int
  , rolls         : Maybe String
  , bonuses       : List SpellBonus
  }
type Component = V | S | M String
type alias AlwaysPrepared = Bool
type alias SpellName = String
type alias Level = Int

type Ritual = Ritual | NotRitual | OnlyRitual

type alias SpellBonus =
  { origin : String
  , bonus : String
  }

type alias PactMagic =
  { slot_count : Int
  , slot_level : Int
  }

type alias Resource =
  { name : String
  , number : Int
  , restore : Dict String String
  }

type PrologTerm = Compound String (List PrologTerm)
                | List (List PrologTerm)
                | Atomic String

foldPT :  (String -> List r -> r)
       -> (List r -> r)
       -> (String -> r)
       -> PrologTerm
       -> r
foldPT fCompound fList fAtomic t =
  case t of
    Compound functor args ->
      fCompound functor
        <| List.map (foldPT fCompound fList fAtomic) args
    List xs ->
      fList
        <| List.map (foldPT fCompound fList fAtomic) xs
    Atomic atom ->
      fAtomic atom

mapPrologTermStrings : (String -> String) -> PrologTerm -> PrologTerm
mapPrologTermStrings f =
  foldPT 
    (\functor args -> Compound (f functor) args)
    (\xs -> List xs)
    (Atomic << f)

defunctor : PrologTerm -> List PrologTerm
defunctor tm =
  case tm of
    Atomic atom     -> [ Atomic atom ]
    List xs         -> xs
    Compound _ args -> args

----------------------------------------------------------------------
-- EDIT CHARACTER PAGE
type alias Options =
  { charlevel : Level
  , id : String
  , display_id : String
  , origin : String
  , origin_category : String
  , display_origin_category : String
  , origin_category_index : Int
  , spec : SpecAndChoice
  }
type Spec
  = ListSpec (List String)
  | OrSpec (String, Spec) (String, Spec)
  | FromSpec Unique Int Spec
type alias Unique = Bool

type Dir = L | R
type SpecAndChoice
  = ListSC
    (Maybe String)               -- The user's choice (if relevant).
    (List (String, List String)) -- List of options, and option description (list of paragraphs).
  | OrSC
    (Maybe Dir)                  -- The user's choice (if relevant).
    (String, SpecAndChoice)      -- Name and spec on the left side.
    (String, SpecAndChoice)      -- Name and spec on the right side.
  | FromSC 
    Unique                       -- Whether this spec is a "from" or "unique_from" spec.
    (Maybe Int)                  -- Number of choices `n` the user gets to make,
                                 -- or `Nothing` if the user gets to make unlimited choices.
    (List SpecAndChoice)         -- `n` repetitions of the spec, each potentially with its own
                                 --   registered choice

type alias Effect = { effect : PrologTerm
                    , pretty : String
                    , origin : PrologTerm
                    , desc   : List String
                    }

extractChoicesList : SpecAndChoice -> List String
extractChoicesList spec =
  case spec of
    ListSC (Just choice) _ -> [ choice ]
    ListSC Nothing _ -> []
    OrSC _ (_, left) (_,right) -> extractChoicesList left ++ extractChoicesList right
    FromSC _ _ specs -> List.concatMap extractChoicesList specs

----------------------------------------------------------------------
-- MODEL
type alias Model =
  { preparedSpells : Dict Origin (Set SpellName)
  , showOnlyPreparedSpells : Bool
  , cardConfig : CardConfig
  , page : Page
  , focusedDropdownId : Maybe String
  , lastTick : Posix
  , charId : CharId
  , sheetCache : Maybe CharacterSheet
  , prevSheetCache : Maybe CharacterSheet
  }
type Page
  = Loading
  | Error String
  | PrintableCharSheetPage CharacterSheet
  | EditCharacterPage EditCharacterPageData
  | CardsPage CardsPageOptions CharacterSheet
  | CardSelectPage { curSheet : CharacterSheet , prevSheet : CharacterSheet }
  | EquipmentPage EquipmentPageData

type alias EditCharacterPageData = 
  { abilityTable : AbilityTable
  , optionsPerLevel : Dict Level (List Options)
  , traitsAndBonusesPerLevel : Dict Level (List Effect)
  , charLevel : Level
  , selectedLevel : Level
  , desc : Maybe (List String)
  , setAbilitiesOnNextTick : Dict Ability Int
  }

type alias EquipmentPageData =
  { equipment : Equipment
  , inputFieldVal : String
  , error : Maybe String
  }

applyPage : Model -> (Page, Cmd Msg) -> (Model, Cmd Msg)
applyPage model ( page, cmd ) =
  ( { model | page = page }, cmd)

invalidateAllSheetCaches : Model -> Model
invalidateAllSheetCaches model =
  { model | sheetCache = Nothing , prevSheetCache = Nothing }

invalidateSheetCache : Model -> Model
invalidateSheetCache model =
  { model | sheetCache = Nothing }

transferSheetCache : Model -> Model
transferSheetCache model =
  { model | sheetCache = Nothing , prevSheetCache = model.sheetCache }

errorPage : Model -> String -> (Model, Cmd Msg)
errorPage model msg = ({ model | page = Error msg }, Cmd.none)

initPreparedSpells : List SpellcastingSection -> Dict Origin (Set SpellName)
initPreparedSpells =
  Dict.fromList << List.map (\section -> ( section.origin, Set.empty ))

setSpellPreparedness : Origin -> SpellName -> Bool
                     -> Dict Origin (Set SpellName)
                     -> Dict Origin (Set SpellName)
setSpellPreparedness origin spell prepared old =
  Debug.log "setSpellPreparedness" <|
    Dict.update
      origin
      (Maybe.map <| \set -> case prepared of
                              False -> Set.remove spell set
                              True  -> Set.insert spell set)
      old

type alias Equipment = List Item
type alias Item = { name : String
                  , inferred : Bool
                  }

-- TODO delete ?
--  type alias Weapon =
--    { base_weapon : String
--    , enchantment : Int
--    , category : String
--    , range : String
--    , to_hit : String
--    , damage : String
--    , notes : String
--    }

type alias Category = String
type alias TraitName = String
type alias CardConfig = 
  { explicitlyExcludedTraits        : Set (Category, TraitName)
  , explicitlyExcludedSpells        : Set (Category, SpellName) -- TODO not sure if this 100% uniquely identifies a spell (maybe there's some weird stuff where you can have the same spell twice as warlock for example, once as a regular spell and once as an eldritch invocation)
  , excludedCategories              : Set Category
  , showSpells                      : Bool
  , showTraits                      : Bool
  , onlyShowChanges                 : Bool
  , spellcastingSectionColorSchemes : Dict Category ColorScheme
  , spellColorSchemes               : Dict (Category, SpellName) ColorScheme
  , traitCategoryColorSchemes       : Dict Category ColorScheme
  , traitColorSchemes               : Dict (Category, TraitName) ColorScheme
  , categoryColorSchemes            : Dict Category ColorScheme
  }
emptyCardConfig : CardConfig
emptyCardConfig =
  { explicitlyExcludedTraits        = Set.empty
  , explicitlyExcludedSpells        = Set.empty
  , excludedCategories              = Set.empty
  , showSpells                      = True
  , showTraits                      = True
  , onlyShowChanges                 = False
  , spellcastingSectionColorSchemes = Dict.empty
  , spellColorSchemes               = Dict.empty
  , traitCategoryColorSchemes       = Dict.empty
  , traitColorSchemes               = Dict.empty
  , categoryColorSchemes            = Dict.empty
  }
type alias ColorScheme = { bg   : Color
                         , fg   : Color
                         , name : String
                         }

----------------------------------------------------------------------
-- MSG
type Msg
  = HttpResponse (Result Http.Error HttpResponseMsg)
  | SetSpellPreparedness Origin SpellName Bool
  | SetShowOnlyPreparedSpells Bool
  | EditCharacterLevel Level
  | Choice String String Choice
  | OrSCChooseDir String String Dir
  | GotoEditCharacter
  | GotoSheet
  | GotoCardsPage CardsPageOptions
  | GotoCardSelectPage CharacterSheet
  | GotoSelectCharacterPage
  | SetEditCharacterPageDesc (Maybe (List String))
  | SelectDropdownOption String String
  | ToggleDropdown String
  | ClickOut
  | Null
  | Tick Posix
  | SetBaseAbilityScore Ability Int
  | GotoEquipmentPage
  | UnequipWeapon { base_weapon : String, enchantment : Int }
  | EquipItem String
  | UnequipItem String
  | Retract Retraction
  | AddItemInput String
  | SetCardConfig CardConfig

type Retraction = RetractChoice { origin : String, id : String }

type Choice = ListChoice (List String) | SingletonChoice String

type alias CharacterOptions = { ability_table      : AbilityTable
                              , options            : Dict Level (List Options)
                              , traits_and_bonuses : Dict Level (List Effect)
                              , char_level         : Level
                              }

type HttpResponseMsg
  = GotCharacterList (List (CharId, String))
  | NewCharacterCreated CharId
  | GotCharacterSheet CharacterSheet
  | GotPrintableCharSheet CharacterSheet
  | GotPrevLevelSheet CharacterSheet CharacterSheet
  | GotCards CharacterSheet
  | GotCharacterOptions CharacterOptions
  | GotEquipment (Result String Equipment)
  | ToCardSelectionPage CharacterSheet -- TODO rename all of these to "ToXPage"
  -- | ChoiceRegistered
  -- | LeveledUp
  -- | UpdatedBaseAbilityScores
  | Update

mkHttpResponseMsg : (a -> HttpResponseMsg) -> (Result Http.Error a -> Msg)
mkHttpResponseMsg f result =
  HttpResponse (Result.map f result)

type alias CardsPageOptions = { showSpells : ShowSpellOption }
type ShowSpellOption = AllSpells | OnlyPreparedSpells | NoSpells
