module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Platform.Cmd
import Set exposing (Set)
import Time exposing (Posix)
import Url exposing (Url)

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
  , tools : List String
  , resistances : List Resistance
  , notable_traits : List NotableTraitCategory
  , attacks : List Attack
  , pact_magic: Maybe PactMagic
  , spellcasting_sections : List SpellcastingSection
  , spell_slots : List Int
  , resources : List Resource
  }
{-
NotableTraits = [( Category, List Trait )]
Trait = [(Desc, Name)]
-}

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

type alias Resistance = { damage_type: String, resistance: String }

type alias NotableTraitCategory = { category: String, traits: List Trait }
type alias Trait = { name: String, desc: Maybe String }

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
  { max_prepared_spells : Maybe Int
  , origin : Origin
  , spell_attack_mod : Int
  , spell_save_dc : Int
  , spellcasting_ability : String
  , spellcasting_ability_mod : Int
  , spells : List Spell
  }
type alias Origin = String

type alias Spell =
  { aoe : Maybe String
  , casting_time : String
  , components : List Component
  , concentration : Bool
  , dc : Maybe Int
  , dc_abi : Maybe String
  , description : List String
  , higher_level : Maybe String
  , duration : String
  , level : Int
  , name : SpellName
  , prepared : AlwaysPrepared
  , range : String
  , resources : List String
  , ritual : Ritual
  , school : String
  , shortdesc : Maybe (List String)
  , summary : String
  , to_hit : Maybe Int
  , rolls : Maybe String
  , bonuses : List SpellBonus
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
  , short_rest : Maybe String
  , long_rest : Maybe String
  }

type PrologTerm = Compound String (List PrologTerm)
                | Atomic String

foldPT :  (String -> List r -> r)
       -> (String -> r)
       -> PrologTerm
       -> r
foldPT fCompound fAtomic t =
  case t of
    Compound functor args ->
      fCompound functor
        <| List.map (foldPT fCompound fAtomic) args
    Atomic atom ->
      fAtomic atom

mapPrologTermStrings : (String -> String) -> PrologTerm -> PrologTerm
mapPrologTermStrings f =
  foldPT 
    (\functor args -> Compound (f functor) args)
    (Atomic << f)

defunctor : PrologTerm -> List PrologTerm
defunctor tm =
  case tm of
    Atomic atom -> [ Atomic atom ]
    Compound _ args -> args

----------------------------------------------------------------------
-- CHARACTER SELECTION PAGE
type alias CharacterSelectionPageData =
  { characters : List (CharId, String)
  , newCharacterName : String
  }

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
    Int                          -- Number of choices `n` the user gets to make.
    (List SpecAndChoice)         -- `n` repetitions of the spec, each potentially with its own
                                 --   registered choice

type alias Effect = { effect : PrologTerm
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
  { url : Url
  , key : Nav.Key
  , preparedSpells : Dict Origin (Set SpellName)
  , showOnlyPreparedSpells : Bool
  , page : Page
  , focusedDropdownId : Maybe String
  , lastTick : Posix
  }
type Page
  = Loading (Maybe CharId)
  | Error String
  | CharacterSelectionPage CharacterSelectionPageData
  | PrintableCharSheetPage CharId CharacterSheet
  | EditCharacterPage CharId EditCharacterPageData
  | CardsPage CharId CardsPageOptions CharacterSheet
  | EquipmentPage CharId EquipmentPageData

type alias EditCharacterPageData = 
  { abilityTable : AbilityTable
  , optionsPerLevel : Dict Level (List Options)
  , traitsAndBonusesPerLevel : Dict Level (List Effect)
  , charLevel : Level
  , selectedLevel : Maybe Level
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

type alias Equipment = List String

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

----------------------------------------------------------------------
-- MSG
type Msg
  = HttpResponse (Result Http.Error HttpResponseMsg)
  | SelectCharacter CharId
  | GotoSelectCharacterPage
  | NewCharacterName String
  | CreateNewCharacter
  | EditCharacter CharId
  | UrlChanged Url
  | LinkClicked Browser.UrlRequest
  | SetSpellPreparedness Origin SpellName Bool
  | SetShowOnlyPreparedSpells Bool
  | EditCharacterLevel Level
  | Choice CharId String String Choice
  | OrSCChooseDir String String Dir
  | GotoSheet CharId
  | GotoLevelUp
  | GotoCardsPage CharId CardsPageOptions CharacterSheet
  | LevelUpAs String
  | SetEditCharacterPageDesc (Maybe (List String))
  | SelectDropdownOption String String
  | ToggleDropdown String
  | ClickOut
  | Null
  | Tick Posix
  | SetBaseAbilityScore Ability Int
  | GotoEquipmentPage CharId
  | UnequipWeapon { base_weapon : String, enchantment : Int }
  | EquipItem String
  | UnequipItem String
  | Retract Retraction
  | AddItemInput String

type Retraction = RetractLevelUp Int
                | RetractChoice { origin : String, id : String }

type Choice = ListChoice (List String) | SingletonChoice String

type HttpResponseMsg
  = GotCharacterList (List (CharId, String))
  | NewCharacterCreated CharId
  | GotCharacterSheet CharacterSheet
  | GotPrintableCharSheet CharacterSheet
  | GotCharacterOptions AbilityTable (Dict Level (List Options)) (Dict Level (List Effect))
  | GotEquipment (Result String Equipment)
  -- | ChoiceRegistered
  -- | LeveledUp
  -- | UpdatedBaseAbilityScores
  | Update

mkHttpResponseMsg : (a -> HttpResponseMsg) -> (Result Http.Error a -> Msg)
mkHttpResponseMsg f result =
  HttpResponse (Result.map f result)
    
type alias CardsPageOptions = { showSpells : ShowSpellOption }
type ShowSpellOption = AllSpells | OnlyPreparedSpells | NoSpells
