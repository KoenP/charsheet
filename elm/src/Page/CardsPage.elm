module Page.CardsPage exposing (..)

import Css exposing (Style, px, mm)
import Css.Media
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import List
-- import Markdown.Option as Md
-- import Markdown.Render as Md
import Markdown
import Platform.Cmd as Cmd
import Set exposing (Set)
import String

import Decoder.CharacterSheet exposing (sheetDec)
import Request exposing (requestUrl)
import Types exposing (..)
import Util exposing (simple)


----------------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------------
update : Msg -> Model -> CharacterSheet -> (Model, Cmd Msg)
update msg model sheet = (model, Cmd.none)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view : CardsPageOptions -> CharacterSheet -> Dict Origin (Set SpellName) -> List (Html Msg)
view options sheet preparedSpells =
  List.map (div [ Attr.css cardsStyle ])
  <| Util.chunks 8 
  <| List.concatMap (viewSpellcastingSection options preparedSpells) sheet.spellcasting_sections

viewSpellcastingSection :  CardsPageOptions -> Dict Origin (Set SpellName) -> SpellcastingSection
                        -> List (Html Msg)
viewSpellcastingSection options preparedSpells section =
  List.map (viewCard section.origin)
    <| List.filter
         (shouldIncludeSpell options
            (Maybe.withDefault Set.empty <| Dict.get section.origin preparedSpells))
    <| section.spells

shouldIncludeSpell : CardsPageOptions -> Set SpellName -> Spell -> Bool
shouldIncludeSpell { showSpells } preparedSpells { name, prepared } =
  case showSpells of
    AllSpells -> True
    OnlyPreparedSpells -> prepared || Set.member name preparedSpells
    NoSpells -> False

viewCard : Origin -> Spell -> Html Msg
viewCard origin spell =
  div [ Attr.css cardStyle ] <|
    [ div [ Attr.css cardTitleSectionStyle ]
        [ div [ Attr.css cardTitleStyle ] [text spell.name]
        , div [ Attr.css cardSubtitleStyle ] [ text (cardSubtitle spell) ]
        ]
    , div [ Attr.css cardBoxesSectionStyle ]
        [ cardBox "action-cost" spell.casting_time
        , cardBox "components" (showComponents spell.components)
        , cardBox "rolls" (Maybe.withDefault "/" spell.rolls)
        , cardBox "hourglass" spell.duration
        , cardBox "range" spell.range
        , cardBox "aoe" (Maybe.withDefault "/" spell.aoe)
        ]
    , div [ Attr.css [ Css.flexGrow (Css.num 1), Css.minHeight Css.zero ] ] []
    , viewSpellDescription (getSpellDescriptionText spell) spell.higher_level
    ]
    ++
    viewConcentrationBadge spell.concentration
    ++ 
    [ div [ Attr.css cardTypeStyle ] [text (origin ++ " spell")]
    ]

getSpellDescriptionText : Spell -> List String
getSpellDescriptionText spell =
  Maybe.withDefault spell.description <| Maybe.map (\d -> "(Summary:)" :: d) spell.shortdesc

viewSpellDescription : List String -> Maybe String -> Html Msg
viewSpellDescription paragraphs higherLevel =
  div [ Attr.css (descriptionStyle (estimateDescFontSize paragraphs higherLevel))
      , Attr.class "card-description"
      ]
    <| (  paragraphs
       |> List.intersperse "\n\n"
       |> String.concat
       -- |> Md.toHtml_ Md.Extended 
       |> Markdown.toHtmlWith
            { githubFlavored = Just { tables = True, breaks = False }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = True
            }
            []
       |> fromUnstyled
       )
       -- |> List.map (text >> List.singleton
       --                   >> p [Attr.css [ Css.marginBottom (px 2)
       --                                  , Css.marginTop (px 0)]]))
       ::
       viewHigherLevelP higherLevel

viewConcentrationBadge : Bool -> List (Html Msg)
viewConcentrationBadge concentration =
  if concentration
  then [ div [Attr.css concentrationBadgeStyle] [text "concentration"] ]
  else []

viewHigherLevelP : Maybe String -> List (Html Msg)
viewHigherLevelP hl =
  case hl of
    Nothing -> []
    Just hldesc -> [p [] [b [] [text "At higher levels. "], text hldesc]]

cardSubtitle : Spell -> String
cardSubtitle spell =
  let levelAndSchool = if spell.level == 0
                       then spell.school ++ " cantrip"
                       else Util.ordinal spell.level ++ " level " ++ spell.school
  in case spell.ritual of
       NotRitual -> levelAndSchool
       Ritual -> levelAndSchool ++ " (ritual)"
       OnlyRitual -> levelAndSchool ++ " (only ritual)"

cardBox : String -> String -> Html Msg
cardBox iconName value =
  div [ Attr.css cardBoxStyle ] [ icon iconName, text value ]

icon : String -> Html Msg
icon iconName =
  img
    [ Attr.src ("/icons/" ++ iconName ++ ".png")
    , Attr.css [ Css.width (px 10), Css.paddingLeft (mm 1) ]
    ]
    []

showComponents : List Component -> String
showComponents = List.map showComponent >> List.intersperse ", " >> String.concat

showComponent : Component -> String
showComponent c =
  case c of
    V   -> "V"
    S   -> "S"
    M _ -> "M"
                  

----------------------------------------------------------------------
-- STYLE
----------------------------------------------------------------------
cardsStyle : List Style
cardsStyle =
  [ Css.displayFlex
  , Css.flexDirection Css.row
  , Css.flexWrap Css.wrap
  , Css.property "break-inside" "avoid"
  ]
  
cardStyle : List Style
cardStyle =
  [ Css.position Css.relative
  , Css.border3 (px 1) Css.solid (Css.rgb 0 0 0)

  -- MTG size
  , Css.width (mm 63)
  , Css.minWidth (mm 63)
  , Css.height (mm 88)
  , Css.minHeight (mm 88)

  , Css.boxSizing Css.borderBox

  , Css.backgroundColor (Css.hex "e8e8e8") |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.fontFamilies ["Verdana", "Dosis"]
  , Css.padding (px 4)

  , Css.displayFlex
  , Css.flexDirection Css.column
  ]

cardTitleSectionStyle : List Style
cardTitleSectionStyle =
  [ Css.textAlign Css.center
  , Css.backgroundColor (Css.hex "ffffff") |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.borderRadius (px 8)
  , Css.border3 (px 2) Css.solid (Css.rgb 0 0 0)
  , Css.displayFlex
  , Css.flexDirection Css.column
  ]

cardTitleStyle : List Style
cardTitleStyle =
  [ Css.fontSize (mm 4.2)
  , Css.textTransform Css.capitalize
  ]

cardSubtitleStyle : List Style
cardSubtitleStyle =
  [ Css.textTransform Css.uppercase
  , Css.fontSize (px 7)
  , Css.fontWeight Css.bold
  ]

cardBoxesSectionStyle : List Style
cardBoxesSectionStyle =
  [ Css.marginTop (mm 1.5)
  , Css.property "display" "grid"
  , Css.property "grid-template-columns" "1fr 1fr 1fr"
  , Css.property "gap" "0.6mm"
  ]

cardBoxStyle : List Style
cardBoxStyle =
  [ Css.backgroundColor (Css.hex "ffffff") |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.borderRadius (px 6)
  , Css.fontSize (px 8)
  , Css.textAlign Css.center
  , Css.position Css.relative
  , Css.property "display" "grid"
  , Css.property "grid-template-columns" "1fr 5fr"
  , Css.padding (mm 0.7)
  ]

descriptionStyle : Float -> List Style
descriptionStyle fontSize =
  [ Css.fontSize <| px fontSize
  , Css.lineHeight <| px fontSize
  , Css.textAlign Css.justify
  , Css.backgroundColor (Css.hex "ffffff")
  , Css.property "print-color-adjust" "exact"
  , Css.borderRadius (px 8)
  , Css.padding (mm 1)
  , Css.marginBottom (mm 2)
  ]

estimateDescFontSize : List String -> Maybe String -> Float
estimateDescFontSize paragraphs higherLevel =
  let
    len = spellDescriptionLength paragraphs higherLevel
  in 
    if len >= 1800 || descriptionContainsTable paragraphs
    then 6
      else if len >= 1600
           then 7
           else if len >= 1000
                then 8
                else 10

spellDescriptionLength : List String -> Maybe String -> Int
spellDescriptionLength paragraphs higherLevel =
  List.sum
    <| Maybe.withDefault 0 (Maybe.map String.length higherLevel)
      :: List.map String.length paragraphs


descriptionContainsTable : List String -> Bool
descriptionContainsTable = List.any (String.contains "|---|")


cardTypeStyle : List Style
cardTypeStyle =
  [ Css.position Css.absolute
  , Css.right Css.zero
  , Css.bottom Css.zero
  , Css.fontSize (px 6)
  , Css.textTransform Css.uppercase
  , Css.backgroundColor (Css.hex "000000")
  , Css.color (Css.hex "ffffff")
  , Css.borderRadius4 (mm 1) Css.zero Css.zero Css.zero
  , Css.paddingLeft (mm 0.4)
  , Css.paddingTop (mm 0.4)
  , Css.fontWeight (Css.int 900)
  ]

concentrationBadgeStyle : List Style
concentrationBadgeStyle =
  [ Css.position Css.absolute
  , Css.left Css.zero
  , Css.bottom Css.zero
  , Css.fontSize (px 6)
  , Css.textTransform Css.uppercase
  , Css.backgroundColor (Css.hex "000000")
  , Css.color (Css.hex "ffffff")
  , Css.borderRadius4 Css.zero (mm 1) Css.zero Css.zero
  , Css.paddingRight (mm 0.4)
  , Css.paddingTop (mm 0.4)
  , Css.fontWeight (Css.int 900)
  ]
