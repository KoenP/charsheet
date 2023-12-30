module Page.CardsPage exposing (..)

import Css exposing (Style, px, mm)
import Css.Media
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import List
import Platform.Cmd as Cmd
import String

import Decoder.CharacterSheet exposing (sheetDec)
import Request exposing (requestUrl)
import Types exposing (..)
import Util exposing (simple)


----------------------------------------------------------------------
-- INITIALIZE
----------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "sheet" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCardsData)
               sheetDec
    }

----------------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------------
update : Msg -> Model -> CharacterSheet -> (Model, Cmd Msg)
update msg model sheet = (model, Cmd.none)

----------------------------------------------------------------------
-- VIEW
----------------------------------------------------------------------
view : CharacterSheet -> List (Html Msg)
view sheet = List.map viewSpellcastingSection sheet.spellcasting_sections

viewSpellcastingSection : SpellcastingSection -> Html Msg
viewSpellcastingSection section =
  div []
    [ div
       [ Attr.css cardsStyle ]
       (List.map (viewCard section.origin) section.spells)
    ]

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
    , div [ Attr.css descriptionStyle ] <|
        (spell.description
          |> List.map (text >> List.singleton
                            >> p [Attr.css [ Css.marginBottom (px 4)
                                           , Css.marginTop (px 0)]]))
        ++
        viewHigherLevelP spell.higher_level
    ]
    ++
    viewConcentrationBadge spell.concentration
    ++ 
    [ div [ Attr.css cardTypeStyle ] [text (origin ++ " spell")]
    ]

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
  if spell.level == 0
  then spell.school ++ " cantrip"
  else Util.ordinal spell.level ++ " level " ++ spell.school

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
    V -> "V"
    S -> "S"
    M _ -> "M"
                  

----------------------------------------------------------------------
-- STYLE
----------------------------------------------------------------------
cardsStyle : List Style
cardsStyle =
  [ Css.displayFlex
  , Css.flexDirection Css.row
  , Css.flexWrap Css.wrap
  ]
  
cardStyle : List Style
cardStyle =
  [ Css.position Css.relative
  , Css.border3 (px 1) Css.solid (Css.rgb 0 0 0)
  , Css.width (mm 63)
  , Css.minWidth (mm 63)
  , Css.height (mm 88)
  , Css.minHeight (mm 88)

  , Css.boxSizing Css.borderBox

  , Css.backgroundColor (Css.hex "e8e8e8") |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.fontFamilies ["Dosis"]
  , Css.padding (px 8)

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
  , Css.padding (mm 1)
  ]

descriptionStyle : List Style
descriptionStyle =
  [ Css.fontSize (px 8)
  , Css.lineHeight (px 10)
  , Css.textAlign Css.justify
  , Css.backgroundColor (Css.hex "ffffff")
  , Css.property "print-color-adjust" "exact"
  , Css.borderRadius (px 8)
  , Css.padding (mm 1)
  , Css.marginBottom (mm 1)
  ]

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
