module Page.EditCharacter exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as E
import Http
import Css exposing (Style)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Platform.Cmd as Cmd

import Request exposing (requestUrl)
import Types exposing (..)

--------------------------------------------------------------------------------
-- LOAD
--------------------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "options" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCharacterOptions)
               (D.list optionsDec)
    }

optionsDec : Decoder Options
optionsDec =
  D.succeed Options
    |> D.andMap (D.field "charlevel" D.int)
    |> D.andMap (D.field "choice" (D.succeed Nothing)) -- TODO (D.nullable D.string))
    |> D.andMap (D.field "id" D.string)
    |> D.andMap (D.field "origin" D.string)
    |> D.andMap (D.field "origin_category" D.string)
    |> D.andMap (D.field "spec" (D.succeed (ListSpec [])))

specDec : Decoder Spec
specDec =
  D.field "spectype" D.string |> 
  D.andThen
    (\spectype ->
       case spectype of
         "list"        -> D.map ListSpec (D.field "list" (D.list D.string))
         "or"          -> orSpecDec
         "from"        -> fromSpecDec False
         "unique_from" -> fromSpecDec True
         _             ->
           D.fail "spectype should be 'list', 'or', 'from', or 'unique_from'")

orSpecDec : Decoder Spec
orSpecDec =
  D.succeed (\lName lSpec rName rSpec -> OrSpec (lName,lSpec) (rName,rSpec))
    |> D.andMap (D.field "leftname" D.string)
    |> D.andMap (D.field "left" (D.lazy (\_ -> specDec)))
    |> D.andMap (D.field "rightname" D.string)
    |> D.andMap (D.field "right" (D.lazy (\_ -> specDec)))

fromSpecDec : Unique -> Decoder Spec
fromSpecDec unique =
  D.succeed (FromSpec unique)
    |> D.andMap (D.field "num" D.int)
    |> D.andMap (D.lazy (\_ -> specDec))

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------
update : Msg -> Model -> List Options -> (Model, Cmd Msg)
update msg model options =
  case msg of
    EditCharacterLevel selectedLevel ->
      applyPage model (EditCharacterPage options selectedLevel, Cmd.none)
    _ ->
      errorPage model "Page.EditCharacter.update called with unsupported message."
      

--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------
view : List Options -> Level -> List (Html Msg)
view opts selectedLevel =
  [ viewSideNav opts, viewMain opts selectedLevel ]
  
viewSideNav : List Options -> Html Msg
viewSideNav opts =
  div
    [ css sideNavStyle ]
    (opts
    |> List.map .charlevel
    |> List.sort
    |> nubSorted
    |> List.map viewSideNavLevelButton
    )
nubSorted : List a -> List a
nubSorted sortedList =
  case sortedList of
    x :: y :: ys -> 
      if x == y then nubSorted (y :: ys) else x :: nubSorted (y :: ys)
    _            ->
      sortedList

viewSideNavLevelButton : Int -> Html Msg
viewSideNavLevelButton lvl =
  button
    [ css sideNavButtonStyle
    , E.onClick (EditCharacterLevel lvl)
    ]
    [ text ("Level " ++ String.fromInt lvl) ]


viewMain : List Options -> Level -> Html Msg
viewMain opts selectedLevel =
  div
    [ css
        [ Css.marginLeft (Css.px 160)
        , Css.padding2 (Css.px 0) (Css.px 10)
        ]
    ]
    [ text ("Level " ++ String.fromInt selectedLevel) ]

--------------------------------------------------------------------------------
-- STYLES
--------------------------------------------------------------------------------

sideNavStyle : List Style
sideNavStyle = 
  [ Css.height (Css.pct 100)
  , Css.width (Css.px 160)
  , Css.position Css.fixed
  , Css.zIndex (Css.int 1)
  , Css.top Css.zero
  , Css.left Css.zero
  , Css.backgroundColor (Css.hex "010101")
  , Css.overflowX Css.hidden
  ]

sideNavButtonStyle : List Style
sideNavButtonStyle =
  [ Css.backgroundColor Css.transparent
  , Css.border Css.zero
  , Css.marginLeft (Css.px 20)
  , Css.marginTop (Css.px 15)
  , Css.padding Css.zero
  , Css.color (Css.hex "818181")
  , Css.cursor Css.pointer
  , Css.fontSize (Css.px 25)
  , Css.display Css.block
  , Css.hover [Css.color (Css.hex "ffffff")]
  ]
