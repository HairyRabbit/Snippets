port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (program)
import List.Extra exposing (inits, uniqueBy, last, (!!))
import String exposing (toList, fromList)
import Regex exposing (Regex, regex, contains)
import Time
import Debug


main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { name : String
  , body : String
  , curr : Maybe String
  , idx  : Int
  , frames : List String
  }

type alias YasPlaceholder =
  { num : Int
  , ctx : String
  }

type alias YasPlaceholderExpand =
  { yas    : YasPlaceholder
  , expand : List String
  }


type Msg
  = NoOp
  | Tick Time.Time


yas : String
yas = """
var ${1:name} = new Class({
  initialize: function(${2:test}) {
    ${3:content}
    $4
  }$1
});
"""


regexPlaceholder : Regex
regexPlaceholder =
  --regex "\\$\\{(\\d+)\\:(\\w+)\\}"
  Regex.regex "\\$\\{*(\\d+)\\:*(\\w+)*\\}*"

regexPlaceholderReplacer : String -> Regex
regexPlaceholderReplacer num =
  Regex.regex <| "\\$\\{*" ++ num ++ "\\:*(\\w+)*\\}*"


findPlaceholder =
  Regex.find Regex.All regexPlaceholder



numberForMatched : List (Maybe String) -> YasPlaceholder
numberForMatched m =
  let
    def1 =
      Result.withDefault 1 << String.toInt
  in
    case m of
      Just num::Just content::m ->
        YasPlaceholder (def1 num) content
      Just num::Nothing::m ->
        YasPlaceholder (def1 num) ""
      _ ->
        YasPlaceholder 1 ""


replaceTpl : YasPlaceholderExpand -> List String -> List String
replaceTpl ph acc =
  let
    re =
      regexPlaceholderReplacer <| toString ph.yas.num
    tpl =
      Maybe.withDefault yas (last acc)
    fx ctx =
      Regex.replace Regex.All re (\{ match } -> ctx) tpl
    list =
      case ph.expand of
        [] -> (fx "") :: []
        _  -> List.map fx ph.expand
  in
    List.append acc list


expandPlaceholderContent : YasPlaceholder -> YasPlaceholderExpand
expandPlaceholderContent ({ ctx } as ph) =
  case String.isEmpty ctx of
    False ->
      YasPlaceholderExpand ph <| strInits ctx
    _ ->
      YasPlaceholderExpand ph []


strInits =
  toList >> inits >> List.map fromList >> List.filter (not << String.isEmpty)


init : (Model, Cmd Msg)
init =
  let
    name = "Class"
    body = yas
    f1 =
      Debug.log "test" <| strInits name
    f2 =
      Debug.log "matched"
        --<| toString
        <| List.foldl replaceTpl []
        <| List.map expandPlaceholderContent
        <| List.reverse
        <| List.sortBy .num
        <| uniqueBy .num
        <| List.map numberForMatched
        <| List.map .submatches
        <| findPlaceholder yas
    frames = List.append f1 f2
    idx = 0
  in
    (Model name body (frames !! idx) idx frames, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      let
        idx' = model.idx + 1
      in
        ( { model | curr = (model.frames !! idx'), idx = idx'}
        , Cmd.none
        )
    _ ->
      (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  --Sub.none
  Sub.batch
     [Time.every (Time.second / 8) Tick]

{-
view : Model -> Html Msg
view model =
  main' []
    [ span [] [ text model ]
    , span [] [ text "Hentais" ]
    , span [] [ text "Hentais 2" ]
    ]
-}

view : Model -> Html Msg
view model =
  let
    content =
      case model.curr of
        Just c ->
          c
        Nothing ->
          ""
  in
    main' [ class "yas-container" ]
    [ section [ class "yas-video" ]
        [ header [ class "yas-video-top" ] [ text "Emacs Yasnippet" ]
        , section [ class "yas-video-middle" ]
            [ pre [ class "yas-video-body" ] [ text content ]
            ]
        , footer [ class "yas-video-bottom" ] [ text "|>" ]
        ]
    ]

