port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (program)
import List.Extra exposing (inits, uniqueBy, last)
import String exposing (toList, fromList)
import Regex exposing (Regex, regex, contains)
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
  , curr : String
  }

type alias YasPlaceholder =
  { num : Int
  , ctx : String
  }

type alias YasPlaceholderExpand =
  { yas    : YasPlaceholder
  , expand : List String
  }

type Msg = NoOp


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
  -- List.map .submatches

       

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
    matchFx matcher =
      matcher.match
    --_ = Debug.log "acc" tpl
    _ = Debug.log "test" <| fx "$4"
    fx ctx =
      Regex.replace Regex.All re matchFx tpl
    list =
      case ph.expand of
        [] -> fx tpl
        _  -> String.join "" <| List.map fx ph.expand
  in
    list :: acc

    
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
    _ =
      Debug.log "test" <| strInits name
    _ =
      Debug.log "matched"
        <| toString
        <| List.foldl replaceTpl []
        <| List.map expandPlaceholderContent
        <| List.reverse
        <| List.sortBy .num
        <| uniqueBy .num
        <| List.map numberForMatched
        <| List.map .submatches
        <| findPlaceholder yas
  in
    (Model name body "C", Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    _ ->
      (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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
    _ =
      Debug.log "test"
        <| (toList >> inits >> List.map fromList >> List.filter (not << String.isEmpty))
        model.name
  in
    main' [ class "yas-container" ]
    [ section [ class "yas-video" ]
        [ header [ class "yas-video-top" ] [ text "Emacs Yasnippet" ]
        , section [ class "yas-video-middle" ]
            [ pre [ class "yas-video-body" ] [ text model.body ]
            ]
        , footer [ class "yas-video-bottom" ] [ text "|>" ]
        ]
    ]
