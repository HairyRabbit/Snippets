port module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (program)
import List.Extra exposing (inits)
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
  regex "\\$\\{(\\d+)\\:(\\w+)\\}"


findPlaceholder =
  Regex.find Regex.All regexPlaceholder


strInits =
  toList >> inits >> List.map fromList


init : (Model, Cmd Msg)
init =
  let
    name = "Class"
    body = yas
    _ =
      Debug.log "test" <| strInits name
    _ =
      Debug.log "matched" <| toString <| List.map .submatches <| findPlaceholder yas
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
      Debug.log "test" <| (toList >> inits >> List.map fromList) model.name
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
