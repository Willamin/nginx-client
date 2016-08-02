port module NginxClient exposing (..)

-- IMPORTS

import Date
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Json
import Json.Decode.Extra exposing ((|:))
import Json.Decode exposing (Decoder, decodeValue, succeed, string, int, oneOf, null, list, bool, maybe, (:=))
import Task
import Debug exposing (..)
import String


main =
  Html.program
    { init = init "http://localhost"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { baseurl : String
  , path: String
  , entities : List Entity
  }

init : String -> (Model, Cmd Msg)
init baseurl =
  ( Model baseurl "" []
  , getListing baseurl ""
  )

type alias Entity =
  { name : String
  , type' : String
  , mtime : String
  , size' : Maybe Int
  }

nullEntity : Entity
nullEntity = { name = "", type' = "", mtime = "", size' = Nothing}

decodeEntityList : Decoder (List Entity)
decodeEntityList =
  Json.Decode.list decodeEntity

decodeEntity : Decoder Entity
decodeEntity =
  succeed Entity
    |: ("name" := string)
    |: ("type" := string)
    |: ("mtime" := string)
    |: (maybe ("size" := int))

-- UPDATE


type Msg
  = Navigate String
  | FetchSucceed (List Entity)
  | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Navigate newpath ->
      (Model model.baseurl (joinPath [model.path, newpath]) [], getListing model.baseurl newpath)

    FetchSucceed entities ->
      (Model model.baseurl model.path entities, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text (model.baseurl ++ model.path)]
    , toHtmlList model
    ]

toHtmlList : Model -> Html Msg
toHtmlList model =
 let entities =
    [{nullEntity | type' = "directory", name = ".."}] ++ model.entities
  in
    div [ class "list-group" ] (List.map (toLi model) entities )
toLi : Model -> Entity -> Html Msg
toLi m ent =
  case ent.type' of
    "directory" ->
      a [ class "list-group-item", href "#", onClick (Navigate ent.name) ]
      [ span [ class "octicon octicon-file-directory" ] []
      , text (" " ++ ent.name ++ "/")
      , span [ class "pull-right" ] [ text ent.mtime ]
      ]
    "file" ->
      a [ class "list-group-item", href (joinPath [m.baseurl, m.path, ent.name]) ]
      [ span [ class "octicon octicon-file" ] []
      , text ent.name
      , span [ class "pull-right" ] [ text (toString (Maybe.withDefault 0 ent.size')) ]
      ]
    _ ->
      a [] [ text "derp" ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HELPER

joinPath : List String -> String
joinPath lst =
  String.join "/" lst


-- HTTP


getListing : String -> String -> Cmd Msg
getListing baseurl path =
  let
    url =
      joinPath [baseurl, path]
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeEntityList url)
