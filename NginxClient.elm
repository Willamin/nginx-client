port module NginxClient exposing (..)

-- IMPORTS

import Date
import Debug exposing (..)
import Erl exposing (Url)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeValue, succeed, string, int, oneOf, null, list, bool, maybe, (:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Json
import Regex
import String
import Task


main =
  Html.program
    { init = init "http://localhost"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { url : Url
  , entities : List Entity
  }

init : String -> (Model, Cmd Msg)
init baseurl =
  let
    model : Model
    model =
      Model (Erl.parse baseurl) []
  in
    ( model
    , getListing model.url
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
      let
        newmodel : Model
        newmodel =
          Model (Erl.appendPathSegments [newpath] model.url) []
      in
        (newmodel, getListing newmodel.url)

    FetchSucceed new_entities ->
      ({ model | entities = new_entities }, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text (Erl.toString model.url)]
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
  let item =
    case ent.type' of
      "directory" ->
        { name = " " ++ ent.name ++ "/"
        , icon = "file-directory"
        , href = "#"
        , extra = [ onClick (Navigate ent.name) ]
        , size' = ""
        }
      "file" ->
        { name = ent.name
        , icon = "file"
        , href = Erl.toString m.url
        , extra = []
        , size' = toString (Maybe.withDefault 0 ent.size')
        }
      _ ->
        { name = ""
        , icon = "alert"
        , href = "#"
        , extra = []
        , size' = ""
        }
  in
    a ( [ class "list-group-item", href item.href ] ++ item.extra )
      [ span [ class ( "octicon octicon-" ++ item.icon ) ] []
      , text item.name
      , p []
        [ span [] [ small [ class "text-muted" ] [ text ent.mtime ] ]
        , span [ class "pull-right" ] [ text item.size' ]
        ]
      ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP


getListing : Url -> Cmd Msg
getListing url =
  Task.perform FetchFail FetchSucceed (Http.get decodeEntityList (Erl.toString url))
