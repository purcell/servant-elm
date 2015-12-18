module Example where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Signal exposing (Signal, Address)
import Task
import Effects exposing (Effects, Never)
import StartApp
import Http
import API



type alias User = {
    name : String
  , age : Int
  }

type alias Discovery = {
    details : String
  , year : Int
  }

decodeUser : Json.Decoder User
decodeUser = Json.object2 User ("name" := Json.string) ("age" := Json.int)

decodeDiscovery : Json.Decoder Discovery
decodeDiscovery = Json.object2 Discovery ("details" := Json.string) ("year" := Json.int)


type alias Model =
  { users : Maybe (List User)
  , discovery : Maybe Discovery
  }

emptyModel =
  { users = Nothing
  , discovery = Nothing
  }


type Action
  = NoOp
  | FetchUsers
  | FetchDiscovery
  | ReceiveUsers (Maybe (List User))
  | ReceiveDiscovery (Maybe (Discovery))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp               -> noFx model
    FetchUsers         -> (model, fetchUsers)
    FetchDiscovery     -> (model, fetchDiscovery 10)
    ReceiveUsers s     -> noFx { model | users = s }
    ReceiveDiscovery s -> noFx { model | discovery = s }

noFx : a -> (a, Effects b)
noFx m = (m, Effects.none)

view : Address Action -> Model -> Html
view address model =
  div []
      [ h2 [] [text "Users"]
      , button [onClick address FetchUsers] [text "Fetch users"]
      , div [] [ case model.users of
                   Nothing -> text "Nobody here, sorry"
                   Just users -> div [] (List.map renderUser users)
               ]
      , h2 [] [text "Discovery"]
      , button [onClick address FetchDiscovery] [text "Fetch discovery"]
      , div [] [ case model.discovery of
                   Nothing -> text "Nothing here, sorry"
                   Just discovery -> renderDiscovery discovery
               ]
      ]

renderUser : User -> Html
renderUser user = div [] [ text user.name, text " - ", text (toString user.age) ]

renderDiscovery : Discovery -> Html
renderDiscovery discovery = div [] [ text discovery.details, text " - ", text (toString discovery.year) ]

fetchUsers : Effects Action
fetchUsers =
  api.getUsers (Json.list decodeUser)
    |> Debug.log "result"
    |> Task.toMaybe
    |> Task.map ReceiveUsers
    |> Effects.task

fetchDiscovery : Int -> Effects Action
fetchDiscovery num =
  api.getDiscoveryById decodeDiscovery num
    |> Debug.log "result"
    |> Task.toMaybe
    |> Task.map ReceiveDiscovery
    |> Effects.task

api = API.getAPI { baseURI = "http://localhost:8081"
                 , settings = Http.defaultSettings }

app = StartApp.start
      { init = (emptyModel, Effects.none)
      , update = update
      , view = view
      , inputs = []
      }

main = app.html

-- Actually run the app's tasks
port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
