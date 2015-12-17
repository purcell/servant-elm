module API where

import Http
import Task
import Json.Decode as Decode

type alias Config = {
    baseURI : String
  , settings : Http.Settings
  }

type alias API a b = {
    fetchUsers : Decode.Decoder a -> Task.Task Http.Error a
  , fetchDiscovery : Decode.Decoder b -> Int -> Task.Task Http.Error b
  }

getAPI : Config -> API a b
getAPI config = {
    fetchUsers = fetchUsers config
  , fetchDiscovery = fetchDiscovery config
  }


fetchUsers : Config -> Decode.Decoder a -> Task.Task Http.Error a
fetchUsers config decoder =
  Http.send config.settings
        { verb = "GET"
        , headers = []
        , url = (config.baseURI ++ "/users")
        , body = Http.empty
        } |> Http.fromJson decoder

fetchDiscovery : Config -> Decode.Decoder a -> Int -> Task.Task Http.Error a
fetchDiscovery config decoder num =
  Http.send config.settings
        { verb = "GET"
        , headers = []
        , url = (config.baseURI ++ "/discovery/" ++ toString num)
        , body = Http.empty
        } |> Http.fromJson decoder
