module Api
  exposing
    ( top
    , new
    , best
    , ask
    , show
    , job
    , item
    )

import Http
import Json.Decode as Decode
import Task exposing (Task)
import Debug exposing (log)

import Feed.Model exposing (Post, Id, postDecoder)

type Error
  = JsonError String
  | HttpError Http.Error

baseUrl : String
baseUrl = "https://hacker-news.firebaseio.com/v0/"

top : Int -> Int -> Task Error (List Int)
top = fetchSection "topstories.json"

new : Int -> Int -> Task Error (List Int)
new = fetchSection "newstories.json"

best : Int -> Int -> Task Error (List Int)
best = fetchSection "beststories.json"

ask : Int -> Int -> Task Error (List Int)
ask = fetchSection "askstories.json"

show : Int -> Int -> Task Error (List Int)
show = fetchSection "showstories.json"

job : Int -> Int -> Task Error (List Int)
job = fetchSection "jobstories.json"

fetchSection section n limit =
  let
    url = baseUrl ++ section
    request = loggedGet (Decode.list Decode.int) url
    page = List.drop (n * limit) >> List.take limit
  in
    Task.map page request

item : Id -> Task Error Post
item id =
  let
    url = baseUrl ++ "item/" ++ (toString id) ++ ".json"
    request = loggedGet postDecoder url
  in
    request

loggedGet : Decode.Decoder a -> String -> Task Error a
loggedGet decoder url =
  let
    decodeTask resp =
      case Decode.decodeString decoder resp of
        Ok a -> Task.succeed a
        Err m -> Task.fail <| JsonError (log "Json decode error" m)
    getTask = Http.getString url |> Task.mapError HttpError
  in
    getTask `Task.andThen` decodeTask
