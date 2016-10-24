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
import Time exposing (Time)

import Post exposing (Post, Id, encodePost, postDecoder)

baseUrl = "https://hacker-news.firebaseio.com/v0/"

top : Int -> Int -> Task Http.Error (List Int)
top = fetchSection "topstories.json"

new : Int -> Int -> Task Http.Error (List Int)
new = fetchSection "newstories.json"

best : Int -> Int -> Task Http.Error (List Int)
best = fetchSection "beststories.json"

ask : Int -> Int -> Task Http.Error (List Int)
ask = fetchSection "askstories.json"

show : Int -> Int -> Task Http.Error (List Int)
show = fetchSection "showstories.json"

job : Int -> Int -> Task Http.Error (List Int)
job = fetchSection "jobstories.json"

fetchSection section n limit =
  let
    url = baseUrl ++ section
    request = Http.get (Decode.list Decode.int) url
    page = List.drop (n * limit) >> List.take limit
  in
    Task.map page request

item : Id -> Task Http.Error Post
item id =
  let
    url = baseUrl ++ "item/" ++ (toString id) ++ ".json"
    request = Http.get postDecoder url
  in
    request
