module Api
  exposing
    ( top
    , new
    , best
    , ask
    , show
    , job
    , item
    , Id
    , Post
    , decodePost
    , encodePost
    )

import Http
import Task exposing (Task)
import Debug exposing (log)
import Time exposing (Time)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))

baseUrl = "https://hacker-news.firebaseio.com/v0/"

type alias Id = Int

type alias Post =
  { by : String
  , descendants : Int
  , id: Id
  , kids: List Id
  , score : Int
  , time : Time
  , title : String
  , url : String
  }

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
    page = List.take limit >> List.drop (n * limit)
  in
    Task.map page request

item : Id -> Task Http.Error Post
item id =
  let
    url = log "fetch item" (baseUrl ++ "item/" ++ (toString id) ++ ".json")
    request = Http.get decodePost url
  in
    request

-- Serialization

decodePost : Decoder Post
decodePost =
  let
    succeed = Decode.succeed
    apply = Decode.object2 (<|)
    decodeIntList = Decode.list Decode.int
    decodeKids = decodeWithDefault [] ("kids" := decodeIntList)
    decodeDesc = decodeWithDefault 0 ("descendants" := Decode.int)
  in
    succeed Post
      `apply` ("by" := Decode.string)
      `apply` decodeDesc
      `apply` ("id" := Decode.int)
      `apply` decodeKids
      `apply` ("score" := Decode.int)
      `apply` ("time" := Decode.float)
      `apply` ("title" := Decode.string)
      `apply` ("url" := Decode.string)

decodeWithDefault default decoder =
  Decode.maybe decoder |> Decode.map (Maybe.withDefault default)

encodePost : Post -> Encode.Value
encodePost post =
  Encode.object
    [ ("by", Encode.string post.by)
    , ("descendants", Encode.int post.descendants)
    , ("id", Encode.int post.id)
    , ("kids", Encode.list (List.map Encode.int post.kids))
    , ("score", Encode.int post.score)
    , ("time", Encode.float post.time)
    , ("title", Encode.string post.title)
    , ("url", Encode.string post.url)
    ]

