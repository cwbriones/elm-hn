module Feed.Model
  exposing
    ( Feed
    , Section(..)
    , stringToSection
    , sectionToString
    , init
    , Post
    , Id
    , Resource(..)
    , Content(..)
    , postDecoder
    )

import Time exposing (Time)
import Json.Decode as Decode exposing (Decoder, (:=))

type alias Feed =
  { posts : List (Resource Id Post)
  , page : Int
  , offset : Int
  , section : Section
  }

type Section =
  Top
  | New
  | Show
  | Ask
  | Jobs

stringToSection : String -> Maybe Section
stringToSection string =
  case string of
    "top" -> Just Top
    "new" -> Just New
    "show" -> Just Show
    "ask" -> Just Ask
    "jobs" -> Just Jobs
    _ -> Nothing

sectionToString : Section -> String
sectionToString section =
  case section of
    Top -> "top"
    New -> "new"
    Show -> "show"
    Ask -> "ask"
    Jobs -> "jobs"

init : Feed
init = { page = 0, posts = [], offset = 0, section = Top }

-- Posts

type Content =
  Url String | Text String

type alias Id = Int

type alias Post =
  { by : String
  , descendants : Int
  , id: Id
  , kids: List Id
  , score : Int
  , time : Time
  , title : String
  , content : Maybe Content
  }

type Resource id a = NotLoaded id | Loaded a

-- Serialization

postDecoder : Decoder Post
postDecoder =
  let
    succeed = Decode.succeed
    apply = Decode.object2 (<|)
    decodeIntList = Decode.list Decode.int
    decodeKids = decodeWithDefault [] ("kids" := decodeIntList)
    decodeDesc = decodeWithDefault 0 ("descendants" := Decode.int)

    contentDecoder =
      let
        urlDecoder = Decode.map Url ("url" := Decode.string)
        textDecoder = Decode.map Text ("text" := Decode.string)
      in
        Decode.maybe <| Decode.oneOf [urlDecoder, textDecoder]

    decodeWithDefault default decoder =
      Decode.maybe decoder |> Decode.map (Maybe.withDefault default)
  in
    succeed Post
      `apply` ("by" := Decode.string)
      `apply` decodeDesc
      `apply` ("id" := Decode.int)
      `apply` decodeKids
      `apply` ("score" := Decode.int)
      `apply` ("time" := Decode.float)
      `apply` ("title" := Decode.string)
      `apply` contentDecoder
