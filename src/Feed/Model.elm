module Feed.Model
  exposing
    ( Feed
    , Section(..)
    , stringToSection
    , sectionToString
    , init
    , Post
    , PostType(..)
    , PostMeta
    , Id
    , Resource(..)
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

type alias Id = Int

type alias Post =
  { by : String
  , descendants : Int
  , id: Id
  , kids: List Id
  , postType : PostType
  , time : Time
  , meta : Maybe PostMeta
  , text : String
  }

type PostType
  = Comment
  | Story
  -- | Ask
  -- | Job
  -- | Poll

type alias PostMeta =
  { title : String
  , url : String
  , score : Int
  }

type Resource id a = NotLoaded id | Loaded a

-- Serialization

postDecoder : Decoder Post
postDecoder =
  let
    succeed = Decode.succeed
    apply = Decode.object2 (<|)
    decodeIntList = Decode.list Decode.int
  in
    succeed Post
      `apply` ("by" := Decode.string)
      `apply` decodeWithDefault 0 ("descendants" := Decode.int)
      `apply` ("id" := Decode.int)
      `apply` decodeWithDefault [] ("kids" := decodeIntList)
      `apply` postTypeDecoder
      `apply` ("time" := Decode.float)
      `apply` (Decode.maybe postMetaDecoder)
      `apply` decodeWithDefault "" ("text" := Decode.string)

postTypeDecoder : Decoder PostType
postTypeDecoder =
  ("type" := Decode.string) `Decode.andThen` postTypeInfo

postTypeInfo : String -> Decoder PostType
postTypeInfo tag =
  let
    apply = Decode.object2 (<|)
    succeed = Decode.succeed
  in
    case tag of
      "comment" ->
        succeed Comment
      "story" ->
        succeed Story
      -- "job" ->
      --   succeed Job
      -- "poll" ->
      --   succeed Poll
      -- "ask" ->
      --   succeed Ask
      _ ->
        Decode.fail (tag ++ " is not a recognized post type")

postMetaDecoder : Decoder PostMeta
postMetaDecoder =
  let
    apply = Decode.object2 (<|)
  in
    (Decode.succeed PostMeta)
      `apply` ("title" := Decode.string)
      `apply` (decodeWithDefault "" ("url" := Decode.string))
      `apply` ("score" := Decode.int)

decodeWithDefault : a -> Decoder a -> Decoder a
decodeWithDefault default decoder =
  Decode.maybe decoder |> Decode.map (Maybe.withDefault default)
