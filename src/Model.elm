module Model
  exposing
    ( Model
    , Id
    , Post
    , Resource(..)
    , Section(..)
    , Content(..)
    , encodePost
    , postDecoder
    , init
    )

import Time exposing (Time)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))

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

type Content =
  Url String | Text String

type Resource id a =
  NotLoaded id | Loaded a

type Section =
  Top
  | New
  | Show
  | Ask
  | Jobs

type alias Model =
  { posts : List (Resource Id Post)
  , time : Time
  , page : Int
  , offset : Int
  , section : Section
  }

init : Model
init = { page = 0, posts = [], time = 0, offset = 0, section = Top }

-- Serialization

postDecoder : Decoder Post
postDecoder =
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
      `apply` contentDecoder

contentDecoder =
  let
    urlDecoder = Decode.map Url ("url" := Decode.string)
    textDecoder = Decode.map Text ("text" := Decode.string)
  in
    Decode.maybe <| Decode.oneOf [urlDecoder, textDecoder]

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
    , ("content", encodeContent post.content)
    ]

encodeContent content =
  case content of
    Just (Url string) -> Encode.string string
    Just (Text string) -> Encode.string string
    Nothing -> Encode.null
