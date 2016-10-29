module Routing
  exposing
    ( Route(..)
    , hopConfig
    , urlParser
    , reverse
    , catchNavigationClicks
    , linkTo
    )

import Json.Decode as Json
import Html.Events exposing (onWithOptions)
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (log)

import UrlParser exposing ((</>))
import Hop
import Hop.Types exposing (Config, Address, Query)
import Navigation

import Feed.Model exposing (Section(..), sectionToString)

type Route
  = FeedRoute Section
  | CommentsRoute Int
  | NotFoundRoute

hopConfig : Config
hopConfig =
  { hash = False
  , basePath = "/"
  }

urlParser : Navigation.Parser (Route, Address)
urlParser =
  let
    parse path =
      path
      |> UrlParser.parse identity routes
      |> Result.withDefault NotFoundRoute

    resolver =
      Hop.makeResolver hopConfig parse
  in
    Navigation.makeParser (.href >> resolver)

routes : UrlParser.Parser (Route -> a) a
routes =
  UrlParser.oneOf
    [ UrlParser.format FeedRoute sectionParser
    , UrlParser.format (CommentsRoute) (UrlParser.s "item" </> UrlParser.int)
    ]

sectionParser : UrlParser.Parser (Section -> a) a
sectionParser =
  let
    checkTag string =
      case string of
        "top" -> Ok TopStories
        "" -> Ok TopStories
        "jobs" -> Ok JobStories
        "new" -> Ok NewStories
        "ask" -> Ok AskStories
        "show" -> Ok ShowStories
        _ -> Err string
  in
    UrlParser.custom "SECTION" checkTag

reverse : Route -> String
reverse route =
  case route of
    FeedRoute TopStories -> "/"
    FeedRoute section -> "/" ++ sectionToString section
    CommentsRoute id -> "/item/" ++ (toString id)
    NotFoundRoute -> "/"

-- Navigation Helpers for Views

catchNavigationClicks : (String -> msg) -> Html.Attribute msg
catchNavigationClicks tagger =
  let
    decoder = Json.map (log "decoding nav") pathDecoder
  in
    onWithOptions "click"
      { stopPropagation = True
      , preventDefault = True
      }
      (Json.map tagger (Json.at [ "target" ] decoder))

pathDecoder =
  Json.oneOf
    [ Json.at [ "data-navigate" ] Json.string
    , Json.at [ "parentElement" ] (lazy (\_ -> pathDecoder))
    , Json.fail "no path found for click"
    ]

navigate : Route -> Cmd msg
navigate route =
  Navigation.newUrl (reverse route)

linkTo : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attrs content =
  a ((linkAttrs route) ++ attrs) content

linkAttrs : Route -> List (Attribute msg)
linkAttrs route =
  let
    path =
      reverse route
  in
    [ href path
    , attribute "data-navigate" path
    ]

lazy : (() -> Json.Decoder a) -> Json.Decoder a
lazy getDecoder =
  Json.customDecoder Json.value <|
    \rawValue ->
       Json.decodeValue (getDecoder ()) rawValue
