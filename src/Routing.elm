module Routing
  exposing
    ( Route(..)
    , urlParser
    , reverse
    , linkTo
    )

import Json.Decode as Json
import Html.Events exposing (onWithOptions)
import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (href, attribute)
import String
import Debug exposing (log)

import UrlParser exposing ((</>), s)
import Navigation

import Feed.Model exposing (Section(..), sectionToString)

type Route
  = FeedRoute Section
  | CommentsRoute Int
  | NotFoundRoute

basePath : String
basePath = "elm-hn"

urlParser : Navigation.Parser Route
urlParser =
  let
    parse path =
      path
      |> UrlParser.parse identity routes
      |> Result.withDefault NotFoundRoute

    stripLeading path =
      case String.startsWith "/" path of
        True -> String.dropLeft 1 path
        False -> path
  in
    Navigation.makeParser (.pathname >> stripLeading >> parse)

routes : UrlParser.Parser (Route -> a) a
routes =
  UrlParser.oneOf
    [ UrlParser.format FeedRoute (s basePath </> sectionParser)
    , UrlParser.format (CommentsRoute) (s basePath </> s "item" </> UrlParser.int)
    ]

sectionParser : UrlParser.Parser (Section -> a) a
sectionParser =
  let
    checkTag string =
      case string of
        "" -> Ok TopStories
        "top" -> Ok TopStories
        "jobs" -> Ok JobStories
        "new" -> Ok NewStories
        "ask" -> Ok AskStories
        "show" -> Ok ShowStories
        _ -> Err string
  in
    UrlParser.custom "SECTION" checkTag

reverse : Route -> String
reverse route =
  let
    relative =
      case route of
        FeedRoute TopStories -> ""
        FeedRoute section -> sectionToString section
        CommentsRoute id -> "item/" ++ (toString id)
        NotFoundRoute -> ""
  in
    "/" ++ basePath ++ "/" ++ relative

-- Navigation Helpers for Views

navigate : Route -> Cmd msg
navigate route =
  Navigation.newUrl (reverse route)

linkTo : Route -> (String -> msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route tagger attrs content =
  let
    path = reverse route
    linkAttrs =
      [ href path
      , attribute "data-navigate" path
      , onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        } (Json.succeed (tagger path))
      ]
  in
    a (linkAttrs ++ attrs) content

