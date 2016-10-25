module Routing
  exposing
    ( Route(..)
    , hopConfig
    , urlParser
    , reverse
    )

import UrlParser exposing ((</>), s)
import Hop
import Hop.Types exposing (Config, Address, Query)
import Navigation

import Feed.Model exposing (Section(..), sectionToString)

type Route
  = FeedRoute Section
  | CommentsRoute
  | NotFoundRoute

hopConfig : Config
hopConfig =
  { hash = True
  , basePath = ""
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
    , UrlParser.format CommentsRoute (s "item")
    ]

sectionParser : UrlParser.Parser (Section -> a) a
sectionParser =
  let
    checkTag string =
      case string of
        "top" -> Ok Top
        "" -> Ok Top
        "jobs" -> Ok Jobs
        "new" -> Ok New
        _ -> Err string
  in
    UrlParser.custom "SECTION" checkTag

reverse : Route -> String
reverse route =
  case route of
    FeedRoute section -> sectionToString section
    CommentsRoute -> "item"
    NotFoundRoute -> ""

