module Feed.Model
  exposing
    ( Feed
    , Section(..)
    , stringToSection
    , sectionToString
    , init
    )

import Post exposing (Resource, Id, Post)

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
