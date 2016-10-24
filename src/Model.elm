module Model
  exposing
    ( Model
    , init
    )

import Time exposing (Time)

import Feed.Model as Feed exposing (Feed)
import Nav exposing (DesiredPage(..))

type Resource id a = NotLoaded id | Loaded a

type alias Model =
  { nav : DesiredPage
  , time : Time
  }

init : Model
init = { nav = Feed Feed.init, time = 0 }

