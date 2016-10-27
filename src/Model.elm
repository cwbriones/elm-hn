module Model
  exposing
    ( Model
    , init
    )

import Time exposing (Time)

import Hop.Types exposing (Address)

import Feed.Model as Feed exposing (Feed)
import Routing exposing (Route(..))

type alias Model =
  { route : Route
  , address : Address
  , time : Time
  , feedModel : Feed
  }

init : ( Route, Address ) -> ( Model, Cmd a )
init (route, address) =
  { route = route
  , address = address
  , time = 0
  , feedModel = Feed.init
  } ! []

