module Model
  exposing
    ( Model
    , init
    )

import Time exposing (Time)
import Debug exposing (log)

import Hop.Types exposing (Address)

import Feed.Model as Feed exposing (Feed)
import Comments.Model as Comments
import Routing exposing (Route(..), reverse)

type alias Model =
  { route : Route
  , address : Address
  , time : Time
  , feedModel : Feed
  , commentsModel : Comments.Model
  }

init : ( Route, Address ) -> Model
init (route, address) =
  let
    model = defaultModel route address
  in
    case route of
      FeedRoute sec ->
        let
          feed = model.feedModel
        in
        {model|
          feedModel = {feed|section = sec}
        }
      CommentsRoute id ->
        {model|
          commentsModel = Comments.init id,
          address = log "routes" address
        }
      _ -> model

defaultModel : Route -> Address -> Model
defaultModel route address =
  { route = route
  , address = address
  , time = 0
  , feedModel = Feed.init
  , commentsModel = Comments.init 0
  }
