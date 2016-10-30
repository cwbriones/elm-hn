module Model
  exposing
    ( Model
    , init
    )

import Time exposing (Time)
import Debug exposing (log)

import Feed.Model as Feed exposing (Feed)
import Comments.Model as Comments
import Routing exposing (Route(..), reverse)

type alias Model =
  { route : Route
  , time : Time
  , feedModel : Feed
  , commentsModel : Comments.Model
  }

init : Route -> Model
init route =
  let
    model = defaultModel route
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
          commentsModel = Comments.init id
        }
      _ -> model

defaultModel : Route -> Model
defaultModel route =
  { route = route
  , time = 0
  , feedModel = Feed.init
  , commentsModel = Comments.init 0
  }
