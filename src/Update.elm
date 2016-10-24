module Update
  exposing
    ( update
    , initialize
    , Msg(..)
    )

import Time exposing (Time)

import Model exposing (Model)
-- import Post exposing (Post, Id, Resource(..))
import Feed.Model exposing (Feed, Section(..))
import Feed.Update

import Navigation
import Nav exposing (DesiredPage(..))

-- import Debug exposing (log)

type Msg =
  Tick Time
  | GoTo DesiredPage
  | FeedMsg Feed.Update.Msg
  | Page DesiredPage

type alias Delegator msg model =
  { tagger : msg -> Msg
  , get : Model -> model
  , set : model -> Model -> Model
  , update : (msg -> model -> (model, Cmd msg))
  }

initialize : Model -> (Model, Cmd Msg)
initialize model =
    feedInitialize Top model

feedInitialize section model =
  case Feed.Update.initialize section of
    (feed, feedMsg) -> ({model|nav = feed}, Cmd.map FeedMsg feedMsg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    setNav nav model = {model|nav = nav}
  in
    case msg of
      GoTo page -> {model|nav=page} ! []
      Tick time -> {model | time = time} ! []
      FeedMsg submsg -> delegate feedDelegator submsg model
      Page desiredPage ->
        case desiredPage of
          Feed section -> feedInitialize section model

-- Fanout Message Types

delegate : Delegator msg model -> msg -> Model -> (Model, Cmd Msg)
delegate d msg model =
  case d.update msg (d.get model) of
    (new, newMsg) -> (d.set new model, Cmd.map d.tagger newMsg)

feedDelegator : Delegator Feed.Update.Msg Feed
feedDelegator =
  { tagger = FeedMsg
  , get = .nav
  , set = \f model -> {model|nav = f}
  , update = Feed.Update.update
  }
