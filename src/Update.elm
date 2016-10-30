module Update
  exposing
    ( update
    , urlUpdate
    , subscriptions
    , initialize
    )

import Task
import Time exposing (Time, second)
import Debug exposing (log)

import Navigation

import Routing exposing (Route(..))
import Messages exposing (Msg(..), noop)
import Model exposing (Model)
import Feed.Update as FeedUpdate
import Comments.Model as Comments
import Comments.Update as CommentsUpdate

initialize : Model -> (Model, Cmd Msg)
initialize model =
  let
    (initialized, cmd) = urlUpdate model.route model
    fetchTime = Task.perform noop Tick Time.now
  in
    (initialized, Cmd.batch [cmd, fetchTime])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> model ! []
    Tick time -> {model|time = time} ! []
    NavigateTo path ->
      (model, Navigation.newUrl path)
    FeedMsg msg ->
      let
        (newFeedModel, feedCmd) = FeedUpdate.update msg model.feedModel
      in
        {model|feedModel = newFeedModel} ! [Cmd.map FeedMsg feedCmd]
    CommentsMsg msg ->
      let
        (newCommentsModel, commentsCmd) = CommentsUpdate.update msg model.commentsModel
      in
        {model|commentsModel = newCommentsModel} ! [Cmd.map CommentsMsg commentsCmd]

urlUpdate : Route -> Model -> (Model, Cmd Msg)
urlUpdate route model =
  case route of
    FeedRoute section ->
      let
        feedModel = model.feedModel
        (newFeedModel, feedCmd) = FeedUpdate.initialize {feedModel|section = section, page = 0}
      in
        {model|route = route, feedModel = newFeedModel} ! [Cmd.map FeedMsg feedCmd]
    CommentsRoute id ->
      let
        commentsModel = model.commentsModel
        (newCommentsModel, commentsCmd) = CommentsUpdate.initialize <| Comments.init id
      in
        {model|commentsModel = newCommentsModel} ! [Cmd.map CommentsMsg commentsCmd]
    _ ->
      model ! []

-- Subscriptions

subscriptions : a -> Sub Msg
subscriptions _ = Time.every second Tick

-- Fanout Message Types

type alias Delegator msg model =
  { tagger : msg -> Msg
  , get : Model -> model
  , set : model -> Model -> Model
  , update : (msg -> model -> (model, Cmd msg))
  }

delegate : Delegator msg model -> msg -> Model -> (Model, Cmd Msg)
delegate d msg model =
  case d.update msg (d.get model) of
    (new, newMsg) -> (d.set new model, Cmd.map d.tagger newMsg)

