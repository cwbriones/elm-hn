module Update
  exposing
    ( update
    , urlUpdate
    , subscriptions
    , initialize
    )

import Time exposing (Time, second)

import Hop
import Hop.Types exposing (Address)
import Navigation

import Routing exposing (hopConfig, Route(..))
import Messages exposing (Msg(..))
import Model exposing (Model)
import Feed.Update as FeedUpdate

initialize : Model -> (Model, Cmd Msg)
initialize model =
  let
    (newFeedModel, feedCmd) = FeedUpdate.initialize model.feedModel
  in
    {model|feedModel = newFeedModel} ! [Cmd.map FeedMsg feedCmd]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time -> {model|time = time} ! []
    NavigateTo path ->
      let
        command =
          Hop.outputFromPath hopConfig path
          |> Navigation.newUrl
      in
        (model, command)
    SetQuery query ->
      let
        command =
          model.address
          |> Hop.setQuery query
          |> Hop.output hopConfig
          |> Navigation.newUrl
      in
        (model, command)
    FeedMsg msg ->
      let
        (newFeedModel, feedCmd) = FeedUpdate.update msg model.feedModel
      in
        {model|feedModel = newFeedModel} ! [Cmd.map FeedMsg feedCmd]

urlUpdate : (Route, Address) -> Model -> (Model, Cmd Msg)
urlUpdate (route, address) model =
  case route of
    FeedRoute section ->
      let
        feedModel = model.feedModel
        (newFeedModel, feedCmd) = FeedUpdate.initialize {feedModel|section = section, page = 0}
      in
        {model|route = route, address = address, feedModel = newFeedModel} ! [Cmd.map FeedMsg feedCmd]
    _ ->
      {model|route = route, address = address} ! []

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

