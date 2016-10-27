module Update
  exposing
    ( update
    , urlUpdate
    , subscriptions
    )

import Time exposing (Time, second)
import Debug exposing (log)

import Hop
import Hop.Types exposing (Address)
import Navigation

import Routing exposing (hopConfig, Route(..))
import Messages exposing (Msg(..))
import Model exposing (Model)
-- import Feed.Model exposing (Feed, Section(..), Post, Id, Resource(..))
-- import Feed.Update

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time -> {model|time = time} ! []
    NavigateTo path ->
      let
        command =
          Hop.outputFromPath hopConfig (log "Navigate to" path)
          |> Navigation.newUrl
      in
        (log "navigateFrom" model, command)
    SetQuery query ->
      let
        command =
          model.address
          |> Hop.setQuery query
          |> Hop.output hopConfig
          |> Navigation.newUrl
      in
        (model, command)

urlUpdate : (Route, Address) -> Model -> (Model, Cmd a)
urlUpdate (route, address) model =
  {model|route = (log "Url update" route), address = address} ! []

-- Subscriptions

subscriptions : a -> Sub Msg
subscriptions _ = Time.every second Tick

-- type Msg =
--   Tick Time
--   | GoTo DesiredPage
--   | FeedMsg Feed.Update.Msg
--   | Page DesiredPage
-- 
-- type alias Delegator msg model =
--   { tagger : msg -> Msg
--   , get : Model -> model
--   , set : model -> Model -> Model
--   , update : (msg -> model -> (model, Cmd msg))
--   }
-- 
-- initialize : Model -> (Model, Cmd Msg)
-- initialize model =
--     feedInitialize Top model
-- 
-- feedInitialize section model =
--   case Feed.Update.initialize section of
--     (feed, feedMsg) -> ({model|nav = feed}, Cmd.map FeedMsg feedMsg)
-- 
-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--   let
--     setNav nav model = {model|nav = nav}
--   in
--     case msg of
--       GoTo page -> {model|nav=page} ! []
--       Tick time -> {model | time = time} ! []
--       FeedMsg submsg -> delegate feedDelegator submsg model
--       Page desiredPage ->
--         case desiredPage of
--           Feed section -> feedInitialize section model
-- 
-- -- Fanout Message Types
-- 
-- delegate : Delegator msg model -> msg -> Model -> (Model, Cmd Msg)
-- delegate d msg model =
--   case d.update msg (d.get model) of
--     (new, newMsg) -> (d.set new model, Cmd.map d.tagger newMsg)
-- 
-- feedDelegator : Delegator Feed.Update.Msg Feed
-- feedDelegator =
--   { tagger = FeedMsg
--   , get = .nav
--   , set = \f model -> {model|nav = f}
--   , update = Feed.Update.update
--   }
