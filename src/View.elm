module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import String
import Debug exposing (log)

import Model exposing (Model)
import Messages exposing (Msg(..))
import Feed.Model exposing (Section(..))
import Feed.View as FeedView
import Comments.View as CommentsView
import Routing exposing (Route(..), linkTo, catchNavigationClicks)

view : Model -> Html Msg
view model =
  div [class "container", Routing.catchNavigationClicks (NavigateTo << log "caught event")]
    [ viewHeader
    , viewPage model
    ]

viewHeader : Html Msg
viewHeader =
  let
    sectionLink txt sec = (linkTo (FeedRoute sec)) [ ] [ text txt ]
  in
    header []
      [ (linkTo (FeedRoute TopStories)) [ id "site-title" ] [ text "Hacker News" ]
      , span [ id "nav" ]
        [ sectionLink "new" NewStories
        , text " | "
        , sectionLink "show" ShowStories
        , text " | "
        , sectionLink "ask" AskStories
        , text " | "
        , sectionLink "jobs" JobStories
        ]
      ]

viewPage : Model -> Html Msg
viewPage model =
  case model.route of
    FeedRoute _ ->
      App.map FeedMsg (FeedView.view model.time model.feedModel)
    CommentsRoute _ ->
      App.map CommentsMsg (CommentsView.view model.time model.commentsModel)
    _ ->
      div [] [ text "You come to a crossroads" ]

