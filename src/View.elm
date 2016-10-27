module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String

import Model exposing (Model)
import Messages exposing (Msg(..))
import Feed.Model exposing (Section(..))

-- View

view : Model -> Html Msg
view model =
  div [class "container"]
    [ viewHeader
    , viewPage model
    ]

viewHeader : Html Msg
viewHeader =
  let
    sectionLink txt sec = a [ href ("#" ++ txt), onClick (NavigateTo txt) ] [ text txt ]
  in
    header []
      [ a [ href "#", onClick (NavigateTo ""), id "site-title" ] [ text "Hacker News" ]
      , span [ id "nav" ]
        [ sectionLink "new" New
        , text " | "
        , sectionLink "show" Show
        , text " | "
        , sectionLink "ask" Ask
        , text " | "
        , sectionLink "jobs" Jobs
        ]
      ]

viewPage : Model -> Html Msg
viewPage model =
--   case model.route of
--     FeedRoute _ ->
--       viewFeed model.feedModel
--     _ ->
  text (String.join "/" model.address.path)

