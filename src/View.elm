module View
  exposing
    (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Time exposing (Time)

import Model exposing (Model, Post, Id, Resource(..), Section(..))
import Update exposing (Msg(..))

view : Model -> Html Msg
view model =
  div [class "container"]
    [ viewHeader
    , viewPosts model
    ]

viewHeader : Html Msg
viewHeader =
  let
    sectionLink txt sec = a [ href ("#" ++ txt), onClick (Section sec) ] [ text txt ]
  in
    header []
      [ a [ href "#", onClick (Section Top), id "site-title" ] [ text "Hacker News" ]
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

viewPosts : Model -> Html Msg
viewPosts model =
  let
    viewResource index res =
      case res of
        NotLoaded _ -> placeholder (model.offset + index)
        Loaded p -> viewPost model.time (model.offset + index) p
    moreLink = a [ href "#top", onClick (Page (model.page + 1)) ] [ text "More" ]
  in
    ul [class "post-list"] ((List.indexedMap viewResource model.posts) ++ [moreLink])

placeholder : Int -> Html Msg
placeholder index =
  let
    indexString = toString (index + 1) ++ ". "
  in
    li [class "post loading"] [ text (indexString ++ " Loading ...") ]

viewPost : Time -> Int -> Post -> Html Msg
viewPost time index post =
  let
    indexString = toString (index + 1) ++ ". "
    by = (toString post.score) ++ " points by " ++ post.by
    comments = (toString <| List.length post.kids) ++ " comments"
    timeAgo = showTime (Time.inSeconds time - post.time)
    info = String.join " | " [ by, timeAgo, comments ]

    showUrl =
      strip "http://"
      >> strip "https://"
      >> strip "www."
      >> (takeUntil '/')
  in
    li [class "post"]
      [ text indexString
      , a [ href post.url ] [ text post.title ]
      , span [ class "url" ]
        [ text "("
        , a [ href post.url ] [ text (showUrl post.url) ]
        , text ")"
        ]
      , p [ class "info" ] [ text info ]
      ]

showTime : Float -> String
showTime secF =
  let
    sec = floor secF
    min = floor (secF / 60)
    hours = floor (secF / 3600)
    days = floor (secF / (24 * 3600))

    showUnit n unit =
      let
        base = (toString n) ++ " " ++ unit
      in
       if n > 1
         then base ++ "s ago"
         else base ++ " ago"
  in
    if secF > 0
      then
        case (days, hours, min, sec) of
          (0, 0, 0, sec) -> showUnit sec "second"
          (0, 0, min, _) -> showUnit min "minute"
          (0, hours, _, _) -> showUnit hours "hour"
          (days, _, _, _) -> showUnit days "day"
      else
        "some time ago"

-- String Utilities

takeUntil : Char -> String -> String
takeUntil char string =
  let
    takeUntil' char index string =
      case String.uncons string of
        Just (hd, tail) ->
          if hd == char then Just index
                       else takeUntil' char (index + 1) tail
        Nothing -> Nothing
  in
    takeUntil' char 0 string
    |> Maybe.map (\idx -> String.slice 0 idx string)
    |> Maybe.withDefault string

strip : String -> String -> String
strip prefix string =
  case String.startsWith prefix string of
    True -> String.dropLeft (String.length prefix) string
    False -> string
