module Feed.View
  exposing
    (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Time exposing (Time)

import Feed.Model exposing (Feed, Section(..), Post, Id, Resource(..), Content(..))
import Feed.Update exposing (Msg(..))

view : Time -> Feed -> Html Msg
view time model =
  let
    viewResource res =
      case res of
        NotLoaded _ -> placeholder
        Loaded p -> viewPost time p
    moreLink = a [ id "more", onClick <| Paginate (model.page + 1) ] [ text "More" ]
  in
    ol [class "post-list", start (model.offset + 1)]
      ((List.map viewResource model.posts) ++ [moreLink])

placeholder : Html Msg
placeholder =
  li [class "post loading"] [ text "Loading ..." ]

viewPost : Time -> Post -> Html Msg
viewPost time post =
  let
    by = (toString post.score) ++ " points by " ++ post.by
    comments = (toString <| List.length post.kids) ++ " comments"
    timeAgo = showTime (Time.inSeconds time - post.time)
    info = String.join " | " [ by, timeAgo, comments ]

    showUrl =
      strip "http://"
      >> strip "https://"
      >> strip "www."
      >> (takeUntil '/')

    viewUrl url title =
      [ a [ href url ] [ text title ]
      , span [ class "url" ]
          [ text "("
          , a [ href url ] [ text (showUrl url) ]
          , text ")"
          ]
       ]

    -- TODO: This should link to the item
    viewContent post =
      case post.content of
        Just (Url url) -> viewUrl url post.title
        _ -> [ a [ ] [ text post.title ] ]
  in
    li [class "post"]
      [ div [class "post-title"] (viewContent post)
      , div [ class "post-meta" ] [ text info ]
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
