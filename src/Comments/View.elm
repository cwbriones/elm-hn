module Comments.View
  exposing
    (view)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Time exposing (Time)

import Markdown

import Comments.Update exposing (Msg(..), fetchRoot)
import Feed.Model
  exposing
    (Post, Id, PostMeta, Resource(..), PostType(..))
import Comments.Model exposing (Model, CommentTree(..), rootTree)

view : Time -> Model -> Html Msg
view time model =
  let
    inner =
      case rootTree model of
        Just (post, comments) ->
            (viewStory time post model.commentCount) ++
            [ viewCommentTree time comments ]
        Nothing -> [placeholder]
  in
    div [ id "post-container" ] inner

viewStory : Time -> Post -> Int -> List (Html Msg)
viewStory time post commentCount =
  case (post.postType, post.meta) of
    (Story, Just meta) ->
      [viewStory' time meta post commentCount]
    _ ->
      []

viewCommentTree : Time -> Array (Resource Id CommentTree) -> Html Msg
viewCommentTree time =
  let
    timeAgo post = showTime (Time.inSeconds time - post.time)

    viewResource ct =
      case ct of
        Loaded ct -> (viewComment ct)
        NotLoaded id -> placeholder

    viewComment ((CommentTree tree) as ct) =
      li [class "comment"]
        [ span [ class "post-meta" ]
          [ text <| tree.post.by ++ " "
          , a [ onClick <| UpdateRoot ct ] [ text (timeAgo tree.post) ]
          ]
        , div [ class "comment-text" ] [
          Markdown.toHtml [] tree.post.text
        ]
        , viewCommentTree time tree.children
        ]
  in
    (Array.map viewResource)
    >> Array.toList
    >> (ul [class "comment-list"])

placeholder : Html Msg
placeholder =
  div [class "post loading"] [ text "Loading ..." ]

viewStory' : Time -> PostMeta -> Post -> Int -> Html Msg
viewStory' time meta post commentCount =
  let
    by = (toString meta.score) ++ " points by " ++ post.by
    comments = (toString commentCount) ++ " comments"
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

    viewMeta meta =
      if meta.url == ""
        then [ a [ ] [ text meta.title ] ]
        else viewUrl meta.url meta.title

    viewText post =
      if post.text == ""
        then []
        else [ div [class "post-body"] [ Markdown.toHtml [] post.text ] ]
  in
    div [class "post"]
      ([ div [class "post-title"] (viewMeta meta)
      , div [ class "post-meta" ] [ text info ]
      ] ++ (viewText post))

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
