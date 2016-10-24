import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)

import String
import Regex
import Task
import Debug exposing (log)
import Time exposing (Time, second)

import Cache exposing (getPostsOrFetch)
import Api exposing (Post)

type alias Model =
  { posts : List Post
  , time : Time
  }

type Msg =
  Tick Time
  | FetchFail
  | FetchSucceed (List Post)

init =
  let
    topPosts = (Api.top 0 30) `Task.andThen` getPostsOrFetch
    fetchTop = Task.perform (always FetchFail) FetchSucceed topPosts
  in
    ({ posts = [], time = 0 }, fetchTop)

noCmd a = (a, Cmd.none)

update msg model =
  case msg of
    Tick time -> noCmd { model | time = time }
    FetchFail -> noCmd model
    FetchSucceed posts -> noCmd { model | posts = posts }

main =
  App.program
    { subscriptions = \_ -> Time.every second Tick
    , init = init
    , update = update
    , view = view
    }

view model =
  div [class "container"] [viewPosts model.time model.posts]

viewPosts : Time -> List Api.Post -> Html Msg
viewPosts time posts =
  ul [class "post-list"] (List.indexedMap (viewPost time) posts)

viewPost : Time -> Int -> Api.Post -> Html Msg
viewPost time index post =
  let
    indexString = toString (index + 1) ++ ". "
    by = (toString post.score) ++ " points by " ++ post.by
    comments = (toString <| List.length post.kids) ++ " comments"
    timeAgo = showTime (Time.inSeconds time - post.time)
    info = String.join " | " [ by, timeAgo, comments ]
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

showUnit n unit =
  let
    base = (toString n) ++ " " ++ unit
  in
   if n > 1
     then base ++ "s ago"
     else base ++ " ago"

showUrl =
  strip "http://"
  >> strip "https://"
  >> strip "www."
  >> (takeUntil '/')

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

strip prefix string =
  case String.startsWith prefix string of
    True -> String.dropLeft (String.length prefix) string
    False -> string
