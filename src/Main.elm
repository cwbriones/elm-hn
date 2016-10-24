import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import String
import Regex
import Task exposing (Task)
import Time exposing (Time, second)
import Debug exposing (log)

import Api exposing (Id, Post)

type alias Model =
  { posts : List (Resource Id Post)
  , time : Time
  , page : Int
  , offset: Int
  }

type Resource id a =
  NotLoaded id | Loaded a

type Msg =
  Tick Time
  | Page Int
  | FetchFail
  | FetchPost Post
  | FetchIds (List Id)

initModel = { page = 0, posts = [], time = 0, offset = 0 }

init = fetchPage initModel 0

fetchPage model page =
  let
    pageSize = 30
    topPosts = Api.top page pageSize
    emptyPage = List.map NotLoaded (List.repeat pageSize 0)

    fetchTop = Task.perform (always FetchFail) FetchIds topPosts
    getTime = Task.perform (always FetchFail) Tick Time.now

    newModel =
      { model | posts = emptyPage, time = 0, page = page, offset = page * pageSize }
  in
    (newModel, Cmd.batch [fetchTop, getTime])

update msg model =
  let
    noCmd a = (a, Cmd.none)
    fetchPost id = Api.item id |> Task.perform (always FetchFail) FetchPost
    fetchPosts ids = List.map fetchPost ids |> List.reverse |> Cmd.batch
  in
    case msg of
      Tick time -> noCmd { model | time = time }
      Page page -> fetchPage model page
      FetchFail -> noCmd model
      FetchPost post -> noCmd { model | posts = insertPost post model.posts }
      FetchIds ids ->
        ({ model | posts = List.map NotLoaded ids }, fetchPosts (log "got ids " ids))

insertPost newPost posts =
  let
    replaceById post =
      case post of
        NotLoaded i ->
          if i == newPost.id then Loaded newPost
                           else NotLoaded i
        loaded -> loaded
  in
    List.map replaceById posts

main =
  App.program
    { subscriptions = \_ -> Time.every second Tick
    , init = init
    , update = update
    , view = view
    }

view model =
  div [class "container"]
    [ viewPosts model
    , a [ href "#top", onClick (Page (model.page + 1)) ] [ text "More" ]
    ]

viewPosts : Model -> Html Msg
viewPosts model =
  let
    viewResource index res =
      case res of
        NotLoaded _ -> placeholder (model.offset + index)
        Loaded p -> viewPost model.time (model.offset + index) p
  in
    ul [class "post-list"] (List.indexedMap viewResource model.posts)

placeholder index =
  let
    indexString = toString (index + 1) ++ ". "
  in
    li [class "post loading"] [ text (indexString ++ " Loading ...") ]

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
