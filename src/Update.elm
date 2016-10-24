module Update
  exposing
    ( update
    , initialize
    , Msg(..)
    )

import Task exposing (Task)
import Time exposing (Time)

import Api
import Model exposing (Model, Post, Id, Resource(..))

type Msg =
  Tick Time
  | Page Int
  | FetchFail
  | FetchPost Post
  | FetchIds (List Id)

initialize : Model -> (Model, Cmd Msg)
initialize model = fetchPage model model.page

update : Msg -> Model -> (Model, Cmd Msg)
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
        ({ model | posts = List.map NotLoaded ids }, fetchPosts ids)

fetchPage : Model -> Int -> (Model, Cmd Msg)
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


insertPost : Post -> List (Resource Id Post) -> List (Resource Id Post)
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

