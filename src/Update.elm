module Update
  exposing
    ( update
    , initialize
    , Msg(..)
    )

import Task exposing (Task)
import Time exposing (Time)

import Api
import Model exposing (Model, Post, Id, Resource(..), Section(..))

type Msg =
  Tick Time
  | Page Int
  | FetchFail
  | FetchPost Post
  | FetchIds (List Id)
  | Section Section

initialize : Model -> (Model, Cmd Msg)
initialize model = fetchPage model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    noCmd a = (a, Cmd.none)
    fetchPost id = Api.item id |> Task.perform (always FetchFail) FetchPost
    fetchPosts ids = List.map fetchPost ids |> List.reverse |> Cmd.batch
  in
    case msg of
      Tick time -> noCmd { model | time = time }
      Page page -> fetchPage {model | page = page}
      FetchFail -> noCmd model
      FetchPost post -> noCmd { model | posts = insertPost post model.posts }
      FetchIds ids ->
        ({ model | posts = List.map NotLoaded ids }, fetchPosts ids)
      Section section -> fetchPage {model | page = 0, section = section}

fetchPage : Model -> (Model, Cmd Msg)
fetchPage model =
  let
    pageSize = 30

    apiCall section =
      case section of
        New -> Api.new
        Ask -> Api.ask
        Jobs -> Api.job
        Show -> Api.show
        Top -> Api.top

    sectionPosts = (apiCall model.section) model.page pageSize
    emptyPage = List.map NotLoaded (List.repeat pageSize 0)

    fetchSection = Task.perform (always FetchFail) FetchIds sectionPosts
    getTime = Task.perform (always FetchFail) Tick Time.now

    newModel =
      { model | posts = emptyPage, time = 0, offset = model.page * pageSize }
  in
    (newModel, Cmd.batch [fetchSection, getTime])


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

