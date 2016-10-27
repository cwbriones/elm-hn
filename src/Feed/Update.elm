module Feed.Update
  exposing
    ( update
    , initialize
    , Msg
    )

import Task

import Api
import Feed.Model as Feed exposing (Feed, Section(..), Id, Post, Resource(..))

type Msg =
  Paginate Int
  | FetchFail
  | FetchPost Post
  | FetchIds (List Id)
  | Section Section

initialize : Section -> (Feed, Cmd Msg)
initialize section = fetchPage (Feed.init)

update : Msg -> Feed -> (Feed, Cmd Msg)
update msg feed =
  let
    noCmd a = (a, Cmd.none)
    fetchPost id = Api.item id |> Task.perform (always FetchFail) FetchPost
    fetchPosts ids = List.map fetchPost ids |> List.reverse |> Cmd.batch
  in
    case msg of
      Paginate page -> fetchPage {feed | page = page}
      FetchFail -> noCmd feed
      FetchPost post -> noCmd { feed | posts = insertPost post feed.posts }
      FetchIds ids -> ({ feed | posts = List.map NotLoaded ids }, fetchPosts ids)
      Section section -> fetchPage {feed | page = 0, section = section}

fetchPage : Feed -> (Feed, Cmd Msg)
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

    newModel =
      { model | posts = emptyPage, offset = model.page * pageSize }
  in
    (newModel, fetchSection)

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
