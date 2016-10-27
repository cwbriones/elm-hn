module Comments.Update
  exposing
    ( update
    , initialize
    , fetchRoot
    , Msg(..)
    )

import Task exposing (Task)
import Time exposing (Time)
import Array exposing (Array)

import Api
import Feed.Model
  exposing (Post, Id, Resource(..))
import Comments.Model exposing (Model, CommentTree(..), rootTree, postToTree)

type Msg =
  Tick Time
  | TaskFail
  | UpdateRoot CommentTree
  | FetchRootSucceed Post
  | FetchSucceed (List Id) Post

perform : (a -> Msg) -> Task x a -> Cmd Msg
perform = Task.perform (always TaskFail)

initialize : Model -> (Model, Cmd Msg)
initialize model =
  let
    getTime = perform Tick Time.now
  in
    case model.root of
      NotLoaded id ->
        model ! [fetchRoot id, getTime]
      Loaded (CommentTree tree) ->
        model ! [fetchComments [] tree.post, getTime]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    noCmd a = (a, Cmd.none)
  in
    case msg of
      Tick time -> noCmd { model | time = time }
      TaskFail -> noCmd model
      UpdateRoot newRoot ->
        {model|root = Loaded newRoot} ! []
      FetchRootSucceed post ->
        {model|root = Loaded (postToTree post)} ! [ fetchComments [] post ]
      FetchSucceed path post ->
        (insertRoot (List.reverse path) post model) ! [ fetchComments (post.id::path) post ]

fetchRoot : Id -> Cmd Msg
fetchRoot id = Api.item id |> perform FetchRootSucceed

fetchComments : List Id -> Post -> Cmd Msg
fetchComments path post =
  List.map Api.item post.kids
  |> List.map (perform (FetchSucceed path))
  |> List.reverse
  |> Cmd.batch

insertRoot : List Id -> Post -> Model -> Model
insertRoot path post model =
  case model.root of
    Loaded root ->
      {model|root = Loaded (insert path post root), commentCount = model.commentCount + 1}
    _ ->
      model

insert : List Id -> Post -> CommentTree -> CommentTree
insert path post (CommentTree tree) =
  let
    extractId res =
      case res of
        NotLoaded id -> id
        Loaded (CommentTree tree) -> tree.post.id
  in
    case path of
      (id::ids) ->
        case find ((\a -> id == a) << extractId ) (tree.children) of
          Just (idx, Loaded subtree) ->
            CommentTree {tree|children = Array.set idx (Loaded (insert ids post subtree)) (tree.children)}
          _ ->
            CommentTree tree
      [] ->
        case find ((\a -> post.id == a) << extractId) (tree.children) of
          Just (idx, _) ->
            CommentTree {tree|children = Array.set idx (Loaded (postToTree post)) (tree.children)}
          _ ->
            CommentTree tree

find : (a -> Bool) -> Array a -> Maybe (Int, a)
find pred arr =
  let
    find' pred i arr =
      case uncons arr of
        Just (hd, tl) ->
          if pred hd then Just (i, hd)
                     else find' pred (i + 1) tl
        Nothing -> Nothing
  in
    find' pred 0 arr

uncons : Array a -> Maybe (a, Array a)
uncons arr =
  let
    hd = Array.get 0
    tl a = Array.slice 1 (Array.length a) a
  in
    Maybe.map (\a -> (a, tl arr)) (hd arr)
