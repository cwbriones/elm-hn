module Cache exposing (getPostsOrFetch, getPostOrFetch)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)

import PersistentCache as Cache

import Debug exposing (log)
import Api exposing (Post, item, decodePost, encodePost)

getPostsOrFetch : List Int -> Task Http.Error (List Post)
getPostsOrFetch ids =
  List.map getPostOrFetch ids |> Task.sequence

getPostOrFetch : Int -> Task Http.Error Post
getPostOrFetch id =
  let
    fetchIfNothing mpost =
      case mpost of
        Just post -> Task.succeed post
        Nothing -> Api.item id `Task.andThen` putAndReturn
  in
    (getPost id) `Task.andThen` fetchIfNothing

postsCache : Cache.Cache Post
postsCache =
  Cache.cache
    { name = "posts"
    , version = 1
    , kilobytes = 1024
    , decode = decodePost
    , encode = encodePost
    }

getPost : Int -> Task x (Maybe Post)
getPost id =
  Cache.get postsCache (toString id)

putAndReturn : Post -> Task x Post
putAndReturn post =
  putPost post `Task.andThen` (always (Task.succeed post))

putPost : Post -> Task x ()
putPost post =
  Cache.add postsCache (toString post.id) post

