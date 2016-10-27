module Comments.Model
  exposing
    ( Model
    , CommentTree(..)
    , postToTree
    , init
    , rootTree
    )

import Feed.Model
  exposing
    ( Post
    , Id
    , PostType(..)
    , PostMeta
    , Resource(..)
    , postDecoder
    )

import Time exposing (Time)
import Json.Decode as Decode exposing (Decoder, (:=))

import Array exposing (Array)

type alias Model =
  { root : Resource Id CommentTree
  , time : Time
  , commentCount : Int
  }

type CommentTree =
  CommentTree
  { post : Post
  , children : Array (Resource Id CommentTree)
  }

postToTree : Post -> CommentTree
postToTree post =
  CommentTree { post = post, children = Array.map NotLoaded (Array.fromList post.kids) }

rootTree : Model -> Maybe (Post, Array (Resource Id CommentTree))
rootTree model =
  case model.root of
    Loaded (CommentTree root) -> Just (root.post, root.children)
    _ -> Nothing

init : Model
init =
  let
    initComments post =
        Array.map NotLoaded (Array.fromList post.kids)

    defaultId = 12802121
  in
    { root = NotLoaded defaultId, time = 0.0, commentCount = 0}

