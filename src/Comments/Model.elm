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

import Array exposing (Array)

type alias Model =
  { root : Resource Id CommentTree
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

init : Id -> Model
init id =
  let
    defaultId = 12802121
  in
    { root = NotLoaded id, commentCount = 0 }

