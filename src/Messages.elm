module Messages exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Feed.Update as F
import Comments.Update as C

type Msg
  = NavigateTo String
  | SetQuery (Dict String String)
  | Tick Time
  | FeedMsg F.Msg
  | CommentsMsg C.Msg
  | Noop

noop : a -> Msg
noop = always Noop
