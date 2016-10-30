module Messages exposing (..)

import Time exposing (Time)

import Feed.Update as F
import Comments.Update as C

type Msg
  = NavigateTo String
  | Tick Time
  | FeedMsg F.Msg
  | CommentsMsg C.Msg
  | Noop

noop : a -> Msg
noop = always Noop
