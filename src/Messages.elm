module Messages exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Feed.Update as F

type Msg
  = NavigateTo String
  | SetQuery (Dict String String)
  | Tick Time
  | FeedMsg F.Msg
