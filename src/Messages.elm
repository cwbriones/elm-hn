module Messages exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

type Msg
  = NavigateTo String
  | SetQuery (Dict String String)
  | Tick Time
