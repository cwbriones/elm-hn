import Html.App as App

import Model
import Update exposing (update, initialize, Msg(Tick))
import View exposing (view)

import Time exposing (Time, second)

main : Program Never
main =
  App.program
    { subscriptions = subscriptions
    , init = initialize Model.init
    , update = update
    , view = view
    }

subscriptions : a -> Sub Msg
subscriptions _ = Time.every second Tick
