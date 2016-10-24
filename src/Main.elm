import Model
import Update exposing (update, initialize, Msg(Tick))
import View exposing (view)
import Time exposing (Time, second)

import Navigation

import Nav exposing (urlUpdate)
import Debug exposing (log)

main : Program Never
main =
  Navigation.program (Navigation.makeParser Nav.parser)
    { subscriptions = subscriptions
    , init = init
    , update = update
    , urlUpdate = urlUpdate
    , view = view
    }

init : Result String a -> (Model.Model, Cmd Msg)
init data =
  (case (log "init data" data) of _ -> initialize Model.init)

subscriptions : a -> Sub Msg
subscriptions _ = Time.every second Tick
