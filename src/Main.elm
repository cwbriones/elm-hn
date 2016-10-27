import Navigation

import Routing exposing (urlParser)
import Model exposing (init)
import View exposing (view)
import Update exposing (update, urlUpdate, subscriptions)

main : Program Never
main =
  Navigation.program Routing.urlParser
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    , urlUpdate = urlUpdate
    }
