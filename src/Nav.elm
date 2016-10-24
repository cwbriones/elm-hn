module Nav exposing (..)

import Navigation
import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string, custom)

import Model exposing (Model)
import Feed.Model exposing (Section, sectionToString, stringToSection)

import Debug exposing (log)

type DesiredPage = Feed Section

parser : Navigation.Location -> Result String DesiredPage
parser location =
  parse identity desiredPage (log "parsing" (location.pathname))

desiredPage : Parser (DesiredPage -> a) a
desiredPage =
  oneOf
    [ format Feed (s "" </> (custom "SECTION" section))
    ]

section : String -> Result String Section
section string =
  case stringToSection string of
    Just s -> Ok s
    Nothing -> Err string

urlUpdate : Result String DesiredPage -> Model -> (Model, Cmd a)
urlUpdate data model =
  case data of
    Ok page -> goTo (log "new page" page) model
    Err _ -> (model, Cmd.none)

pageToUrl : DesiredPage -> String
pageToUrl page =
  case page of
    Feed section -> "/" ++ (sectionToString section)

goTo : DesiredPage -> Model -> (Model, Cmd a)
goTo page model =
  case page of
    Feed section -> model ! []
