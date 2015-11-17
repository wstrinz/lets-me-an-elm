module Game where
import Html exposing (Html, text, div, h4, br, button)
import Html.Events exposing (onClick)
import Signal exposing (Signal, (~), (<~))

type Page = CounterA | CounterB

type alias Model = { counta: Int, countb: Int, page: Page }

initialModel : Model
initialModel = { counta = 0, countb = 0, page = CounterA}

model : Signal Model
model = Signal.foldp update initialModel actions.signal

view : Signal.Address Action -> Model -> Html.Html
view address model =
  case model.page of
    CounterA -> viewA model address
    CounterB -> viewB model address

viewA : Model -> Signal.Address Action -> Html.Html
viewA model address = div [] [
        countForPage model,
        br [] [],
        button [ onClick address (CountOp CounterA) ] [ text "+" ],
        br [] [],
        button [ onClick address (SwitchPage CounterB) ] [ text ">" ]
      ]

viewB : Model -> Signal.Address Action -> Html.Html
viewB model address = div [] [
        h4 [] [text ((toString model.page) ++ ": ")],
        countForPage model,
        br [] [],
        button [ onClick address (CountOp CounterB) ] [ text "+" ],
        br [] [],
        button [ onClick address (SwitchPage CounterA) ] [ text "<" ]
      ]

countForPage : Model -> Html.Html
countForPage model =
  case model.page of
    CounterA -> h4 [] [text (toString model.counta)]
    CounterB -> h4 [] [text (toString model.countb)]

type Action =
  NoOp
  | CountOp (Page)
  | SwitchPage (Page)

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

update :  Action -> Model -> Model
update act moderu =
  case act of
    NoOp -> moderu
    CountOp (CounterA) -> { moderu | counta <- moderu.counta + 1 }
    CountOp (CounterB) -> { moderu | countb <- moderu.countb + 1 }
    SwitchPage (CounterA) -> { moderu | page <- CounterA }
    SwitchPage (CounterB) -> { moderu | page <- CounterB }

main : Signal Html.Html
main = (view actions.address) <~ model
--x goal 0 - compilable program that displays a thing
--x  goal 1 - basically make a countaer. button that sends signal that affects model
--  goal 2 - hook up model to port, properly receive port data after a wait time
--x  goal 3 - multi-page 'navigation' based on signals and port interaction
--  goal 4 - silly procgenish things for buttons on one page to lead to random text on another
--  andthen - try to make the real game?
