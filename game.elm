module Game where
import Html exposing (Html, text, div, h4, br, button)
import Html.Events exposing (onClick)
import Signal exposing (Signal)
import LocalStorage
import Task exposing (Task, andThen)
import Json.Encode exposing (Value, encode, object, string, int)
import Json.Decode as Decode exposing (Decoder, (:=))

type Page = CounterA | CounterB
type alias Model = { counta: Int, countb: Int, page: Page }
type alias StringyModel = { counta: Int, countb: Int, page: String }
type alias ActSig = Signal.Address Action

main : Signal Html.Html
main = Signal.map2 (view actions.address) hello.signal model

initialModel : Model
initialModel = { counta = 0, countb = 0, page = CounterA}

jModel : Model -> Value
jModel model = object [
    ("counta", int model.counta),
    ("countb", int model.countb),
    ("page", string (toString model.page))
  ]

modelToJson : Model -> String
modelToJson model = encode 0 (jModel model)

modelFromJson : String -> Model
modelFromJson json =
  let decodeResult = Decode.decodeString modelDecoder json
  in
    case decodeResult of
      Result.Ok mod -> modelFromStringy mod
      Result.Err errStr -> initialModel

modelDecoder : Decoder StringyModel
modelDecoder =
  Decode.object3 StringyModel
    ("counta" := Decode.int)
    ("countb" := Decode.int)
    ("page" := Decode.string)

-- pageDecoder : Decoder Page
-- pageDecoder = Decode.object1 initialModel.page

modelFromStringy : StringyModel -> Model
modelFromStringy stm =
  let a = stm.counta
      b = stm.countb
  in case stm.page of
    "CounterB" -> {counta = a, countb = b, page = CounterB}
    "CounterA" -> {counta = b, countb = a, page = CounterA}
    _ -> {counta = b, countb = a, page = CounterA}

model : Signal Model
model = Signal.foldp update initialModel actions.signal

anBr : Html.Html
anBr = br [] []

view : ActSig -> String -> Model -> Html.Html
view address helloStr model = div [] [
                       div [] [text helloStr], anBr,
                       pageHeader model, anBr,
                       countForPage model, anBr,
                       pageActions address model, anBr,
                       switchPage address model, anBr
                     ]

pageHeader : Model -> Html.Html
pageHeader model = h4 [] [text (toString model.page)]

pageActions : ActSig -> Model -> Html.Html
pageActions address model =
   button [ onClick address (CountOp model.page) ] [ text "+" ]

switchPage : ActSig -> Model -> Html.Html
switchPage address model =
   button [ onClick address (SwitchPage (nextPage model)) ] [ nextButton model ]

nextPage : Model -> Page
nextPage model =
  if model.page == CounterB then CounterA else CounterB

nextButton : Model -> Html.Html
nextButton model =
  let str =
    case model.page of
      CounterB -> "<"
      CounterA -> ">"
  in text str

countForPage : Model -> Html.Html
countForPage model =
  let count =
    case model.page of
      CounterA -> model.counta
      CounterB -> model.countb
  in h4 [] [text (toString count)]

type Action =
  NoOp
  | CountOp (Page)
  | SwitchPage (Page)

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    CountOp (counter) -> countUp counter model
    SwitchPage newPage -> { model | page = newPage }

countUp : Page -> Model -> Model
countUp counter model =
  case counter of
    CounterA -> { model | counta = model.counta + 1 }
    CounterB -> { model | countb = model.countb + 1 }

hello : Signal.Mailbox String
hello = Signal.mailbox "waiting..."

sendIt : Maybe String -> Task LocalStorage.Error ()
sendIt str =
  case str of
    Just val -> Signal.send hello.address val
    Nothing -> Signal.send hello.address "nope"

setHello : String -> Task LocalStorage.Error String
setHello count = LocalStorage.set "some-key" (toString count)

port runHello : Task LocalStorage.Error ()
port runHello = (LocalStorage.get "some-key") `andThen` sendIt --Signal.send hello.address

-- port setIt : Task LocalStorage.Error String
-- port setIt = setHello (modelToJson initialModel)

--x goal 0 - compilable program that displays a thing
--x goal 1 - basically make a countaer. button that sends signal that affects model
--x goal 2 - multi-page 'navigation' based on signals and port interaction
--  goal 3 - persist to localstorage using elm
--  gaol 3.01 - use json decode/encode to persist model to localstorage
--  goal 3.1 - Talk to server w/ http
--  goal 3.2 - Give server seed (maybe w/ native geoloc), get procgen response
--  goal 4 - silly procgenish things for buttons on one page to lead to random text on another
--  andthen - try to make the real game?

-- Maybe do
--  goal 3 - hook up model to port, properly receive port data after a wait time
