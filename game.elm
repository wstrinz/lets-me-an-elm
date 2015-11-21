module Game where
import Html exposing (Html, text, div, h4, br, button)
import Html.Events exposing (onClick)
import Signal exposing (Signal)
import LocalStorage
import Task exposing (Task, andThen)
import Json.Encode exposing (Value, encode, object, string, int)
import Json.Decode as Decode exposing (Decoder, (:=))

type Page = CounterA | CounterB
type State = Initializing | Loaded
type alias Model = { counta: Int, countb: Int, page: Page, state: State }
type alias StringyModel = { counta: Int, countb: Int, page: String, state: String }
type alias ActSig = Signal.Address Action

main : Signal Html.Html
main = Signal.map (view actions.address) model

initialModel : Model
initialModel = { counta = 0, countb = 0, page = CounterA, state = Initializing}

jModel : Model -> Value
jModel model = object [
    ("counta", int model.counta),
    ("countb", int model.countb),
    ("page", string (toString model.page)),
    ("state", string (toString model.state))
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
  Decode.object4 StringyModel
    ("counta" := Decode.int)
    ("countb" := Decode.int)
    ("page" := Decode.string)
    ("state" := Decode.string)

stateFor : String -> State
stateFor str =
  case str of
    "Loaded" -> Loaded
    _ -> Initializing

modelFromStringy : StringyModel -> Model
modelFromStringy stm =
  let a = stm.counta
      b = stm.countb
      s = stateFor stm.state
  in case stm.page of
    "CounterB" -> {counta = a, countb = b, page = CounterB, state = s}
    "CounterA" -> {counta = a, countb = b, page = CounterA, state = s}
    _ -> {counta = b, countb = a, page = CounterA, state = s}

model : Signal Model
model = Signal.foldp update initialModel actions.signal

anBr : Html.Html
anBr = br [] []

view : ActSig -> Model -> Html.Html
view address model = div [] [
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
  | SetModel (Model)

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    SetModel newModel -> { model |
                            page = newModel.page,
                            counta = newModel.counta,
                            countb = newModel.countb,
                            state = Loaded}
    _ ->
      case model.state of
        Initializing -> model
        Loaded ->
          case action of
            NoOp -> model
            CountOp (counter) -> countUp counter model
            SwitchPage newPage -> { model | page = newPage }
            SetModel newModel -> { model |
                                    page = newModel.page,
                                    counta = newModel.counta,
                                    countb = newModel.countb,
                                    state = newModel.state }

countUp : Page -> Model -> Model
countUp counter model =
  case counter of
    CounterA -> { model | counta = model.counta + 1 }
    CounterB -> { model | countb = model.countb + 1 }

-- sendIt : Maybe String -> Task LocalStorage.Error ()
-- sendIt str =
--   case str of
--     Just val -> Signal.send actions.address (SetModel (modelFromJson val))
--     Nothing -> Signal.send actions.address (SetModel initialModel)

-- possiblySetModel -> Task LocalStorage.Error -> Maybe (Signal Action)

-- port loadModel : Task LocalStorage.Error (Maybe String)
-- port loadModel = LocalStorage.get "model-key" `andThen` possiblySetModel --Signal.send hello.address

saveModel : Model -> Task LocalStorage.Error String
saveModel model = LocalStorage.set "model-key" (modelToJson model)

watchModel : Model -> Task LocalStorage.Error String
watchModel model =
  case model.state of
    Initializing -> LocalStorage.set "loading" "isLoading"
    Loaded -> saveModel model

port modelWatcher : Signal (Task LocalStorage.Error String)
port modelWatcher = Signal.map watchModel model

-- setupModel : Maybe String -> Task LocalStorage.Error Action
-- setupModel modString =
--   let loadedModel =
--     case modString of
--       Just str -> modelFromJson str
--       Nothing -> initialModel
--   in Signal.send actions.address (SetModel (loadedModel))
--
-- saveStorageStateIGuess lastResult = LocalStorage.set "loading" "done!"

port modelLoader : Task LocalStorage.Error ()
port modelLoader =
  let handle str =
    case str of
      Just s -> Signal.send actions.address (SetModel (modelFromJson s))
      Nothing -> Signal.send actions.address (SetModel (initialModel))
  in (LocalStorage.get "model-key") `andThen` handle
-- modelBox : Signal.Mailbox Model
-- modelBox = Signal.mailbox initialModel

-- setHello : String -> Task LocalStorage.Error String
-- setHello count = LocalStorage.set "model-key" (toString count)


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
