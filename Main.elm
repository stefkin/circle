module Main exposing (main)
import Euteprea exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import ViewComponents exposing (stylesheet, flatCircle, selector)

type alias Model =
    { tonic : PitchClass
    , mode : Mode
    }

initialModel : Model
initialModel = Model C Major

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }

type Msg = ChangeTonic PitchClass | ChangeMode Mode

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeTonic val ->
      { model | tonic = val }
    ChangeMode val ->
      { model | mode = val }

view : Model -> Html Msg
view model =
    let
        modes = [Major, Minor, Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian]
        pcs = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]
        selectedScale = scale model.mode model.tonic
    in
        Html.section [class "section"]
            [Html.div [class "container"]
                 [ Html.div [class "level"]
                       [ Html.div [class "level-left"]
                             [ selector pcs ChangeTonic
                             , selector modes ChangeMode
                             ]
                       , Html.div [class "level-right"]
                           [ flatCircle selectedScale
                           ]
                       ]
                 , stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
                 ]
            ]
