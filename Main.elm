module Main exposing (main)
import Euteprea exposing (circle, scale, scaleChords, chordToString, Mode(..), PitchClass(..))
import VisualizationTest exposing (circleVisualization)
import Html exposing (..)
import Browser
import Html.Attributes exposing (..)
import ViewComponents exposing (stylesheet, selector)

type alias Model =
    { tonic : PitchClass
    , mode : Mode
    }

initialModel : Model
initialModel = Model C Major

main : Program () Model Msg
main =
  Browser.sandbox { init = initialModel, view = view, update = update }

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
        modes = [Major, Minor, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian]
        pcs = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]
        selectedScale = scale model.mode model.tonic |> List.map Tuple.first |> List.map Debug.toString
        circleWrap = List.take 12 circle |> List.map Tuple.first |> List.map Debug.toString
        chords = scaleChords model.mode |> List.map chordToString
    in
        Html.section [style "background-color" "rgb(6,7,19)", class "section"]
            [Html.div [ class "container"]
                 [ Html.div [class "columns"]
                       [ Html.div [class "column is-6"]
                             [ selector pcs ChangeTonic C
                             , selector modes ChangeMode Major
                             ]
                       , Html.div [class "column is-6"]
                           [circleVisualization circleWrap selectedScale chords]
                       ]
                 , stylesheet "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css"
                 ]
            ]
