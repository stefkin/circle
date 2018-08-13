module ViewComponents exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List exposing (head, drop)
import ListUtils exposing (indexOf, get)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json

selector : List a -> (a -> msg) -> Html msg
selector xs msg =
    let
      optionTag val =
        option [ value <| toString << xToInt <| val ] [ text (toString val) ]

      intToX i = case get i xs of
                     Just x -> x
                     Nothing -> Debug.crash "Error"

      xToInt x = case indexOf x xs of
                     Just x -> x
                     Nothing -> Debug.crash "Error"
    in
      Html.div [class ""]
          [select [ on "change" (Json.map msg (Json.map intToX targetValueIntParse)) ]
              (List.map optionTag xs)
          ]

padJoin n xs =
    List.map (String.padRight n ' ') xs |> String.join " "

renderPitches : List (a1, a2) -> Html msg
renderPitches pitches =
    let
      pitchToStr = toString << Tuple.first
      pcs = List.map pitchToStr pitches
      renderPc pc =
          Html.span [] [ text (toString pc)]
    in
      Html.div [class "columns"] [text <| padJoin 4 pcs]

renderChords : List String -> Html msg
renderChords chords =
    Html.div [class "columns"] [text <| padJoin 4 chords ]

stylesheet : String -> Html msg
stylesheet href =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      href
            ]
        children = []
    in
        node tag attrs children
