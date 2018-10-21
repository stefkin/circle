module ViewComponents exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List exposing (head, drop)
import ListUtils exposing (indexOf, get)
--import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import Result

selector : List a -> (a -> msg) -> a -> Html msg
selector xs msg default =
    let
      optionTag val =
        option [ value <| String.fromInt << xToInt <| val ] [ text (Debug.toString val) ]

      intToX : Int -> a
      intToX i = case get i xs of
                     Just x -> x
                     Nothing -> default

      xToInt : a -> Int
      xToInt x = case indexOf x xs of
                     Just y -> y
                     Nothing -> 0

      maybeIntToX : Maybe Int -> a
      maybeIntToX maybeint =
        Maybe.withDefault 0 maybeint |> intToX

      handler = targetValue |> Json.map String.toInt |> Json.map maybeIntToX |> Json.map msg
    in
      Html.div [class ""]
          [select [ on "change" (handler) ]
              (List.map optionTag xs)
          ]

padJoin n xs =
    List.map (String.padRight n ' ') xs |> String.join " "

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
