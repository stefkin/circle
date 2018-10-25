module ViewComponents exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import List exposing (head, drop)
import ListUtils exposing (indexOf, get)
import Json.Decode as Json

selector : List a -> (a -> msg) -> a -> (a -> String) -> Html msg
selector xs msg default toString =
    let
      optionTag val =
        option [ value <| String.fromInt << xToInt <| val ] [ text (toString val) ]
      intToX : Int -> a
      intToX i = get i xs |> Maybe.withDefault default
      xToInt : a -> Int
      xToInt x = indexOf x xs |> Maybe.withDefault 0

      handler = targetValue |> Json.map (String.toInt >> Maybe.withDefault 0 >> intToX >> msg)
    in
      Html.div [class ""]
          [select [ on "change" (handler) ] (List.map optionTag xs)
          ]

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
