module VisualizationTest exposing (circleVisualization)

import ListUtils exposing (zip, find)
import Array exposing (Array)
import Color exposing (Color)
import Path
import Shape exposing (defaultPieConfig, Arc)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em)

colors : Array Color
colors =
    Array.fromList
        [ Color.rgb255 152 171 198
        , Color.rgb255 138 137 166
        , Color.rgb255 123 104 136
        , Color.rgb255 107 72 107
        , Color.rgb255 159 92 85
        , Color.rgb255 208 116 60
        , Color.rgb255 255 96 0
        ]

w = 500
h = 500

defaultArcConfig : Arc
defaultArcConfig =
    { innerRadius = radius - 100
    , outerRadius = radius
    , cornerRadius = 10
    , startAngle = 0
    , endAngle = 360 / 12
    , padAngle = 0
    , padRadius = 0
    }

toRad deg = deg * pi / 180

nthArcConfig : Float -> Int -> Arc
nthArcConfig radDelta index =
  { defaultArcConfig |
    startAngle = toRad (toFloat index) * 30,
    endAngle = toRad ((toFloat index) + 1) * 30,
    innerRadius = radius + radDelta - 60,
    outerRadius = radius + radDelta
  }

radius : Float
radius =
    min w h / 2

makeLabel radDelta index (label, isSelected) =
    let
        slice = nthArcConfig radDelta index
        ( x, y ) =
            Shape.centroid { slice |
              innerRadius = slice.outerRadius - 30,
              outerRadius = slice.outerRadius - 30
            }
        fillColor = Fill <| if isSelected then Color.white else Color.black
    in
        text_
            [ transform [ Translate x y ]
            , dy (em 0.35)
            , textAnchor AnchorMiddle
            , fill fillColor
            ]
            [ text label ]

makeDonutSector : Float -> Int -> (a, Bool) -> Svg msg
makeDonutSector radDelta index (label, isSelected) =
    let
        slice = nthArcConfig radDelta index
        fillColor = Fill <| if isSelected then Color.blue else Color.grey
    in
        Path.element (Shape.arc <| slice) [ fill fillColor , stroke Color.white ]

circleVisualization circleWrap selectedScale chords =
    let
      circleWrap_ =
          List.map (\n -> if List.member n selectedScale then (n, True) else (n, False)) circleWrap
      chords_ =
          let
              zz = zip selectedScale chords
              noteToChord n =
                  Tuple.second (Maybe.withDefault ("", "") <| find (\(n_,c) -> n == n_) zz)
          in
            List.map (\n -> if List.member n selectedScale then (noteToChord n, False) else ("", False)) circleWrap
    in
        svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap (makeDonutSector 0.0) circleWrap_
            , g [] <| List.indexedMap (makeLabel 0.0) circleWrap_
            , g [] <| List.indexedMap (makeDonutSector (-60.0)) chords_
            , g [] <| List.indexedMap (makeLabel (-60.0)) chords_
            ]
        ]
