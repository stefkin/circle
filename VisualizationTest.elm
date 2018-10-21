module VisualizationTest exposing (circleVisualization)

import ListUtils exposing (zip, find, indexOf)
import Array exposing (Array)
import Color exposing (Color)
import Path
import Shape exposing (defaultPieConfig, Arc)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (fontWeight, fontSize, dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (FontWeight(..), Length(..), AnchorAlignment(..), Fill(..), Transform(..), em)

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
    , cornerRadius = 0
    , startAngle = 0
    , endAngle = 360 / 12
    , padAngle = 0
    , padRadius = 0
    }

toRad deg = deg * pi / 180

nthArcConfig : {outerRadius: Float, innerRadius: Float} -> Int -> Arc
nthArcConfig baseConfig index =
  { defaultArcConfig |
    outerRadius = baseConfig.outerRadius,
    innerRadius = baseConfig.innerRadius,
    startAngle = toRad (toFloat index) * 30,
    endAngle = toRad ((toFloat index) + 1) * 30
  }

radius : Float
radius =
    min w h / 2

makeLabel configFn index (label, color) =
    let
        slice = configFn index
        ( x, y ) =
            Shape.centroid { slice |
              innerRadius = (slice.outerRadius + slice.innerRadius) / 2,
              outerRadius = (slice.outerRadius + slice.innerRadius) / 2
            }
    in
        text_
            [ transform [ Translate x y ]
            , dy (em 0.35)
            , fontSize <| Px 25
            , fontWeight FontWeightBold
            , textAnchor AnchorMiddle
            , fill <| Fill color.text
            ]
            [ text label ]

makeDonutSector : (Int -> Arc) -> Int -> (a, CellColor) -> Svg msg
makeDonutSector configFn index (label, color) =
    let
        slice = configFn index
    in
        Path.element (Shape.arc <| slice) [ fill <| Fill color.background, stroke darkerblue ]

type alias CellColor = { text : Color, background : Color }

darkblue = Color.rgb255 31 31 44
darkerblue = Color.rgb255 6 7 19
darkergrey = Color.rgb255 16 16 27
darkgrey = Color.rgb255 62 64 67
lightblue = Color.rgb255 25 126 255
darkred = Color.rgb255 66 32 39

majorCellColor =
  { text = Color.white
  , background = lightblue
  }

minorCellColor =
  { text = Color.white
  , background = darkred
  }

diminishedCellColor =
  { text = Color.white
  , background = darkred
  }


blankChordColor =
  { text = darkerblue
  , background = darkerblue
  }

regularNoteColor =
  { text = darkerblue
  , background = darkblue
  }

selectedChordColor =
  { background = darkergrey
  , text = darkgrey
  }

circleVisualization circleWrap selectedScale chords =
    let
      circleWrap_ =
          List.map (\n ->
            case indexOf n selectedScale of
              Just 0 -> (n, majorCellColor)
              Just 1 -> (n, majorCellColor)
              Just 2 -> (n, majorCellColor)
              Just 3 -> (n, minorCellColor)
              Just 4 -> (n, minorCellColor)
              Just 5 -> (n, minorCellColor)
              Just 6 -> (n, diminishedCellColor)
              _ -> (n, regularNoteColor)
              ) circleWrap
      chords_ =
          let
              zz = zip selectedScale chords
              noteToChord n =
                  Tuple.second (Maybe.withDefault ("", "") <| find (\(n_,c) -> n == n_) zz)
          in
            List.map (\n -> if List.member n selectedScale then (noteToChord n, selectedChordColor) else ("", blankChordColor)) circleWrap
      outerDonutSettings = nthArcConfig { outerRadius = radius, innerRadius = radius - 60 }
      innerDonutSettings = nthArcConfig { outerRadius = radius - 60, innerRadius = radius - 100 }
    in
        svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap (makeDonutSector outerDonutSettings) circleWrap_
            , g [] <| List.indexedMap (makeLabel outerDonutSettings) circleWrap_
            , g [] <| List.indexedMap (makeDonutSector innerDonutSettings) chords_
            , g [] <| List.indexedMap (makeLabel innerDonutSettings) chords_
            ]
        ]
