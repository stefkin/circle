module VisualizationTest exposing (circleVisualization)

import ColorScheme exposing (CellColor, majorCellColor, minorCellColor,
                             diminishedCellColor, blankChordColor, regularNoteColor,
                             selectedChordColor)

import ListUtils exposing (zip, find, indexOf)
import Array exposing (Array)
import Path
import Shape exposing (defaultPieConfig, Arc)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (fontWeight, fontSize, dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (FontWeight(..), Length(..), AnchorAlignment(..), Fill(..), Transform(..), em)

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

buildArcConfig : {outerRadius: Float, innerRadius: Float} -> Int -> Arc
buildArcConfig baseConfig index =
  { defaultArcConfig |
    outerRadius = baseConfig.outerRadius,
    innerRadius = baseConfig.innerRadius,
    startAngle = toRad (toFloat index) * 30,
    endAngle = toRad ((toFloat index) + 1) * 30
  }

radius : Float
radius =
    min w h / 2

makeLabel arcBuilder index (label, color) =
    let
        arc = arcBuilder index
        ( x, y ) =
            Shape.centroid { arc |
              innerRadius = (arc.outerRadius + arc.innerRadius) / 2,
              outerRadius = (arc.outerRadius + arc.innerRadius) / 2
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
makeDonutSector arcBuilder index (label, color) =
    Path.element (Shape.arc <| arcBuilder index)
        [ fill <| Fill color.background, stroke blankChordColor.background ]

circleVisualization : List String -> List String -> List String -> Svg msg
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
              _      -> (n, regularNoteColor)
              ) circleWrap

      chords_ =
          let
              zz = zip selectedScale chords
              noteToChord n =
                  Tuple.second (Maybe.withDefault ("", "") <| find (\(n_,c) -> n == n_) zz)
          in
            List.map (\n -> if List.member n selectedScale then (noteToChord n, selectedChordColor) else ("", blankChordColor)) circleWrap

      outerDonutSettings = buildArcConfig { outerRadius = radius, innerRadius = radius - 60 }
      innerDonutSettings = buildArcConfig { outerRadius = radius - 60, innerRadius = radius - 100 }
    in
        svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (w / 2) (h / 2) ] ]
            [ g [] <| List.indexedMap (makeDonutSector outerDonutSettings) circleWrap_
            , g [] <| List.indexedMap (makeLabel outerDonutSettings) circleWrap_
            , g [] <| List.indexedMap (makeDonutSector innerDonutSettings) chords_
            , g [] <| List.indexedMap (makeLabel innerDonutSettings) chords_
            ]
        ]
