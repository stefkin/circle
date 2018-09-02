module CollageTest exposing (circleCollage)

import ListUtils exposing (zip, find)
import Array
import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Html exposing (Html)


-- Model -----------------------------------------------------------------------


type alias Model =
    { active : Bool }


init : Model
init =
    { active = False }



-- Update ----------------------------------------------------------------------


type Msg
    = Switch


update : Msg -> Model -> Model
update msg model =
    case msg of
        Switch ->
            { model | active = not model.active }



-- View ------------------------------------------------------------------------
-- Styles --


border : LineStyle
border =
    solid verythin <| uniform black



-- Text --


txt : Collage Msg
txt =
    fromString "Hello collage!"
        |> rendered



-- Shapes --


elps : Model -> Collage Msg
elps model =
    ellipse 100 50
        |> styled
            ( uniform <|
                if model.active then
                    lightPurple
                else
                    lightBlue
            , border
            )
        |> rotate (degrees -30)
        |> onClick Switch


rect : Collage msg
rect =
    roundedRectangle 200 250 20
        |> styled ( uniform lightOrange, border )


tria : Collage msg
tria =
    triangle 100
        |> styled ( uniform lightGreen, border )


penta : Collage msg
penta =
    ngon 5 100
        |> styled ( uniform lightCharcoal, border )



-- Alignments --


alignments : Collage msg
alignments =
    horizontal <|
        List.map (showOrigin << align top) [ rect, tria, rect, rect ]


sectorLines : Collage msg -> List (Collage msg)
sectorLines line =
    List.range 1 6 |> List.map toFloat |> List.map (\n -> rotate (degrees <| 15 + 30 * n) line)

sectorText r renderedNotes =
    let

        frags = 360 / 12
        theta = Array.fromList (List.range 0 11 |> List.map toFloat |> List.map (\n -> (frags / 180) * (n - 3) * -3.1415))
        positionsX = List.range 0 11 |> List.map (\n -> r * cos (Maybe.withDefault 0 <| Array.get n theta))
        positionsY = List.range 0 11 |> List.map (\n -> r * sin (Maybe.withDefault 0 <| Array.get n theta))
        positions = zip positionsX positionsY

    in
        zip positions renderedNotes |> List.map (\( pos, note ) -> shift pos note)

-- Main ------------------------------------------------------------------------

circleWrap = ["C", "G", "D", "A", "E", "B", "Fs", "Cs", "Gs", "Ds", "As", "F"]
selectedScale = ["F", "C", "G", "D", "A", "E", "B"]
chords = ["IV", "I", "V", "ii", "vi", "iii", "viiº"]
renderNote (note, isBold) =
    let
        textWeight =
            if isBold then
                Collage.Text.Bold
            else
                Collage.Text.Regular
    in
        ((Collage.Text.fromString note |> Collage.Text.size Collage.Text.huge) |> Collage.Text.weight textWeight |> rendered)

ssscale = zip selectedScale chords

circleWrap_ =
    List.map (\n -> if List.member n selectedScale then (n, True) else (n, False)) circleWrap
        |> List.map renderNote
chords_ =
    let
        zz = zip selectedScale chords
        noteToChord n =
            Tuple.second (Maybe.withDefault ("", "") <| find (\(n_,c) -> n == n_) zz)
    in
      List.map (\n -> if List.member n selectedScale then (noteToChord n, False) else ("", False)) circleWrap
        |> List.map renderNote


chordss = ["I", "V", "ii", "vi", "iii", "viiº", "", "", "", "", "", "IV"]
chordsss = List.map (\n -> List.member) circleWrap

view : Model -> Html Msg
view model =
  [ circle 240 |> outlined (solid thin (uniform black))
  , circle 160 |> outlined (solid thin (uniform black))
  , circle 100 |> outlined (solid thin (uniform black))
  , circle 100 |> filled (uniform white)
  ] ++ sectorLines (line 480 |> traced (solid verythin (uniform black)))
    ++ sectorText 200 circleWrap_
    ++ sectorText 130 chords_
  |> stack
  |> debug
  |> svg

circleCollage circleWrap selectedScale chords =
    let
      circleWrap_ =
          List.map (\n -> if List.member n selectedScale then (n, True) else (n, False)) circleWrap
              |> List.map renderNote
      chords_ =
          let
              zz = zip selectedScale chords
              noteToChord n =
                  Tuple.second (Maybe.withDefault ("", "") <| find (\(n_,c) -> n == n_) zz)
          in
            List.map (\n -> if List.member n selectedScale then (noteToChord n, False) else ("", False)) circleWrap
              |> List.map renderNote
    in
      [ circle 240 |> outlined (solid thin (uniform black))
      , circle 160 |> outlined (solid thin (uniform black))
      , circle 100 |> outlined (solid thin (uniform black))
      , circle 100 |> filled (uniform white)
      ] ++ sectorLines (line 480 |> traced (solid verythin (uniform black)))
        ++ sectorText 200 circleWrap_
        ++ sectorText 130 chords_
      |> stack
      |> debug
      |> svg

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = init, view = view, update = update }
