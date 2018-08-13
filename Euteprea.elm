module Euteprea exposing (..)

import ListUtils exposing (..)
import List exposing (..)

type Mode = Major | Minor | Ionian
          | Dorian | Phrygian | Lydian | Mixolydian
          | Aeolian | Locrian | CustomMode String

type PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss

type alias AbsPitch = Int
type alias Octave = Int
type alias Pitch = (PitchClass, Octave)

get : Int -> List a -> Maybe a
get n xs = head (drop n xs)

intToPc : Int -> PitchClass
intToPc i  =
    case i of
      (-2) -> Cff
      (-1) -> Cf
      0 -> C
      1 -> Cs
      -- 2 -> Css
      -- 0 -> Dff
      -- 1 -> Df
      2 -> D
      3 -> Ds
      -- 4 -> Dss
      -- 2 -> Eff
      -- 3 -> Ef
      4 -> E
      -- 5 -> Es
      -- 6 -> Ess
      -- 3 -> Fff
      -- 4 -> Ff
      5 -> F
      6 -> Fs
      -- 7 -> Fss
      -- 5 -> Gff
      -- 6 -> Gf
      7 -> G
      8 -> Gs
      -- 9 -> Gss
      -- 7 -> Aff
      -- 8 -> Af
      9 -> A
      10 -> As
      -- 11 -> Ass
      -- 9 -> Bff
      -- 10 -> Bf
      11 -> B
      12 -> Bs
      -- 13 -> Bss
      _ -> C
pcToInt : PitchClass -> Int
pcToInt pc  =
    case pc of
      Cff  -> -2
      Cf  -> -1
      C  -> 0
      Cs  -> 1
      Css  -> 2
      Dff  -> 0
      Df  -> 1
      D  -> 2
      Ds  -> 3
      Dss  -> 4
      Eff  -> 2
      Ef  -> 3
      E  -> 4
      Es  -> 5
      Ess  -> 6
      Fff  -> 3
      Ff  -> 4
      F  -> 5
      Fs  -> 6
      Fss  -> 7
      Gff  -> 5
      Gf  -> 6
      G  -> 7
      Gs  -> 8
      Gss  -> 9
      Aff  -> 7
      Af  -> 8
      A  -> 9
      As  -> 10
      Ass  -> 11
      Bff  -> 9
      Bf  -> 10
      B  -> 11
      Bs  -> 12
      Bss  -> 13
absPitch : Pitch -> AbsPitch
absPitch (pc,oct)  = 12*(oct+1) + pcToInt pc

pitch : AbsPitch -> Pitch
pitch ap  =
 let
   oct = ap // 12
   n = ap % 12
   maybeP = get n [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]
   p = Maybe.withDefault C maybeP
 in
  (p, oct-1)

trans : Int -> Pitch -> Pitch
trans i p  = pitch (absPitch p + i)

circle_ : Pitch -> Int -> List Pitch
circle_ p n =
  let
    fifth = trans 7
    nextNote = fifth p
    nums = List.range 1 n
  in
    case n of
      0 -> []
      _ ->
        p :: (circle_ nextNote (n - 1))

circle : List Pitch
circle = circle_ (C, 0) 120

genScale : Int -> PitchClass -> List Pitch
genScale pos tonic =
  let
    pitchToPitchClass = Tuple.first
    pcCircle = map pitchToPitchClass circle
    tonicIndex = Maybe.withDefault 0 <| indexOf tonic pcCircle
    firstScalePitchIndex = tonicIndex - pos + 12
    singleScaleWrap = take 7 <| drop firstScalePitchIndex circle
  in
    singleScaleWrap
    -- filter (\ (pc, o) -> member pc singleScaleWrap) circle
modeToTonicPosition : Mode -> Int
modeToTonicPosition mode =
    case mode of
      Lydian     -> 0
      Major      -> 1
      Ionian     -> 1
      Mixolydian -> 2
      Dorian     -> 3
      Minor      -> 4
      Aeolian    -> 4
      Phrygian   -> 5
      Locrian    -> 6
      _          -> 0

scale : Mode -> PitchClass -> List Pitch
scale = genScale << modeToTonicPosition

type ChordType = Maj | Min | Dim
type alias ChordPosition = Int
type alias Chord = (ChordPosition, ChordType)

scaleChords : Mode -> List Chord
scaleChords mode =
    let
        pos = 7 - (modeToTonicPosition mode)
        chordTypes = [Maj, Maj, Maj, Min, Min, Min, Dim]
        baseChordPositions = [1,5,2,6,3,7,4]
        chordPositions = (drop pos baseChordPositions) ++
                         (take pos baseChordPositions)
    in
        zip chordPositions chordTypes

chordToString : Chord -> String
chordToString (pos, ct) =
    let
        modifier = case ct of
                       Maj ->
                           String.toUpper
                       Min ->
                           String.toLower
                       Dim ->
                           (flip (String.append) "º") >> String.toLower
        base = case pos of
                   1 -> "I"
                   2 -> "II"
                   3 -> "III"
                   4 -> "IV"
                   5 -> "V"
                   6 -> "VI"
                   _ -> "VII"
    in
        modifier base
