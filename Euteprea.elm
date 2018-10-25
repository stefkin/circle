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
   n = modBy 12 ap
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
                         \x -> String.append x "º" |> String.toLower
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

pcToString : PitchClass -> String
pcToString pc =
    case pc of
      Cff  -> "Cbb"
      Cf  -> "Cb"
      C  -> "C"
      Cs  -> "C#"
      Css  -> "C##"
      Dff  -> "Dbb"
      Df  -> "Db"
      D  -> "D"
      Ds  -> "D#"
      Dss  -> "D##"
      Eff  -> "Ebb"
      Ef  -> "Eb"
      E  -> "E"
      Es  -> "E#"
      Ess  -> "E##"
      Fff  -> "Fbb"
      Ff  -> "Fb"
      F  -> "F"
      Fs  -> "F#"
      Fss  -> "F##"
      Gff  -> "Gbb"
      Gf  -> "Gb"
      G  -> "G"
      Gs  -> "G#"
      Gss  -> "G##"
      Aff  -> "Abb"
      Af  -> "Ab"
      A  -> "A"
      As  -> "A#"
      Ass  -> "A##"
      Bff  -> "Bbb"
      Bf  -> "Bb"
      B  -> "B"
      Bs  -> "B#"
      Bss  -> "B##"

pitchToString : Pitch -> String
pitchToString (pc, _) = pcToString pc

modeToString : Mode -> String
modeToString mode =
    case mode of
        Major -> "Major"
        Minor -> "Minor"
        Ionian -> "Ionian"
        Dorian -> "Dorian"
        Phrygian -> "Phrygian"
        Lydian -> "Lydian"
        Mixolydian -> "Mixolydian"
        Aeolian -> "Aeolian"
        Locrian -> "Locrian"
        CustomMode a -> a
