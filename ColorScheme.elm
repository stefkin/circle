module ColorScheme exposing (..)

import Color exposing (Color)

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
