module ListUtils exposing (indexOf, get)
import List exposing (..)

indexOf : a -> List a -> Maybe Int
indexOf el list =
  let
    indexOf_ list_ index =
      case list_ of
        [] ->
          Nothing
        (x::xs) ->
          if x == el then
            Just index
          else
            indexOf_ xs (index + 1)
  in
    indexOf_ list 0

get : Int -> List a -> Maybe a
get n xs = head <| drop n xs
