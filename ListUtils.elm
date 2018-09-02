module ListUtils exposing (indexOf, get, zip, find)
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

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    (x :: xs_, y :: ys_) -> (x,y) :: zip xs_ ys_
    _ -> []

find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest
