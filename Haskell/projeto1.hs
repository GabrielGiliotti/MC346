-- Integrantes:
-- José Philipe Mendes De Godoy  RA: 200073
-- Gabriel Volpato Giliotti      RA: 197569

{--
--------
I must not fear monads.
Fear monads is the mind-killer.
Fear monads is the little-death that brings total obliteration.
I will face my fear of monads.
I will permit it to pass over me and through me.
And when it has gone past, I will turn the inner eye to see its path.
Where the fear of monads has gone there will be Maybe (Just/Nothing). Only (Maybe I) will remain.

https://tee4names.com/god-made-the-strongest-haskell-5865?s=hanes-5250&c=Black&p=FRONT&pr=10foryou
-------

Referencias:
* https://newbedev.com/haskell-how-to-properly-display-a-percentage-up-to-two-decimals
* https://stackoverflow.com/questions/24713584/haskell-implement-empty-insert-member-of-data-set-as-data-map
* https://hackage.haskell.org/package/containers-0.6.5.1/docs/src/Data.Set.Internal.html#insert
* https://www.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/
* https://juristr.com/blog/2008/11/haskell-type-conversions-converting/
* https://stackoverflow.com/questions/42798257/add-a-element-at-the-end-of-list-in-haskell/42798479
* https://programming-idioms.org/idiom/22/convert-string-to-integer/802/haskell
* https://stackoverflow.com/questions/7376937/fastest-way-to-get-the-last-element-of-a-list-in-haskell#:~:text=You%20can%20use%20the%20last,%2C%20you%20could%20use%20(init%20.
* http://zvon.org/other/haskell/Outputmaybe/isJust_f.html
* https://wiki.haskell.org/How_to_work_on_lists
* https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
* https://stackoverflow.com/questions/37656419/how-i-can-split-n-in-haskell/37656663
* https://www.youtube.com/watch?v=RvWnYrQ_WSk&ab_channel=MichaelGilliland
* https://www.youtube.com/watch?v=4gJ9zsZLv0Y&ab_channel=MichaelGilliland
* https://www.youtube.com/watch?v=8nQzw05Ob-8&ab_channel=MichaelGilliland
* https://www.youtube.com/watch?v=3TZkrhhWZHw&ab_channel=MichaelGilliland
-}

module Graphs where

import Data.Map.Strict as M
import Data.Set as S
import Data.Maybe as Ma
import Text.Printf as T


type Distances a = M.Map (Vertex a) Float
type Prevs a = M.Map (Vertex a) (Vertex a)

newtype Vertex a = Keyed a
  deriving (Ord, Eq, Show)

class Graph g where
  vertices :: Ord a => g a -> S.Set (Vertex a)
  neighbors :: Ord a => g a -> Vertex a -> S.Set (Float, Vertex a)

newtype MapGraph a = Weighted { toMap :: M.Map (Vertex a) (S.Set ( Float, Vertex a )) }

instance Graph MapGraph where
  vertices (Weighted m) =
    S.union (M.keysSet m) $ (S.unions) $ Prelude.map (S.map snd) $ Prelude.map snd $ M.toList m

  neighbors (Weighted m) v =
    M.findWithDefault S.empty v m


dj :: (Graph g, Ord a) => g a -> Vertex a -> (Distances a, Prevs a)
dj graph source =
  let
    q = vertices graph
    dist = M.singleton source 0
  in
    loopf q dist M.empty

  where
    loopf q dist prev =
      if S.null q
      then (dist, prev)
      else
        let
          u' = Prelude.foldr (minIn dist) Nothing q
        in
          case u' of
            Nothing -> (dist, prev)
            Just u ->
              let
                q' = S.delete u q
                ns = neighbors graph u
                (dist', prev') = Prelude.foldr (insertV u) (dist, prev) ns
              in
                loopf q' dist' prev'

    minIn _ v Nothing = Just v
    minIn dist v1 (Just v2) =
      case (M.lookup v1 dist, M.lookup v2 dist) of
        (Just d1, Just d2) -> Just (if d1 < d2 then v1 else v2)
        (Nothing, Nothing) -> Nothing
        (Nothing, _) -> Just v2
        (_, Nothing) -> Just v1

    insertV u (length, v) (dist, prev) =
      case (M.lookup u dist, M.lookup v dist) of
        (Just uD, Just vD) ->
          let
            alt = uD + length
          in
            if alt < vD
            then (M.insert v alt dist, M.insert v u prev)
            else (dist, prev)
        (Just uD, _) ->
          let
            alt = uD + length
          in
            (M.insert v alt dist, M.insert v u prev)
        _ -> (dist, prev)


parse_line graph line = do
  let w = words line
  if (length w) == 3
    -- Is a 'graph' type input
    then
      if (Ma.isJust( Prelude.lookup (Keyed (w !! 0)) graph ))
      then
        Prelude.foldl
          (\ acc node -> if (fst node == Keyed (w !! 0))
                            then
                              acc ++ [(fst node, S.insert ((read (w !! 2))::Float, Keyed (w !! 1)) (snd node))]
                            else acc ++ [node]) [] graph
      else graph ++ [(Keyed (w !! 0), S.fromList [((read (w !! 2))::Float, Keyed (w !! 1))])]
    else graph

parse_input_to_list d =
  Prelude.foldl parse_line [] (lines d)


proc d = Weighted $ M.fromList (parse_input_to_list d)

get_sick content =
   last $ lines content


main = do
       d <- getContents
       let rs_graph = proc d
       let sickness_origin = get_sick d
       T.printf "%.2f" $ (\ x -> 1/x ) $ maximum $ fst $ dj rs_graph (Keyed sickness_origin)