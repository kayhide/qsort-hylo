module DevMain where

import ClassyPrelude

import Data.List (cycle)
import Data.Functor.Foldable (hylo)

run :: IO ()
run = do
  let xs = [3,2,9,8] :: [Int]

  sayShow $ xs
  sayShow $ split' xs
  sayShow $ (fmap split' . split' $ xs)
  sayShow $ (fmap (fmap split') . fmap split' . split' $ xs)
  sayShow $ fmap (fmap (fmap split')) . fmap (fmap split') . fmap split' . split' $ xs
  sayShow $ fmap (fmap (fmap join')) . fmap (fmap (fmap split')) . fmap (fmap split') . fmap split' . split' $ xs
  sayShow $ fmap (fmap join') . fmap (fmap (fmap join')) . fmap (fmap (fmap split')) . fmap (fmap split') . fmap split' . split' $ xs
  sayShow $ fmap join' . fmap (fmap join') . fmap (fmap (fmap join')) . fmap (fmap (fmap split')) . fmap (fmap split') . fmap split' . split' $ xs
  sayShow $ join' . fmap join' . fmap (fmap join') . fmap (fmap (fmap join')) . fmap (fmap (fmap split')) . fmap (fmap split') . fmap split' . split' $ xs

  say ""
  sayShow $ (join' . fmap id . split') xs
  sayShow $ (join' . fmap (join' . fmap id . split') . split') xs
  sayShow $ (join' . fmap (join' . fmap (join' . fmap id . split') . split') . split') xs

  say ""
  let h = join' . fmap h . split'
  sayShow $ h xs

  say ""
  sayShow $ qsort xs



data TreeF a r = Leaf | Node a r r
  deriving (Show, Functor)

split' :: Ord a => [a] -> TreeF a [a]
split' = \case
  [] -> Leaf
  x : xs -> Node x l r
    where
      (l, r) = partition (< x) xs

join' :: TreeF a [a] -> [a]
join' = \case
  Leaf       -> []
  Node x l r -> l <> [x] <> r

qsort :: Ord a => [a] -> [a]
qsort = hylo join' split'
