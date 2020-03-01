module DevMain where

import ClassyPrelude

import Data.Functor.Foldable (hylo)

run :: IO ()
run =
  putStrLn $ tshow $ qsort ([1,8,2,9] :: [Int])



data TreeF a r = Leaf | Node a r r
  deriving Functor

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
