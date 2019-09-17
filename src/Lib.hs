module Lib where

import qualified Data.Map.Strict as M
import Control.Arrow ((>>>))
import Data.Monoid (mconcat)

newtype PQ a =  PQ {getPQ :: M.Map a Int}

enqueuePQ :: Ord a => a -> PQ a -> PQ a
enqueuePQ a = PQ . M.alter (Just . maybe 1 (+1)) a . getPQ

dequeuePQ :: Ord a => PQ a -> (Maybe a, PQ a)
dequeuePQ (PQ pq) = 
    if pq == M.empty then (Nothing, PQ M.empty)
    else
        let
            maxVal = fst $ M.findMax pq
            newPQ = M.alter (>>= (subtract  1 >>> \n -> if n > 0 then Just n else Nothing)) maxVal pq
        in
            (Just maxVal, PQ newPQ)

modifyFirst :: Ord a => (a -> a) -> PQ a -> PQ a
modifyFirst f pq = let
    (maxVal, newPQ) = dequeuePQ pq
    in
        case maxVal of 
            Just n -> enqueuePQ (f n) newPQ
            Nothing -> pq

fromListPQ :: Ord a => [a] -> PQ a
fromListPQ = foldr enqueuePQ emptyPQ

emptyPQ :: Ord a => PQ a
emptyPQ = PQ M.empty


toListPQ :: Ord a => PQ a -> [a]
toListPQ pq = let
    (mayVal, newPQ) = dequeuePQ pq
    in
        case mayVal of
            Nothing -> []
            Just v -> v : toListPQ newPQ
