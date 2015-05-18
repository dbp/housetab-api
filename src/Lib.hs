{-# LANGUAGE TupleSections #-}
module Lib where

import           GHC.Generics

import qualified Data.ByteString       as BS
import           Data.List             (groupBy, notElem, sort)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Entry.Types           (Entry, Entry' (..))
import           Person.Types          (Person, Person' (..), Share,
                                        Share' (..))

import           Test.QuickCheck       (Arbitrary (..), arbitrary, choose,
                                        elements, listOf, listOf1, quickCheck)
import           Text.Printf           (printf)

import           Debug.Trace

import qualified Data.ByteString.Char8 as B8

type Spent = Double
type Owes = Double

data Result = Result {people      :: [(Person, Spent, Owes)]
                     ,currentdate :: Date}
                     deriving (Show, Eq, Generic)

type Date = (Integer, Int, Int)

data Purchase = Purchase { purchaser :: (Person, [Share])
                         , date      :: Date
                         , ammount   :: Double
                         , payers    :: [(Person, [Share])]}
              deriving (Show, Eq)

instance Ord Purchase where
    compare (Purchase p1 d1 _ _) (Purchase p2 d2 _ _) =
        if compare d1 d2 == EQ then compare p1 p2 else compare d1 d2

getPercentage :: (Person, [Share]) -> Date -> Double
getPercentage (_, shares) date = fn shares
    where
      fn ((Share _ _ d p):[]) = p
      fn ((Share _ _ d p):(Share i person d' p'):xs) = if (toGregorian (utctDay d')) > date
                        then p
                        else fn $ (Share i person d' p'):xs
      fn [] = 0 -- if they have nothing assigned, assume no share.

getPerson :: [(Person, [Share])] -> Int -> (Person, [Share])
getPerson ps id' = head $ filter (\(p, _) -> (personId p) == id') ps

purchasify :: [(Person, [Share])] -> [Entry] -> [Purchase]
purchasify people ((Entry _ _ who _ _ when howmuch whopays):xs) =
    (Purchase (getPerson people who)
              (toGregorian (utctDay when))
              howmuch
              (map (getPerson people) whopays)) : (purchasify people xs)
purchasify _ [] = []

doTheSplit :: Purchase -> [((Person, [Share]), Double)]
doTheSplit purchase = map (\p -> (p, (ammount purchase) * (gp p) / (if unity == 0 then 1 else unity))) (payers purchase)
    where unity = sum (map gp (payers purchase))
          gp p = getPercentage p (date purchase)

concatJoin :: [[((Person, [Share]), Double)]] -> [((Person, [Share]), Double)]
concatJoin pds = map (\grp -> ((fst (head grp)), sum (map snd grp))) $
                 groupBy (\a b -> (fst a) == (fst b)) $ sort $ concat pds

spent :: [Purchase] -> [((Person, [Share]), Double)]
spent purchases = concatJoin $ [map (\p -> (purchaser p, ammount p)) purchases]

processPurchases :: [Purchase] -> Result
processPurchases [] = Result [] (1000, 1, 1)
processPurchases purchases = Result (map (\(a, b, c) -> (fst a, b, c)) (map (pickSpent purchases) $ concatJoin $ map doTheSplit purchases)) (date (head (reverse (sort purchases))))

pickSpent purchases (p, a) = (p, spent {- - paybacks-}, a - spent)
    where count ps = sum $ map ammount $ ps
          spent = count $ filter (\pur -> purchaser pur == p) purchases
          -- this is because the total spent looks strange when you don't factor these in.
          -- a payback is classified by one person directly paying another (ie, the first
          -- is the spender, the receiver is only splitter)
          paybacks = count $ filter (\pur -> (length (payers pur) == 1) && (head (payers pur) == p) && (purchaser pur /= p)) purchases


addMissing :: [Purchase] -> [(Person, [Share])] -> Result -> Result
addMissing ps house (Result personResult date) =
  Result (personResult ++
          (map (\(a,b,c) -> (fst a, b, c))
               (map (\p -> pickSpent ps (p,0)) missing))) date
    where missing :: [(Person, [Share])]
          missing = filter (\(p,_) -> notElem p (map (\(p,_,_) -> p) personResult)) house

run :: [(Person, [Share])] -> [Entry] -> Result
run house s = addMissing purchases house $ processPurchases purchases
  where purchases = purchasify house s
