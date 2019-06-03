module Main where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Prelude (Int, Integer, undefined)
import System.IO (IO, putStrLn)

type Point = (Int, Int)

type PersonId = Integer

data PersonData =
  PersonData
    { personId :: PersonId
    }

data IndustryData =
  IndustryData
    { industryWorkers :: Set PersonId
    , industryCapacity :: Int
    }

data StoreData =
  StoreData
    { storeCustomers :: Set PersonId
    , storeCapacity :: Int
    }

data HouseData =
  HouseData
    { houseInhabitants :: Set PersonId
    }

data GridPoint
  = House HouseData
  | Store StoreData
  | Industry IndustryData
  | Street
  | Nature

data Grid =
  Grid
    { gridSize :: Point
    , gridData :: Map Point GridPoint
    }

-- findEmptyHouses :: Grid -> [GridPoint]
-- findEmptyHouses = undefined
-- houseReaches ::
main :: IO ()
main = putStrLn "Hello, Haskell!"
