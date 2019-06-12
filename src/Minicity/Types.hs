{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Minicity.Types where

import Brick.Types (Widget)
import Control.Lens
  ( Lens'
  , Traversal'
  , (^.)
  , _Just
  , at
  , has
  , makeLenses
  , makePrisms
  , non
  , traverse
  )
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Int (Int)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.String (String)
import Minicity.Point (Point, _x, _y)
import Prelude (Integer)
import System.Random (StdGen)
import Text.Show (Show)

type PersonId = Integer

data PersonData =
  PersonData
    { _personId :: PersonId
    , _personName :: String
    , _personAge :: Int
    }
  deriving (Eq, Show)

makeLenses ''PersonData

data IndustryData =
  IndustryData
    { _industryWorkers :: Set PersonId
    , _industryCapacity :: Int
    }
  deriving (Eq, Show)

makeLenses ''IndustryData

data HouseData =
  HouseData
    { _houseDataInhabitants :: [PersonData]
    }
  deriving (Eq, Show)

makeLenses ''HouseData

data GridPoint
  = House (Maybe HouseData)
  | Industry (Maybe IndustryData)
  | Street
  | Nature
  deriving (Eq, Show)

makePrisms ''GridPoint

isStreet :: GridPoint -> Bool
isStreet = has _Street

isNature :: GridPoint -> Bool
isNature = has _Nature

houseInhabitants :: Traversal' GridPoint PersonData
houseInhabitants = _House . _Just . houseDataInhabitants . traverse

type GridMap = Map Point GridPoint

data Grid =
  Grid
    { _gridSize :: Point
    , _gridData :: GridMap
    }

makeLenses ''Grid

class HasDimensions a where
  dimensions :: Lens' a Point
  width :: Lens' a Int
  width = dimensions . _x
  height :: Lens' a Int
  height = dimensions . _y

instance HasDimensions Grid where
  dimensions = gridSize

data PointedGrid =
  PointedGrid
    { _pointedGrid :: Grid
    , _pointedGridSelected :: Point
    }

makeLenses ''PointedGrid

instance HasDimensions PointedGrid where
  dimensions = pointedGrid . gridSize

gridAtWithDefault :: Point -> Lens' Grid GridPoint
gridAtWithDefault p = gridData . at p . non Nature

type Year = Int

data CityState =
  CityState
    { _cityGrid :: PointedGrid
    , _cityYear :: Year
    , _cityLog :: [(Year, String)]
    , _cityRng :: StdGen
    }

makeLenses ''CityState

citySelectedPoint :: Lens' CityState GridPoint
citySelectedPoint f s =
  (cityGrid . pointedGrid)
    (gridAtWithDefault (s ^. cityGrid . pointedGridSelected) f)
    s

-- cityPeople :: Getter CityState [PersonData]
-- cityPeople =
--   to (\s -> s ^.. cityGrid . grid . gridData . folded . houseInhabitants)
cityPeople :: Traversal' CityState PersonData
cityPeople = cityGrid . pointedGrid . gridData . traverse . houseInhabitants

type CityUiName = ()

type CityUiEvent = ()

type CityWidget = Widget CityUiName

data SimulationState =
  SimulationState
    { _simStateRng :: StdGen
    , _simStateLog :: [String]
    }

makeLenses ''SimulationState
