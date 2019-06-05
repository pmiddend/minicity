{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Minicity.Types where

import Brick.Types (Widget)
import Data.Bool (Bool(False, True))
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Int (Int)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Lens.Micro (Lens', (^.), at, non)
import Lens.Micro.GHC ()
import Lens.Micro.TH (makeLenses)
import Minicity.Point (Point, _x, _y)
import Prelude (Integer)
import Text.Show (Show)

type PersonId = Integer

data PersonData =
  PersonData
    { _personId :: PersonId
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

data StoreData =
  StoreData
    { _storeCustomers :: Set PersonId
    , _storeCapacity :: Int
    }
  deriving (Eq, Show)

makeLenses ''StoreData

data HouseData =
  HouseData
    { _houseInhabitants :: Set PersonId
    }
  deriving (Eq, Show)

makeLenses ''HouseData

data GridPoint
  = House (Maybe HouseData)
  | Store (Maybe StoreData)
  | Industry (Maybe IndustryData)
  | Street
  | Nature
  deriving (Eq, Show)

isStreet :: GridPoint -> Bool
isStreet Street = True
isStreet _ = False

isNature :: GridPoint -> Bool
isNature Nature = True
isNature _ = False

data Grid =
  Grid
    { _gridSize :: Point
    , _gridData :: Map Point GridPoint
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
    { _grid :: Grid
    , _gridSelected :: Point
    }

makeLenses ''PointedGrid

instance HasDimensions PointedGrid where
  dimensions = grid . gridSize

gridAtWithDefault :: Point -> Lens' Grid GridPoint
gridAtWithDefault p = gridData . at p . non Nature

type CityPeople = Map PersonId PersonData

data CityState =
  CityState
    { _cityGrid :: PointedGrid
    , _cityPeople :: CityPeople
    , _cityYear :: Int
    }

makeLenses ''CityState

citySelectedPoint :: Lens' CityState GridPoint
citySelectedPoint f s =
  (cityGrid . grid) (gridAtWithDefault (s ^. cityGrid . gridSelected) f) s

type CityUiName = ()

type CityUiEvent = ()

type CityWidget = Widget CityUiName
