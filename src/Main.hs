{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Main (App(..), continue, defaultMain, halt)
import Brick.Markup (Markup, (@?), markup)
import Brick.Types
  ( BrickEvent(VtyEvent)
  , CursorLocation(..)
  , EventM
  , Location(..)
  , Next
  , Result(..)
  , Size(Fixed)
  , Widget(..)
  )
import Brick.Util (bg)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core ((<+>), (<=>), str, txt)
import Control.Applicative (pure)
import Control.Arrow ((>>>))
import Control.Lens
  ( (%%~)
  , (%~)
  , (&)
  , (+~)
  , (.=)
  , (.~)
  , (<>=)
  , (<>~)
  , (?~)
  , (^.)
  , (^..)
  , at
  , filtered
  , folded
  , hasn't
  , itoList
  , ix
  , traverse
  , use
  )
import Control.Monad (filterM, void, when)
import Control.Monad.State (State, runState)
import Data.Bool (Bool(False, True), (||), not)
import Data.Foldable (fold, foldMap, toList)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List ((!!), intersperse, length, null)
import qualified Data.List (filter, unlines)
import qualified Data.Map (fromList)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Monoid ((<>), mempty)
import Data.Ord (Ord, (<), (>=), max, min)
import Data.Set (Set, difference, filter, insert, singleton)
import qualified Data.Set (fromList)
import Data.String (String)
import Data.Text.Lazy (Text, toStrict, unlines)
import Data.Text.Markup (fromText)
import Data.Tuple (fst, snd)
import Graphics.Vty (defAttr, defAttr, dim, green, withStyle, yellow)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KEsc))
import Minicity.Params
import Minicity.Point (Point(..))
import Minicity.Types
import Prelude (Double, Int, (*), (+), (-), fromIntegral)
import System.IO (IO)
import System.Random (Random, RandomGen, mkStdGen)
import qualified System.Random (random, randomR)
import Text.Pretty.Simple (pShowNoColor)
import Text.Show (show)

attrHouse :: AttrName
attrHouse = attrName "house"

attrFull :: AttrName
attrFull = attrName "full"

attrIndustry :: AttrName
attrIndustry = attrName "industry"

attrNature :: AttrName
attrNature = attrName "nature"

gridPointToMarkup :: GridPoint -> Markup AttrName
gridPointToMarkup (House Nothing) = " " @? attrHouse
gridPointToMarkup (House _) = "H" @? attrHouse
gridPointToMarkup (Industry _) = "I" @? attrIndustry
gridPointToMarkup Street = fromText "#"
gridPointToMarkup Nature = "." @? attrNature

gridToMarkup :: Grid -> Markup AttrName
gridToMarkup g =
  let rows :: [Int]
      rows = [0 .. (g ^. height)]
      lines :: [Markup AttrName]
      lines =
        (\y ->
           foldMap
             (\x -> gridPointToMarkup (g ^. gridAtWithDefault (Point x y)))
             [0 .. (g ^. width)]) <$>
        rows
      interspersed :: [Markup AttrName]
      interspersed = intersperse (fromText "\n") lines
   in fold interspersed

toTuple :: Point -> (Int, Int)
toTuple (Point x y) = (x, y)

toLocation :: Point -> Location
toLocation = Location . toTuple

gridToWidget :: Grid -> Point -> CityWidget
gridToWidget g cursor =
  Widget Fixed Fixed $ do
    strRendering <- render (markup (gridToMarkup g))
    pure (strRendering {cursors = [CursorLocation (toLocation cursor) Nothing]})

peopleString :: [PersonData] -> Text
peopleString p = unlines (pShowNoColor <$> p)

cityDraw :: CityState -> [CityWidget]
cityDraw s =
  let cityWidget =
        borderWithLabel
          (str "City")
          (gridToWidget
             (s ^. cityGrid . pointedGrid)
             (s ^. cityGrid . pointedGridSelected))
      currentPoint :: Text
      currentPoint = pShowNoColor (s ^. citySelectedPoint)
      currentPointWidget =
        borderWithLabel (str "Cursor") (txt (toStrict currentPoint))
      peopleText = toStrict (peopleString (s ^.. cityStatePeople))
      personWidget =
        borderWithLabel
          (str "People")
          (if hasn't cityStatePeople s
             then str "No inhabitants"
             else txt peopleText)
      statusStr = "Year " <> show (s ^. cityYear)
      statusWidget = str statusStr
      logLineToStr (year, line) = "Year " <> show year <> ": " <> line
      logWidget =
        borderWithLabel
          (str "Log")
          (str (Data.List.unlines (logLineToStr <$> (s ^. cityLog))))
   in [ statusWidget <=>
        ((cityWidget <=> currentPointWidget) <+> (personWidget <=> logWidget))
      ]

cityChooseCursor ::
     CityState
  -> [CursorLocation CityUiName]
  -> Maybe (CursorLocation CityUiName)
cityChooseCursor _ = listToMaybe

clamp :: Ord a => a -> a -> a -> a
clamp a b x = max a (min b x)

clampPos :: Int -> Int -> Int
clampPos = clamp 0

moveCursor :: Point -> CityState -> CityState
moveCursor p s =
  let clampCursor :: Point -> Point
      clampCursor (Point x y) =
        Point
          (clampPos (s ^. cityGrid . width) x)
          (clampPos (s ^. cityGrid . height) y)
   in s & cityGrid . pointedGridSelected %~ (clampCursor . (+ p))

reachableFrom :: Grid -> Point -> [(Point, GridPoint)]
reachableFrom s = reachableFrom' mempty
  where
    gridWidth = s ^. width
    gridHeight = s ^. height
    neighbors :: Point -> Set Point
    neighbors (Point x y) =
      Data.Set.fromList
        [Point (x - 1) y, Point (x + 1) y, Point x (y + 1), Point x (y - 1)]
    neighborOob :: Point -> Bool
    neighborOob (Point x y) =
      x < 0 || x >= gridWidth || y < 0 || y >= gridHeight
    validNeighbors :: Point -> Set Point
    validNeighbors = filter (neighborOob >>> not) . neighbors
    resolve :: Point -> GridPoint
    resolve p = s ^. gridAtWithDefault p
    reachableFrom' :: Set Point -> Point -> [(Point, GridPoint)]
    reachableFrom' visited p =
      let nbsSet :: Set Point
          nbsSet = validNeighbors p `difference` visited
          nbs :: [(Point, GridPoint)]
          nbs = [(n, resolve n) | n <- toList nbsSet]
          streets :: Set Point
          streets =
            foldMap
              (fst >>> singleton)
              (Data.List.filter (snd >>> isStreet) nbs)
          rest :: [(Point, GridPoint)]
          rest = Data.List.filter (snd >>> isStreet >>> not) nbs
          newVisited = p `insert` visited
          recursion = foldMap (reachableFrom' newVisited) streets
       in rest <> recursion

place :: CityState -> GridPoint -> CityState
place s p = s & citySelectedPoint .~ p

cityHandleEvent ::
     CityState
  -> BrickEvent CityUiName CityUiEvent
  -> EventM CityUiName (Next CityState)
cityHandleEvent s e =
  case e of
    VtyEvent (EvKey KEsc _) -> halt s
    VtyEvent (EvKey (KChar 'H') _) -> continue (place s (House Nothing))
    VtyEvent (EvKey (KChar '#') _) -> continue (place s Street)
    VtyEvent (EvKey (KChar ' ') _) -> continue (runSimulation s)
    VtyEvent (EvKey (KChar 'I') _) ->
      continue (place s (Industry (IndustryData mempty paramsIndustryCapacity)))
    VtyEvent (EvKey (KChar 'h') _) -> continue (moveCursor (Point (-1) 0) s)
    VtyEvent (EvKey (KChar 'l') _) -> continue (moveCursor (Point 1 0) s)
    VtyEvent (EvKey (KChar 'j') _) -> continue (moveCursor (Point 0 1) s)
    VtyEvent (EvKey (KChar 'k') _) -> continue (moveCursor (Point 0 (-1)) s)
    _ -> continue s

cityStartEvent :: CityState -> EventM CityUiName CityState
cityStartEvent = pure

cityAttrMap :: CityState -> AttrMap
cityAttrMap _ =
  attrMap
    defAttr
    [ (attrHouse, bg green `withStyle` dim)
    , (attrIndustry, bg yellow)
    , (attrNature, defAttr `withStyle` dim)
    ]

personDeathProbability :: PersonData -> Double
personDeathProbability p =
  let x = fromIntegral (p ^. personAge)
   in 3.569812499e-5 * x * x - 5.54312499e-4 * x

type SimState a = State SimulationState a

inhabitantSurvives :: PersonData -> SimState Bool
inhabitantSurvives p = do
  let probability = personDeathProbability p
  rangeValue <- randomR (0.0, 1.0)
  when
    (rangeValue < probability)
    (log ((p ^. personName) <> " died at the age of " <> show (p ^. personAge)))
  pure (rangeValue >= probability)

killInhabitants' :: Maybe HouseData -> SimState (Maybe HouseData)
killInhabitants' Nothing = pure Nothing
killInhabitants' (Just h) = do
  newInhabitants <- filterM inhabitantSurvives (h ^. houseDataInhabitants)
  case newInhabitants of
    [] -> pure Nothing
    _ -> pure (Just (h & houseDataInhabitants .~ newInhabitants))

simulation :: Grid -> Year -> SimState Grid
simulation grid' (-1) = firstSimulation grid'
simulation grid' _ = do
  let grid'' = grid' & gridPeople . personAge +~ 1
  grid'' & gridData . traverse . _House %%~ killInhabitants'

isEmptyHouse :: GridPoint -> Bool
isEmptyHouse (House (Just hd)) = null (hd ^. houseDataInhabitants)
isEmptyHouse (House Nothing) = True
isEmptyHouse _ = False

findEmptyHouse :: Grid -> Maybe MovingInData
findEmptyHouse g =
  let gridPoints :: [(Point, GridPoint)]
      gridPoints = itoList (g ^. gridData)
      nonEmptyHouses :: [(Point, GridPoint)]
      nonEmptyHouses = gridPoints ^.. folded . filtered (isEmptyHouse . snd)
      withIndustry :: [MovingInData]
      withIndustry = do
        (p, gp) <- nonEmptyHouses
        (ip, igp) <- reachableFrom g p
        case igp of
          Industry ind ->
            if industryHasCapacity ind
              then [MovingInData p gp ip]
              else mempty
          _ -> mempty
   in listToMaybe withIndustry

log :: String -> SimState ()
log s = simStateLog <>= [s]

random :: Random a => SimState a
random = do
  curGen <- use simStateRng
  let (v, newGen) = System.Random.random curGen
  simStateRng .= newGen
  pure v

randomR :: Random a => (a, a) -> SimState a
randomR r = do
  curGen <- use simStateRng
  let (v, newGen) = System.Random.randomR r curGen
  simStateRng .= newGen
  pure v

randomElement' :: RandomGen g => [a] -> g -> (a, g)
randomElement' l gen =
  let (v, gen') = System.Random.randomR (0, length l - 1) gen
   in (l !! v, gen')

randomElement :: [a] -> SimState a
randomElement l = do
  index <- randomR (0, length l)
  pure (l !! index)

randomPerson :: SimState PersonData
randomPerson = do
  pid <- random
  firstName <- randomElement randomFirstNames
  lastName <- randomElement randomLastNames
  age <- randomR (paramsMinAge, paramsMaxAge)
  pure (PersonData pid (firstName <> " " <> lastName) age)

firstSimulation :: Grid -> SimState Grid
firstSimulation grid' = firstSimulation' grid' paramsFirstWave
  where
    firstSimulation' :: Grid -> Int -> SimState Grid
    firstSimulation' grid'' 0 = pure grid''
    firstSimulation' grid'' peopleLeft =
      case findEmptyHouse grid'' of
        Nothing -> do
          log (show peopleLeft <> " people left on first day!")
          pure grid''
        Just mid -> do
          newPerson <- randomPerson
          let newHouse = House (Just (HouseData [newPerson]))
              newGrid :: Grid
              newGrid =
                grid'' & gridData . at (mid ^. movingInCoord) ?~ newHouse &
                gridData .
                ix (mid ^. movingInIndustry) .
                _Industry .
                industryWorkers <>~
                singleton (newPerson ^. personId)
          firstSimulation' newGrid (peopleLeft - 1)

runSimulation :: CityState -> CityState
runSimulation s =
  let simState =
        SimulationState {_simStateRng = s ^. cityRng, _simStateLog = mempty}
      (newGrid, newSimState) =
        runState
          (simulation (s ^. cityGrid . pointedGrid) (s ^. cityYear))
          simState
   in s & cityGrid . pointedGrid .~ newGrid & cityYear +~ 1 &
      (cityLog <>~ ((s ^. cityYear, ) <$> (newSimState ^. simStateLog))) &
      cityRng .~
      (newSimState ^. simStateRng)

emptyCityState :: Point -> CityState
emptyCityState gs =
  CityState
    { _cityGrid =
        PointedGrid
          { _pointedGrid = Grid {_gridSize = gs, _gridData = mempty}
          , _pointedGridSelected = Point 0 0
          }
    , _cityYear = -1
    , _cityLog = mempty
    , _cityRng = mkStdGen 0
    }

sampleCity :: CityState
sampleCity =
  CityState
    { _cityGrid =
        PointedGrid
          { _pointedGrid =
              Grid
                { _gridSize = Point 8 8
                , _gridData =
                    Data.Map.fromList
                      [ (Point 1 1, House Nothing)
                      , (Point 2 1, Street)
                      , (Point 3 1, Street)
                      , ( Point 4 1
                        , Industry (IndustryData mempty paramsIndustryCapacity))
                      ]
                }
          , _pointedGridSelected = Point 0 0
          }
    , _cityYear = -1
    , _cityLog = [(-1, "MiniCity started")]
    , _cityRng = mkStdGen 0
    }

main :: IO ()
main = do
  let app =
        App
          { appDraw = cityDraw
          , appChooseCursor = cityChooseCursor
          , appHandleEvent = cityHandleEvent
          , appStartEvent = cityStartEvent
          , appAttrMap = cityAttrMap
          }
      initialState = sampleCity
  void (defaultMain app initialState)
