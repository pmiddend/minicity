module Main where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main (App(..), continue, defaultMain, halt)
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
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core ((<+>), (<=>), str, txt)
import Control.Applicative (pure)
import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Bool (Bool, (||), not)
import Data.Char (Char)
import Data.Foldable (foldMap, toList)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.List (filter, unlines)
import Data.Map (elems)
import qualified Data.Map (fromList)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Monoid ((<>), mempty)
import Data.Ord (Ord, (<), (>=), max, min)
import Data.Set (Set, difference, filter, insert, singleton)
import qualified Data.Set (fromList)
import Data.String (String)
import Data.Text.Lazy (Text, toStrict, unlines)
import Data.Tuple (fst, snd)
import Graphics.Vty (defAttr)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KEsc))
import Lens.Micro ((%~), (&), (.~), (^.))
import Minicity.Point (Point(..))
import Minicity.Types
import Prelude (Int, (+), (-))
import System.IO (IO)
import Text.Pretty.Simple (pShow)
import Text.Show (show)

emptyCityState :: Point -> CityState
emptyCityState gs =
  CityState
    { _cityGrid =
        PointedGrid
          { _grid = Grid {_gridSize = gs, _gridData = mempty}
          , _gridSelected = Point 0 0
          }
    , _cityPeople = mempty
    , _cityYear = 0
    }

gridPointToChar :: GridPoint -> Char
gridPointToChar (House _) = 'H'
gridPointToChar (Store _) = 'S'
gridPointToChar (Industry _) = 'I'
gridPointToChar Street = '#'
gridPointToChar Nature = '.'

gridToString :: Grid -> String
gridToString g =
  let rows = [0 .. (g ^. height)]
      lines =
        (\y ->
           (\x -> gridPointToChar (g ^. gridAtWithDefault (Point x y))) <$>
           [0 .. (g ^. width)]) <$>
        rows
   in Data.List.unlines lines

toTuple :: Point -> (Int, Int)
toTuple (Point x y) = (x, y)

toLocation :: Point -> Location
toLocation = Location . toTuple

gridToWidget :: Grid -> Point -> CityWidget
gridToWidget g cursor =
  Widget Fixed Fixed $ do
    strRendering <- render (str (gridToString g))
    pure (strRendering {cursors = [CursorLocation (toLocation cursor) Nothing]})

peopleString :: CityPeople -> Text
peopleString p = unlines (pShow <$> elems p)

cityDraw :: CityState -> [CityWidget]
cityDraw s =
  let cityWidget =
        borderWithLabel
          (str "City")
          (gridToWidget (s ^. cityGrid . grid) (s ^. cityGrid . gridSelected))
      currentPoint :: Text
      currentPoint = pShow (s ^. citySelectedPoint)
      currentPointWidget =
        borderWithLabel (str "Cursor") (txt (toStrict currentPoint))
      personWidget =
        borderWithLabel
          (str "People")
          (txt (toStrict (peopleString (s ^. cityPeople))))
      statusStr = "Year " <> show (s ^. cityYear)
      statusWidget = str statusStr
   in [statusWidget <=> ((cityWidget <=> currentPointWidget) <+> personWidget)]

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
   in s & cityGrid . gridSelected %~ (clampCursor . (+ p))

reachableFrom :: CityState -> Point -> [(Point, GridPoint)]
reachableFrom s = reachableFrom' mempty
  where
    gridWidth = s ^. cityGrid . width
    gridHeight = s ^. cityGrid . height
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
    resolve p = s ^. cityGrid . grid . gridAtWithDefault p
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
    VtyEvent (EvKey (KChar 'S') _) -> continue (place s (Store Nothing))
    VtyEvent (EvKey (KChar 'I') _) -> continue (place s (Industry Nothing))
    VtyEvent (EvKey (KChar 'h') _) -> continue (moveCursor (Point (-1) 0) s)
    VtyEvent (EvKey (KChar 'l') _) -> continue (moveCursor (Point 1 0) s)
    VtyEvent (EvKey (KChar 'j') _) -> continue (moveCursor (Point 0 1) s)
    VtyEvent (EvKey (KChar 'k') _) -> continue (moveCursor (Point 0 (-1)) s)
    _ -> continue s

cityStartEvent :: CityState -> EventM CityUiName CityState
cityStartEvent = pure

cityAttrMap :: CityState -> AttrMap
cityAttrMap _ = attrMap defAttr mempty

sampleCity :: CityState
sampleCity =
  CityState
    { _cityGrid =
        PointedGrid
          { _grid =
              Grid
                { _gridSize = Point 8 8
                , _gridData =
                    Data.Map.fromList
                      [ (Point 1 1, House Nothing)
                      , (Point 2 1, Street)
                      , (Point 3 1, Street)
                      , (Point 4 1, Store Nothing)
                      ]
                }
          , _gridSelected = Point 0 0
          }
    , _cityPeople = mempty
    , _cityYear = 0
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
      --initialState = emptyCityState (Point 8 8)
      initialState = sampleCity
  void (defaultMain app initialState)
