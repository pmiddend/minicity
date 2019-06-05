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
  , getContext
  )
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core (str)
import Control.Applicative (pure)
import Control.Monad (void)
import Data.Char (Char)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (unlines)
import Data.Maybe (Maybe(..), listToMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord, max, min)
import Data.String (String)
import Graphics.Vty (defAttr)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KEsc))
import Lens.Micro ((%~), (&), (^.))
import Minicity.Point (Point(..))
import Minicity.Types
import Prelude (Int, (+), undefined)
import System.IO (IO)

emptyCityState :: Point -> CityState
emptyCityState gs =
  CityState
    { _cityGrid =
        PointedGrid
          { _grid = Grid {_gridSize = gs, _gridData = mempty}
          , _gridSelected = Point 0 0
          }
    , _cityPeople = mempty
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
   in unlines lines

toTuple :: Point -> (Int, Int)
toTuple (Point x y) = (x, y)

toLocation :: Point -> Location
toLocation = Location . toTuple

gridToWidget :: Grid -> Point -> CityWidget
gridToWidget g cursor =
  Widget Fixed Fixed $ do
    strRendering <- render (str (gridToString g))
    pure (strRendering {cursors = [CursorLocation (toLocation cursor) Nothing]})

cityDraw :: CityState -> [CityWidget]
cityDraw s =
  [ borderWithLabel
      (str "City")
      (gridToWidget (s ^. cityGrid . grid) (s ^. cityGrid . gridSelected))
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
   in s & cityGrid . gridSelected %~ (clampCursor . (+ p))

cityHandleEvent ::
     CityState
  -> BrickEvent CityUiName CityUiEvent
  -> EventM CityUiName (Next CityState)
cityHandleEvent s e =
  case e of
    VtyEvent (EvKey KEsc _) -> halt s
    VtyEvent (EvKey (KChar 'h') _) -> continue (moveCursor (Point (-1) 0) s)
    VtyEvent (EvKey (KChar 'l') _) -> continue (moveCursor (Point 1 0) s)
    VtyEvent (EvKey (KChar 'j') _) -> continue (moveCursor (Point 0 1) s)
    VtyEvent (EvKey (KChar 'k') _) -> continue (moveCursor (Point 0 (-1)) s)
    _ -> continue s

cityStartEvent :: CityState -> EventM CityUiName CityState
cityStartEvent = pure

cityAttrMap :: CityState -> AttrMap
cityAttrMap _ = attrMap defAttr mempty

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
      initialState = emptyCityState (Point 8 8)
  void (defaultMain app initialState)
