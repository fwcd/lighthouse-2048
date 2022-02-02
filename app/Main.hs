{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (guard, void, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Lighthouse.Connection
import Lighthouse.Display
import Lighthouse.Options
import Lighthouse.Protocol (InputEvent (..), Input (..))
import Lighthouse.Utils.Color
import Lighthouse.Utils.Logging
import System.Environment (getEnv)
import System.Random.Stateful (uniformRM, globalStdGen)

-- TODO: Use Data.Vector for efficient random access?

newtype Tile = Tile { tileValue :: Int }
  deriving (Show, Eq)

newtype Board = Board { boardRows :: [[Maybe Tile]] }
  deriving (Show, Eq)

data Dir = DirLeft | DirRight | DirUp | DirDown
  deriving (Show, Eq)

data Pos = Pos { posX :: Int, posY :: Int }
  deriving (Show, Eq)

-- | The (constant) board width.
boardWidth :: Int
boardWidth = 4

-- | The (constant) board height.
boardHeight :: Int
boardHeight = 4

-- | The empty board.
emptyBoard :: Board
emptyBoard = Board $ replicate boardHeight (replicate boardWidth Nothing)

-- | Checks whether the given position is in bounds.
inBounds :: Pos -> Bool
inBounds (Pos x y) = x >= 0 && x < boardWidth && y >= 0 && y < boardHeight

-- | All valid positions.
positions :: [Pos]
positions = [Pos x y | y <- [0..(boardHeight - 1)], x <- [0..(boardWidth - 1)]]

-- | Fetches the empty positions on the board.
emptyPositions :: Board -> [Pos]
emptyPositions b = filter (isNothing . flip tileAt b) positions

-- | Fetches the tile at the given position.
tileAt :: Pos -> Board -> Maybe Tile
tileAt pos@(Pos x y) (Board rs) | inBounds pos = (rs !! y) !! x
                                | otherwise    = Nothing

-- | Generates a board from a function that maps positions to tiles.
generateBoard :: (Pos -> Maybe Tile) -> Board
generateBoard gen = Board $ (\y -> (\x -> gen (Pos x y)) <$> [0..(boardWidth - 1)]) <$> [0..(boardHeight - 1)]

-- | Places the tile at the given position.
putTileAt :: Pos -> Maybe Tile -> Board -> Board
putTileAt p t b = generateBoard $ \p' -> if p == p' then t else tileAt p' b

-- TODO: Add optimization rule transpose . transpose = id?

-- | Transposes the board.
transpose :: Board -> Board
transpose b = generateBoard $ \(Pos x y) -> tileAt (Pos y x) b

-- | Flips the board vertically.
flipV :: Board -> Board
flipV (Board rs) = Board $ reverse rs

-- | Flips the board horizontally.
flipH :: Board -> Board
flipH = transpose . flipV . transpose

-- | Rotates the board left by 90 degrees.
rotL :: Board -> Board
rotL = flipV . transpose

-- | Rotates the board right by 90 degrees.
rotR :: Board -> Board
rotR = flipH . transpose

-- | Shifts and merges tiles in the given direction.
shiftAndMerge :: Dir -> Board -> Board
shiftAndMerge dir = case dir of
    DirLeft  -> stepLeft
    DirRight -> stepRight
    DirUp    -> rotR . stepLeft . rotL
    DirDown  -> rotR . stepRight . rotL

  where stepLeft (Board rs) = Board $ updateRow [] <$> rs
        stepRight           = flipH . stepLeft . flipH

        -- Update row by merging adjacent equal tiles and shifting the tiles to the left
        updateRow acc ts = case ts of
          []                               -> acc
          Just t : Just t' : ts' | t == t' -> Just (Tile (2 * tileValue t)) : updateRow (Nothing : acc) ts'
          Just t : ts'                     -> Just t : updateRow acc ts'
          Nothing : ts'                    -> updateRow (Nothing : acc) ts'

-- | Randomly chooses a value from the given list (assuming it is non-empty).
chooseRandom :: MonadIO m => [a] -> m a
chooseRandom [] = error "Cannot choose random from empty list!"
chooseRandom xs = do
  i <- uniformRM (0, length xs - 1) globalStdGen
  return $ xs !! i

-- | Spawns a new tile at a random position.
spawnTile :: MonadIO m => Board -> m Board
spawnTile b = do
  t <- chooseRandom (Tile 4 : replicate 3 (Tile 2))
  p <- chooseRandom (emptyPositions b)
  return $ putTileAt p (Just t) b

-- | Performs a game step in the given direction.
step :: MonadIO m => Dir -> Board -> m Board
step dir = spawnTile . shiftAndMerge dir

-- | Maps a tile to a color for the lighthouse.
tileColor :: Tile -> Color
tileColor (Tile 2)  = white
tileColor (Tile 4)  = Color 255 231 150 -- light yellow
tileColor (Tile 8)  = Color 255 144 25  -- orange
tileColor (Tile 16) = Color 189 88 0    -- darker orange
tileColor (Tile 32) = Color 255 88 66   -- light red
tileColor (Tile 64) = Color 171 20 0    -- dark red
tileColor (Tile _)  = yellow

-- | Converts a game board to a lighthouse-sized display.
boardToDisplay :: Board -> Display
boardToDisplay b = generateDisplay pixAt
  where pixAt x y = maybe black tileColor $ tileAt (Pos ((x - 4) `div` 5) ((y - 1) `div` 3)) b

-- | The lighthouse application, i.e. our game.
app :: Listener Board
app = mempty
  { onConnect = do
      logInfo "app" "Connected!"

      -- Send initial board
      b  <- getUserState
      b' <- spawnTile b
      putUserState b'
      sendDisplay $ boardToDisplay b'

      -- Request input events
      requestStream

  , onInput = \e -> void . runMaybeT $ do
      -- Handle only key down events
      guard (keIsDown e)

      -- Convert key input to direction
      dir <- MaybeT . return $ case keInput e of
        KeyInput 37 -> Just DirLeft
        KeyInput 38 -> Just DirUp
        KeyInput 39 -> Just DirRight
        KeyInput 40 -> Just DirDown
        _           -> Nothing

      -- Update board and send it
      lift $ do
        b  <- getUserState
        b' <- step dir b
        putUserState b'
        sendDisplay $ boardToDisplay b'
  }

-- | The main function.
main :: IO ()
main = do
  username <- T.pack <$> getEnv "LIGHTHOUSE_USERNAME"
  token    <- T.pack <$> getEnv "LIGHTHOUSE_TOKEN"
  let opts  = Options { optAuthentication = Authentication { authUsername = username, authToken = token }
                      , optLogHandler = simpleLogHandler infoLevel
                      , optInitialState = emptyBoard
                      }

  runLighthouseApp app opts
